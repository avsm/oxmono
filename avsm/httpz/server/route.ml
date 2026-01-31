(* route.ml - Zero-copy HTTP routing for httpz

   Design:
   - No boxed request type - handlers receive spans directly
   - Pattern matching uses spans throughout
   - String capture only when route matches (GADT constraint)
   - Exception for no-match (unboxed hot path)
*)

open Base
open Httpz

module I16 = Stdlib_stable.Int16_u

let[@inline] i16 x = I16.of_int x

(** {1 Response}

    Responses support both immediate bodies and streaming. *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }

(** {1 Response Headers}

    Typed response headers using Header_name.t for type safety. *)

type resp_header = Header_name.t * string
(** A response header: typed name and string value. *)

(** {1 Response Writer}

    Handlers receive a local [respond] function that directly writes the response.
    No intermediate record allocation - fully CPS with stack-allocated closures and headers. *)

type respond = status:Res.status -> headers:local_ resp_header list -> body -> unit
(** Response writer function. Headers list is local for stack allocation. *)

(** {2 Response helpers}

    Convenience functions that call respond with common patterns.
    All take local respond function and use local typed headers. *)

let[@inline] html (local_ respond) s =
  respond ~status:Res.Success ~headers:[(Header_name.Content_type, "text/html; charset=utf-8")] (String s)

let[@inline] json (local_ respond) s =
  respond ~status:Res.Success ~headers:[(Header_name.Content_type, "application/json; charset=utf-8")] (String s)

let[@inline] xml (local_ respond) s =
  respond ~status:Res.Success ~headers:[(Header_name.Content_type, "application/xml")] (String s)

let[@inline] atom (local_ respond) s =
  respond ~status:Res.Success ~headers:[(Header_name.Content_type, "application/atom+xml; charset=utf-8")] (String s)

let[@inline] plain (local_ respond) s =
  respond ~status:Res.Success ~headers:[(Header_name.Content_type, "text/plain")] (String s)

let[@inline] redirect (local_ respond) ~status ~location =
  respond ~status ~headers:[(Header_name.Location, location)] Empty

let[@inline] not_found (local_ respond) =
  respond ~status:Res.Not_found ~headers:[] (String "Not Found")

let[@inline] respond_string (local_ respond) ~status ?(local_ headers = []) s =
  respond ~status ~headers (String s)

let[@inline] stream (local_ respond) ~status ?(local_ headers = []) ?length iter =
  respond ~status ~headers (Stream { length; iter })

let[@inline] respond_bigstring (local_ respond) ~status ?(local_ headers = []) buf ~off ~len =
  respond ~status ~headers (Bigstring { buf; off; len })

(** {1 Context}

    Minimal context passed to handlers for path/query access. *)

type ctx = {
  buf : bytes;
  path : Span.t;
  query : Span.t;
}

(** Get path as string. *)
let[@inline] path ctx = Span.to_string ctx.buf ctx.path

(** Find query parameter as string. *)
let query_param ctx name =
  let #(found, span) = Target.find_query_param ctx.buf ctx.query name in
  if found then Some (Span.to_string ctx.buf span) else None

(** Find all query parameters with given name. *)
let query_params ctx name =
  Target.fold_query_params ctx.buf ctx.query ~init:[] ~f:(fun acc key value ->
      if Span.equal ctx.buf key name
      then Span.to_string ctx.buf value :: acc
      else acc)
  |> List.rev

(** Get all query parameters as pairs. *)
let query ctx = Target.query_to_string_pairs ctx.buf ctx.query

(** Generate cache key from path and query. *)
let cache_key ctx =
  let path_str = path ctx in
  if Span.is_empty ctx.query then path_str
  else
    let pairs = Target.query_to_string_pairs ctx.buf ctx.query in
    let sorted = List.sort pairs ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) in
    let qs = String.concat ~sep:"&" (List.map sorted ~f:(fun (k, v) -> k ^ "=" ^ v)) in
    path_str ^ "?" ^ qs

(** {1 Header Requirements}

    GADT for declaring which headers a route needs.
    Headers are extracted during dispatch, not looked up lazily. *)

type _ hdr =
  | H0 : unit hdr
  | H : Header_name.t * 'a hdr -> (string option * 'a) hdr

let h0 = H0
let[@inline] ( +> ) name rest = H (name, rest)

(** Find header value by name in local list. *)
let rec find_header_value (buf : bytes) (local_ headers : Header.t list) name : string option =
  match headers with
  | [] -> None
  | h :: rest ->
      if Poly.equal h.name name
      then Some (Span.to_string buf h.value)
      else find_header_value buf rest name

(** Extract required headers from local header list. Only extracts what's needed. *)
let rec extract_headers : type h. bytes -> local_ Header.t list -> h hdr -> h =
  fun buf headers spec ->
    match spec with
    | H0 -> ()
    | H (name, rest) ->
        let value = find_header_value buf headers name in
        (value, extract_headers buf headers rest)

(** {1 Pattern GADT}

    Type-safe path patterns. Captures are strings (GADT value layout constraint). *)

type _ pat =
  | Root : unit pat
  | Lit : string -> unit pat
  | Lits : string list -> unit pat
  | Seg : string pat
  | Tail : string pat
  | Seq : 'a pat * 'b pat -> ('a * 'b) pat
  | LitThen : string * 'a pat -> 'a pat  (** Literal then pattern, discards unit *)
  | ThenLit : 'a pat * string -> 'a pat  (** Pattern then literal, discards unit *)

let root = Root
let[@inline] lit s = Lit s
let seg = Seg
let tail = Tail
let[@inline] ( ** ) p1 p2 = Seq (p1, p2)
let[@inline] ( **> ) prefix capture = LitThen (prefix, capture)
let[@inline] ( <** ) pattern suffix = ThenLit (pattern, suffix)

(** {2 Pattern Matching} *)

exception No_match

(** Match result: captures and remaining path offset/length. *)
type 'a match_result = { cap : 'a; rest_off : int; rest_len : int }

let rec match_lits (buf : bytes) segs path =
  match segs with
  | [] -> { cap = (); rest_off = Span.off path; rest_len = Span.len path }
  | s :: rest ->
      let #(ok, remaining) = Target.match_segment buf path s in
      if ok then match_lits buf rest remaining else raise No_match

let rec match_pat : type a. bytes -> a pat -> Span.t -> a match_result =
  fun buf pat path ->
    match pat with
    | Root ->
        if Target.is_empty path then { cap = (); rest_off = 0; rest_len = 0 }
        else raise No_match
    | Lit s ->
        let #(ok, rest) = Target.match_segment buf path s in
        if ok then { cap = (); rest_off = Span.off rest; rest_len = Span.len rest }
        else raise No_match
    | Lits segs ->
        match_lits buf segs path
    | Seg ->
        let #(ok, seg, rest) = Target.match_param buf path in
        if ok then { cap = Span.to_string buf seg; rest_off = Span.off rest; rest_len = Span.len rest }
        else raise No_match
    | Tail ->
        { cap = Span.to_string buf path; rest_off = 0; rest_len = 0 }
    | LitThen (s, p) ->
        let #(ok, rest) = Target.match_segment buf path s in
        if ok then match_pat buf p rest
        else raise No_match
    | ThenLit (p, s) ->
        let r = match_pat buf p path in
        let rest = Span.make ~off:(i16 r.rest_off) ~len:(i16 r.rest_len) in
        let #(ok, rest') = Target.match_segment buf rest s in
        if ok then { cap = r.cap; rest_off = Span.off rest'; rest_len = Span.len rest' }
        else raise No_match
    | Seq (p1, p2) ->
        let r1 = match_pat buf p1 path in
        let rest1 = Span.make ~off:(i16 r1.rest_off) ~len:(i16 r1.rest_len) in
        let r2 = match_pat buf p2 rest1 in
        { cap = (r1.cap, r2.cap); rest_off = r2.rest_off; rest_len = r2.rest_len }

let[@inline] match_complete r = r.rest_len = 0

(** {1 Routes}

    Handlers receive captures, headers, context, and a respond function.
    The respond function writes the response directly - no intermediate record. *)

type ('a, 'h) handler = 'a -> 'h -> ctx -> respond -> unit

type route =
  | Pat : { meth : Method.t; pat : 'a pat; hdr : 'h hdr; handler : ('a, 'h) handler } -> route

(** Routes without headers (most common case). *)
let[@inline] get pat handler =
  Pat { meth = Method.Get; pat; hdr = H0; handler = fun a () ctx respond -> handler a ctx respond }
let[@inline] post pat handler =
  Pat { meth = Method.Post; pat; hdr = H0; handler = fun a () ctx respond -> handler a ctx respond }
let[@inline] put pat handler =
  Pat { meth = Method.Put; pat; hdr = H0; handler = fun a () ctx respond -> handler a ctx respond }
let[@inline] delete pat handler =
  Pat { meth = Method.Delete; pat; hdr = H0; handler = fun a () ctx respond -> handler a ctx respond }

(** Routes with explicit header requirements. *)
let[@inline] get_h pat hdr handler = Pat { meth = Method.Get; pat; hdr; handler }
let[@inline] post_h pat hdr handler = Pat { meth = Method.Post; pat; hdr; handler }
let[@inline] put_h pat hdr handler = Pat { meth = Method.Put; pat; hdr; handler }
let[@inline] delete_h pat hdr handler = Pat { meth = Method.Delete; pat; hdr; handler }
let[@inline] route_h meth pat hdr handler = Pat { meth; pat; hdr; handler }

let segments_to_pat : string list -> unit pat = function
  | [] -> Root
  | segs -> Lits segs

let get_ segments handler =
  Pat { meth = Method.Get; pat = segments_to_pat segments; hdr = H0;
        handler = fun () () ctx respond -> handler ctx respond }

let post_ segments handler =
  Pat { meth = Method.Post; pat = segments_to_pat segments; hdr = H0;
        handler = fun () () ctx respond -> handler ctx respond }

type t = route list

let empty = []
let add r t = r :: t
let of_list rs = rs

(** {1 Dispatch}

    Zero-copy dispatch: parse target once, match patterns against path span.
    Headers are extracted per-route based on what each route declares it needs.
    The respond function is passed through to the handler for direct response writing. *)

(** Try matching a single route. Returns true if matched. *)
let[@inline] try_route (buf : bytes) ctx (local_ headers : Header.t list) (Pat { meth = route_meth; pat; hdr; handler }) req_meth ~respond =
  if not (Poly.equal req_meth route_meth) then false
  else
    try
      let result = match_pat ctx.buf pat ctx.path in
      if match_complete result then begin
        let h = extract_headers buf headers hdr in
        handler result.cap h ctx respond;
        true
      end
      else false
    with No_match -> false

let rec dispatch_loop (buf : bytes) ctx (local_ headers) meth ~respond = function
  | [] -> false
  | route :: rest ->
      if try_route buf ctx headers route meth ~respond then true
      else dispatch_loop buf ctx headers meth ~respond rest

(** Primary dispatch function. Parses target and dispatches to routes.
    Zero-copy: headers are only extracted for routes that need them.
    [respond] is called by the handler to write the response directly.
    Returns true if a route matched, false otherwise. *)
let[@inline] dispatch (buf : bytes) ~meth ~target ~(local_ headers : Header.t list) t ~respond =
  let parsed = Target.parse buf target in
  let path = Target.path parsed in
  let query = Target.query parsed in
  let ctx = { buf; path; query } in
  dispatch_loop buf ctx headers meth ~respond t
