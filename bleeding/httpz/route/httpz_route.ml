(* route.ml - Trie-based HTTP routing

   The trie matches literal path segments. At each node we store routes
   whose literal prefix ends there. Pattern matching handles the suffix
   (wildcards, tails) against remaining segments.
*)

open Base

module I16 = Stdlib_stable.Int16_u
module Iarray = Stdlib_stable.Iarray

let[@inline always] i16 x = I16.of_int x

(** {1 Response Types} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }
  | Stream_bigstring of
      { length : int option
      ; iter : (Base_bigstring.t -> off:int -> len:int -> unit) -> unit
      }

type resp_header = Httpz.Header_name.t * string

type respond = status:Httpz.Res.status -> headers:local_ resp_header list -> body -> unit

(** {2 Response Helpers} *)

let[@inline] html (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8")]
    (String s)

let[@inline] json (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8")]
    (String s)

let[@inline] xml (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/xml")]
    (String s)

let[@inline] atom (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8")]
    (String s)

let[@inline] plain (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "text/plain")]
    (String s)

let[@inline] redirect (local_ respond) ~status ~location =
  respond ~status ~headers:[(Httpz.Header_name.Location, location)] Empty

let[@inline] not_found (local_ respond) =
  respond ~status:Httpz.Res.Not_found ~headers:[] (String "Not Found")

let[@inline] respond_string (local_ respond) ~status ?(local_ headers = []) s =
  respond ~status ~headers (String s)

let[@inline] stream (local_ respond) ~status ?(local_ headers = []) ?length iter =
  respond ~status ~headers (Stream { length; iter })

(** {1 Request Context} *)

(* [path_span] rather than a materialised segment list: the trie walk compares
   segments in place against the buffer, so nothing is copied for a route made
   only of literals. [ctx] never escapes [dispatch] — and must not, since
   [buf] is the connection's reused parse buffer. *)
type ctx = {
  (* [global_]: the parse buffer is always a heap value, so reading it out of
     a stack-allocated [ctx] must not inherit the record's locality — callers
     pass it to functions expecting a global [bytes]. *)
  global_ buf : bytes;
  meth : Httpz.Method.t;
  path_span : Httpz.Span.t;
  query : Httpz.Span.t;
  body : Httpz.Span.t;
  content_length : int64#;
}

let[@inline] meth (ctx @ local) = ctx.meth
let[@inline] is_head (ctx @ local) = phys_equal ctx.meth Httpz.Method.Head

(* The path exactly as it appeared in the request, including repeated and
   trailing slashes. *)
let[@inline] path (ctx @ local) = Httpz.Span.to_string ctx.buf ctx.path_span

(* Decode a query key or value out of the parse buffer, resolving ["%XX"] and
   reading ['+'] as a space per application/x-www-form-urlencoded.

   Most values contain neither, so scan first and blit the span straight to the
   result. When there is something to decode, the scratch is a region-allocated
   [bytes] and the only heap block is the returned string.

   A malformed ['%'] cannot occur: {!Httpz.parse} validates the whole target
   against the RFC 3986 grammar, which admits a ['%'] only in a complete
   triplet, and rejects the request otherwise. The [n < 0] branch is therefore
   unreachable through the parser and yields the undecoded text, which is the
   conservative answer for a caller that built a target by hand. *)
let[@inline] decode_span (local_ buf : bytes) (sp : Httpz.Span.t) =
  let off = Httpz.Span.off sp in
  let len = Httpz.Span.len sp in
  let local_ s = Stdlib.Bytes.unsafe_to_string buf in
  if not (Uriz.Raw.needs_decode s ~pos:off ~len ~plus_as_space:true)
  then Httpz.Span.to_string buf sp
  else begin
    let local_ dst = Bytes.create len in
    let n = Uriz.Raw.pct_decode_into s ~pos:off ~len ~dst ~dst_pos:0 ~plus_as_space:true in
    if n < 0
    then Httpz.Span.to_string buf sp
    else (
      (* [dst] borrows this region, so the copy out of it is bound rather than
         left in tail position. *)
      let out = Bytes.To_string.sub dst ~pos:0 ~len:n in
      out)
  end
;;

let[@inline] query_param (ctx @ local) name =
  let #(found, span) = Httpz.Target.find_query_param ctx.buf ctx.query name in
  if found then Some (decode_span ctx.buf span) else None

let query_params (ctx @ local) name =
  (* Read the fields out before the closure: [ctx] is local, but [buf] is the
     global parse buffer and [query] is unboxed, so the closure captures
     neither the record nor its region. *)
  let buf = ctx.buf in
  let query = ctx.query in
  Httpz.Target.fold_query_params buf query ~init:[] ~f:(fun acc key value ->
    if Httpz.Span.equal buf key name then decode_span buf value :: acc else acc)
  |> List.rev

let[@inline] query (ctx @ local) = Httpz.Target.query_to_string_pairs ctx.buf ctx.query

let[@inline] body (ctx @ local) = ctx.body

let[@inline] body_string (ctx @ local) =
  let sp = ctx.body in
  if Httpz.Span.len sp > 0 then Some (Httpz.Span.to_string ctx.buf sp)
  else None

let[@inline] content_length (ctx @ local) = ctx.content_length

(** {2 Response Helpers - WebDAV} *)

let[@inline] xml_multistatus (local_ respond) s =
  respond ~status:Httpz.Res.Multi_status
    ~headers:[(Httpz.Header_name.Content_type, "application/xml; charset=utf-8")]
    (String s)

(** {2 Lazy Response Helpers}

    These variants skip body generation for HEAD requests. Pass a thunk that
    generates the body; it will only be called for non-HEAD requests. *)

let[@inline] html_gen (ctx @ local) (local_ respond) f =
  if is_head ctx then
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8")]
      Empty
  else
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8")]
      (String (f ()))

let[@inline] json_gen (ctx @ local) (local_ respond) f =
  if is_head ctx then
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8")]
      Empty
  else
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8")]
      (String (f ()))

let[@inline] xml_gen (ctx @ local) (local_ respond) f =
  if is_head ctx then
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/xml")]
      Empty
  else
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/xml")]
      (String (f ()))

let[@inline] atom_gen (ctx @ local) (local_ respond) f =
  if is_head ctx then
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8")]
      Empty
  else
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8")]
      (String (f ()))

let[@inline] plain_gen (ctx @ local) (local_ respond) f =
  if is_head ctx then
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "text/plain")]
      Empty
  else
    respond ~status:Httpz.Res.Success
      ~headers:[(Httpz.Header_name.Content_type, "text/plain")]
      (String (f ()))

(** {1 Path Patterns} *)

type _ pat =
  | End : unit pat
  | Lit : string * 'a pat -> 'a pat
  | Seg : 'a pat -> (string * 'a) pat
  | Tail : string list pat

let root = End
let[@inline] lit s rest = Lit (s, rest)
let[@inline] ( / ) s rest = Lit (s, rest)
let[@inline] seg rest = Seg rest
let tail = Tail

(** Extract literal prefix from pattern, return (prefix, suffix). *)
let rec split_pat : type a. a pat -> string list * a pat = function
  | Lit (s, rest) ->
      let prefix, suffix = split_pat rest in
      (s :: prefix, suffix)
  | pat -> ([], pat)

(** {2 Segment Cursor}

    The path is walked in place, as offsets into the parse buffer. A segment
    is only copied into a [string] when a [Seg] or [Tail] actually captures
    it, so a route made only of literals copies nothing. *)

(* Start of the next segment at or after [pos], skipping any run of '/'. *)
let[@inline] skip_slashes (local_ buf : bytes) ~pos ~stop =
  let mutable p = pos in
  while p < stop && Char.equal (Bytes.unsafe_get buf p) '/' do
    p <- p + 1
  done;
  p
;;

(* End of the segment starting at [pos]. *)
let[@inline] seg_end (local_ buf : bytes) ~pos ~stop =
  let mutable e = pos in
  while e < stop && not (Char.equal (Bytes.unsafe_get buf e) '/') do
    e <- e + 1
  done;
  e
;;

let[@inline] sub_string (local_ buf : bytes) ~pos ~len =
  let dst = Bytes.create len in
  Bytes.blit ~src:buf ~src_pos:pos ~dst ~dst_pos:0 ~len;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

let rec segments_of (local_ buf : bytes) ~pos ~stop =
  let p = skip_slashes buf ~pos ~stop in
  if p >= stop
  then []
  else (
    let e = seg_end buf ~pos:p ~stop in
    sub_string buf ~pos:p ~len:(e - p) :: segments_of buf ~pos:e ~stop)
;;

(** {2 Pattern Matching}

    Match the non-literal suffix of a pattern against the remaining path.
    Called after the trie walk has consumed the literal prefix. *)

exception No_match

let rec match_suffix : type a. a pat -> bytes -> pos:int -> stop:int -> a =
  fun pat buf ~pos ~stop ->
   let p = skip_slashes buf ~pos ~stop in
   match pat with
   | End -> if p >= stop then () else Stdlib.raise_notrace No_match
   | Lit _ -> Stdlib.raise_notrace No_match (* Lits are consumed by the trie *)
   | Seg rest ->
     if p >= stop
     then Stdlib.raise_notrace No_match
     else (
       let e = seg_end buf ~pos:p ~stop in
       let captured = sub_string buf ~pos:p ~len:(e - p) in
       (captured, match_suffix rest buf ~pos:e ~stop))
   | Tail -> segments_of buf ~pos:p ~stop

(** {1 Header Requirements} *)

type _ hdr =
  | H0 : unit hdr
  | H1 : Httpz.Header_name.t * 'a hdr -> (string option * 'a) hdr

let h0 = H0
let[@inline] ( +> ) name rest = H1 (name, rest)

let rec find_header buf (local_ headers : Httpz.Header.t list) name =
  match headers with
  | [] -> None
  | h :: rest ->
      if phys_equal h.Httpz.Header.name name
      then Some (Httpz.Span.to_string buf h.Httpz.Header.value)
      else find_header buf rest name

let rec extract_headers : type h. bytes -> local_ Httpz.Header.t list -> h hdr -> h =
  fun buf headers spec ->
    match spec with
    | H0 -> ()
    | H1 (name, rest) ->
        let value = find_header buf headers name in
        (value, extract_headers buf headers rest)

(** {1 Routes} *)

type ('a, 'h) handler = 'a -> 'h -> ctx @ local -> local_ respond -> unit

type route =
  | Route : {
      meth : Httpz.Method.t;
      pat : 'a pat;
      hdr : 'h hdr;
      handler : ('a, 'h) handler;
    } -> route

(** {2 Route Constructors} *)

let[@inline] route meth pat hdr handler = Route { meth; pat; hdr; handler }

let[@inline] get pat handler =
  Route { meth = Httpz.Method.Get; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] post pat handler =
  Route { meth = Httpz.Method.Post; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] put pat handler =
  Route { meth = Httpz.Method.Put; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] delete pat handler =
  Route { meth = Httpz.Method.Delete; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] get_h pat hdr handler = Route { meth = Httpz.Method.Get; pat; hdr; handler }
let[@inline] post_h pat hdr handler = Route { meth = Httpz.Method.Post; pat; hdr; handler }
let[@inline] put_h pat hdr handler = Route { meth = Httpz.Method.Put; pat; hdr; handler }
let[@inline] delete_h pat hdr handler = Route { meth = Httpz.Method.Delete; pat; hdr; handler }

let[@inline] get_h1 pat name handler =
  Route { meth = Httpz.Method.Get; pat; hdr = H1 (name, H0);
          handler = fun a (h, ()) ctx respond -> handler a h ctx respond }

let[@inline] post_h1 pat name handler =
  Route { meth = Httpz.Method.Post; pat; hdr = H1 (name, H0);
          handler = fun a (h, ()) ctx respond -> handler a h ctx respond }

(** Convenience for literal-only paths *)
let rec lits_to_pat : string list -> unit pat = function
  | [] -> End
  | s :: rest -> Lit (s, lits_to_pat rest)

let[@inline] get_ segments handler =
  Route { meth = Httpz.Method.Get; pat = lits_to_pat segments; hdr = H0;
          handler = fun () () ctx respond -> handler ctx respond }

let[@inline] post_ segments handler =
  Route { meth = Httpz.Method.Post; pat = lits_to_pat segments; hdr = H0;
          handler = fun () () ctx respond -> handler ctx respond }

(** {2 WebDAV Route Constructors} *)

let[@inline] propfind pat handler =
  Route { meth = Httpz.Method.Propfind; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] proppatch pat handler =
  Route { meth = Httpz.Method.Proppatch; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] mkcol pat handler =
  Route { meth = Httpz.Method.Mkcol; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] report pat handler =
  Route { meth = Httpz.Method.Report; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] copy_ pat handler =
  Route { meth = Httpz.Method.Copy; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] move_ pat handler =
  Route { meth = Httpz.Method.Move; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] lock pat handler =
  Route { meth = Httpz.Method.Lock; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] unlock pat handler =
  Route { meth = Httpz.Method.Unlock; pat; hdr = H0;
          handler = fun a () ctx respond -> handler a ctx respond }

let[@inline] propfind_h pat hdr handler = Route { meth = Httpz.Method.Propfind; pat; hdr; handler }
let[@inline] proppatch_h pat hdr handler = Route { meth = Httpz.Method.Proppatch; pat; hdr; handler }
let[@inline] mkcol_h pat hdr handler = Route { meth = Httpz.Method.Mkcol; pat; hdr; handler }
let[@inline] report_h pat hdr handler = Route { meth = Httpz.Method.Report; pat; hdr; handler }
let[@inline] copy_h pat hdr handler = Route { meth = Httpz.Method.Copy; pat; hdr; handler }
let[@inline] move_h pat hdr handler = Route { meth = Httpz.Method.Move; pat; hdr; handler }
let[@inline] lock_h pat hdr handler = Route { meth = Httpz.Method.Lock; pat; hdr; handler }
let[@inline] unlock_h pat hdr handler = Route { meth = Httpz.Method.Unlock; pat; hdr; handler }

(** {1 Trie-Based Router} *)

(** At each node: routes whose literal prefix ends here, indexed by suffix
    type. Literal children are a pair of parallel arrays rather than a map:
    the fan-out at a node in a real route table is a handful of entries, so a
    linear scan beats a comparator-driven lookup — and, more importantly, it
    can compare a key against the buffer in place. *)
type node = {
  keys : string iarray; (* literal children, parallel to [kids] *)
  kids : node iarray;
  exact : route list; (* suffix = End *)
  wild : route list; (* suffix = Seg _ *)
  catch : route list; (* suffix = Tail *)
}

let empty_node = { keys = [::]; kids = [::]; exact = []; wild = []; catch = [] }

(* Routes are kept alongside the built tree so that [add] can rebuild. Adds
   only happen while assembling the table, never on the dispatch path. *)
type t =
  { routes : route list
  ; root : node
  }

(* Mutable only during construction. *)
type builder =
  { mutable bkeys : (string * builder) list
  ; mutable bexact : route list
  ; mutable bwild : route list
  ; mutable bcatch : route list
  }

let new_builder () = { bkeys = []; bexact = []; bwild = []; bcatch = [] }

let rec descend b = function
  | [] -> b
  | s :: rest ->
    let child =
      match List.Assoc.find b.bkeys s ~equal:String.equal with
      | Some c -> c
      | None ->
        let c = new_builder () in
        b.bkeys <- (s, c) :: b.bkeys;
        c
    in
    descend child rest
;;

let rec freeze b =
  { keys = Iarray.of_list (List.map b.bkeys ~f:fst)
  ; kids = Iarray.of_list (List.map b.bkeys ~f:(fun (_, c) -> freeze c))
  ; exact = b.bexact
  ; wild = b.bwild
  ; catch = b.bcatch
  }
;;

(* Each route is stored with its pattern replaced by the suffix the trie did
   not consume, so dispatch never has to re-run [split_pat]. *)
let build routes =
  let root = new_builder () in
  List.iter routes ~f:(fun (Route { meth; pat; hdr; handler }) ->
    let prefix, suffix = split_pat pat in
    let r = Route { meth; pat = suffix; hdr; handler } in
    let b = descend root prefix in
    match suffix with
    | End -> b.bexact <- r :: b.bexact
    | Seg _ -> b.bwild <- r :: b.bwild
    | Tail -> b.bcatch <- r :: b.bcatch
    | Lit _ -> () (* cannot happen after [split_pat] *));
  freeze root
;;

let empty : t = { routes = []; root = empty_node }

(* [routes] is held newest-first, matching [add]'s prepend. [build] also
   prepends as it walks, so it must be fed oldest-first for the newest route
   to end up at the head of each node's list — which is what makes a later
   [add] shadow an earlier one, as before. *)
let add route t =
  let routes = route :: t.routes in
  { routes; root = build (List.rev routes) }
;;

let of_list routes = { routes = List.rev routes; root = build routes }

(** {2 Dispatch} *)

let[@inline] try_route (Route { meth = route_meth; pat; hdr; handler })
    meth (local_ req_headers) buf ~pos ~stop (ctx @ local) (local_ respond) =
  (* HEAD matches GET routes - handlers can check ctx.meth to skip body generation *)
  let method_matches =
    phys_equal meth route_meth ||
    (phys_equal meth Httpz.Method.Head && phys_equal route_meth Httpz.Method.Get)
  in
  if not method_matches then false
  else
    (* [pat] is already the suffix; the prefix was consumed by the walk. *)
    match match_suffix pat buf ~pos ~stop with
    | captures ->
        let h = extract_headers ctx.buf req_headers hdr in
        handler captures h ctx respond;
        true
    | exception No_match -> false

let rec try_routes routes meth (local_ req_headers) buf ~pos ~stop (ctx @ local)
    (local_ respond) =
  match routes with
  | [] -> false
  | r :: rest ->
      if try_route r meth req_headers buf ~pos ~stop ctx respond then true
      else try_routes rest meth req_headers buf ~pos ~stop ctx respond

(* Index of the literal child matching [buf[pos, pos+len)], or -1. The key is
   compared against the buffer in place, so no segment string is built. *)
let[@inline] find_kid (node : node) (local_ buf : bytes) ~pos ~len =
  let ks = node.keys in
  let m = Iarray.length ks in
  let mutable i = 0 in
  let mutable found = -1 in
  while found < 0 && i < m do
    let k = Iarray.unsafe_get ks i in
    if String.length k = len
       && Httpz.Span.equal buf (Httpz.Span.make ~off:(i16 pos) ~len:(i16 len)) k
    then found <- i
    else i <- i + 1
  done;
  found
;;

(** Walk the trie matching segments in place, trying routes at each node. *)
let rec dispatch_walk (node : node) meth (local_ req_headers) buf ~pos ~stop
    (ctx @ local) (local_ respond) =
  (* Try catchalls first - they match any remaining path *)
  if try_routes node.catch meth req_headers buf ~pos ~stop ctx respond then true
  else
    let p = skip_slashes buf ~pos ~stop in
    if p >= stop then
      (* End of path - try exact matches *)
      try_routes node.exact meth req_headers buf ~pos:p ~stop ctx respond
    else
      let e = seg_end buf ~pos:p ~stop in
      let idx = find_kid node buf ~pos:p ~len:(e - p) in
      let found =
        if idx >= 0
        then
          dispatch_walk (Iarray.unsafe_get node.kids idx) meth req_headers buf ~pos:e
            ~stop ctx respond
        else false
      in
      if found then true
      else
        (* Try wildcard routes at this node *)
        try_routes node.wild meth req_headers buf ~pos:p ~stop ctx respond

let dispatch buf ~meth ~(path : Httpz.Span.t) ~(query : Httpz.Span.t)
    ~(body : Httpz.Span.t) ~(content_length : int64#)
    ~(local_ headers : Httpz.Header.t list) (t : t) ~(local_ respond) =
  let pos = Httpz.Span.off path in
  let stop = pos + Httpz.Span.len path in
  let ctx = stack_ { buf; meth; path_span = path; query; body; content_length } in
  (dispatch_walk t.root meth headers buf ~pos ~stop ctx respond) [@nontail]
