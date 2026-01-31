(* route.ml - Segment-based HTTP routing with Base_trie

   Design principles:
   1. Parse path to segment list once at dispatch entry
   2. Walk trie by segments using find_child (no list copying)
   3. Local headers and stack-allocated response tuples
   4. Inline hot paths
*)

open Base

(** {1 Response Types} *)

type body =
  | Empty
  | String of string
  | Bigstring of { buf : Base_bigstring.t; off : int; len : int }
  | Stream of { length : int option; iter : (string -> unit) -> unit }

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

type ctx = {
  buf : bytes;
  segments : string list;
  query : Httpz.Span.t;
}

let[@inline] path ctx =
  "/" ^ String.concat ~sep:"/" ctx.segments

let[@inline] query_param ctx name =
  let #(found, span) = Httpz.Target.find_query_param ctx.buf ctx.query name in
  if found then Some (Httpz.Span.to_string ctx.buf span) else None

let query_params ctx name =
  Httpz.Target.fold_query_params ctx.buf ctx.query ~init:[] ~f:(fun acc key value ->
    if Httpz.Span.equal ctx.buf key name
    then Httpz.Span.to_string ctx.buf value :: acc
    else acc)
  |> List.rev

let[@inline] query ctx = Httpz.Target.query_to_string_pairs ctx.buf ctx.query

(** {1 Path Patterns}

    Patterns match against segment lists. The type parameter encodes captures. *)

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

(** {2 Pattern Matching}

    Uses exception for mismatch - avoids Option boxing on the hot path. *)

exception No_match

let rec match_pat : type a. a pat -> string list -> a * string list =
  fun pat segments ->
    match pat, segments with
    | End, segs -> ((), segs)
    | Lit (expected, rest), seg :: segs when String.equal expected seg ->
        match_pat rest segs
    | Lit _, _ -> Stdlib.raise_notrace No_match
    | Seg rest, seg :: segs ->
        let captures, remaining = match_pat rest segs in
        (seg, captures), remaining
    | Seg _, [] -> Stdlib.raise_notrace No_match
    | Tail, segs -> (segs, [])

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
      if Poly.equal h.Httpz.Header.name name
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

type ('a, 'h) handler = 'a -> 'h -> ctx -> local_ respond -> unit

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

(** {1 Trie-Based Dispatch}

    Routes indexed by literal prefix using Base_trie.
    Dispatch walks trie via find_child - no list copying. *)

module Path_trie = Trie.Of_list(String)

module Router = struct
  module Meth = struct
    module T = struct
      type t = Httpz.Method.t
      let compare = Poly.compare
      let sexp_of_t m = Sexp.Atom (Httpz.Method.to_string m)
    end
    include T
    include Comparator.Make(T)
  end

  type node_data = {
    handlers : route list Map.M(Meth).t;
    wildcards : route list Map.M(Meth).t;
    catchalls : route list Map.M(Meth).t;
  }

  let empty_node = {
    handlers = Map.empty (module Meth);
    wildcards = Map.empty (module Meth);
    catchalls = Map.empty (module Meth);
  }

  type t = node_data Path_trie.t

  let create () = Trie.empty Path_trie.Keychain.keychainable

  (** Extract literal prefix and terminator from pattern *)
  let rec literal_prefix : type a. a pat -> string list * [`End | `Seg | `Tail] = function
    | End -> [], `End
    | Lit (s, rest) ->
        let prefix, term = literal_prefix rest in
        s :: prefix, term
    | Seg _ -> [], `Seg
    | Tail -> [], `Tail

  let add_to_map meth route map =
    Map.update map meth ~f:(function
      | None -> [route]
      | Some routes -> route :: routes)

  let add (Route { meth; pat; _ } as route) t =
    let prefix, terminator = literal_prefix pat in
    Trie.update t prefix ~f:(fun data_opt ->
      let data = Option.value data_opt ~default:empty_node in
      match terminator with
      | `End -> { data with handlers = add_to_map meth route data.handlers }
      | `Seg -> { data with wildcards = add_to_map meth route data.wildcards }
      | `Tail -> { data with catchalls = add_to_map meth route data.catchalls })

  let of_routes routes =
    List.fold routes ~init:(create ()) ~f:(fun t r -> add r t)

  (** Try a single route against remaining segments *)
  let[@inline] try_route (Route { meth = route_meth; pat; hdr; handler })
      meth (local_ req_headers) segments ctx (local_ respond) =
    if not (Poly.equal meth route_meth) then false
    else
      match match_pat pat segments with
      | captures, _remaining ->
          let h = extract_headers ctx.buf req_headers hdr in
          handler captures h ctx respond;
          true
      | exception No_match -> false

  let rec try_routes routes meth (local_ req_headers) segments ctx (local_ respond) =
    match routes with
    | [] -> false
    | r :: rest ->
        if try_route r meth req_headers segments ctx respond then true
        else try_routes rest meth req_headers segments ctx respond

  (** Dispatch by walking trie with find_child - no list allocation *)
  let rec dispatch_walk trie meth (local_ req_headers) segments all_segments ctx (local_ respond) =
    let node = Trie.datum trie |> Option.value ~default:empty_node in
    (* Try catchalls at this node - use all_segments so pattern can match from root *)
    match Map.find node.catchalls meth with
    | Some routes when try_routes routes meth req_headers all_segments ctx respond -> true
    | _ ->
    match segments with
    | [] ->
        (* End of path - try exact handlers *)
        (match Map.find node.handlers meth with
         | Some routes -> try_routes routes meth req_headers all_segments ctx respond
         | None -> false)
    | seg :: rest ->
        (* Try literal child first (most specific) *)
        let found = match Trie.find_child trie seg with
          | Some child -> dispatch_walk child meth req_headers rest all_segments ctx respond
          | None -> false
        in
        if found then true
        else
          (* Try wildcard routes at this node *)
          match Map.find node.wildcards meth with
          | Some routes -> try_routes routes meth req_headers all_segments ctx respond
          | None -> false

  let[@inline] dispatch t ~meth ~(local_ headers) ~segments ~ctx ~(local_ respond) =
    dispatch_walk t meth headers segments segments ctx respond
end

(** {1 Public Interface} *)

type t = Router.t

let of_list routes = Router.of_routes routes
let empty = Router.create ()
let add route t = Router.add route t

let parse_segments path =
  String.split path ~on:'/'
  |> List.filter ~f:(fun s -> not (String.is_empty s))

let dispatch buf ~meth ~(target : Httpz.Target.t) ~(local_ headers : Httpz.Header.t list) t ~(local_ respond) =
  let path_str = Httpz.Span.to_string buf (Httpz.Target.path target) in
  let segments = parse_segments path_str in
  let ctx = { buf; segments; query = Httpz.Target.query target } in
  Router.dispatch t ~meth ~headers ~segments ~ctx ~respond
