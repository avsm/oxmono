(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz-native HTTP routing *)

type meth = Httpz.Method.t

let meth_to_string = Httpz.Method.to_string

module Request = struct
  type t = {
    meth : meth;
    path : string;
    query : (string * string) list;
    headers : (string * string) list;
  }

  let create ~meth ~path ~query ~headers = { meth; path; query; headers }
  let meth t = t.meth
  let path t = t.path
  let query t = t.query

  let query_param t name =
    List.find_map
      (fun (k, v) -> if String.equal k name then Some v else None)
      t.query

  let query_params t name =
    List.filter_map
      (fun (k, v) -> if String.equal k name then Some v else None)
      t.query

  let headers t = t.headers

  let header t name =
    let name_lower = String.lowercase_ascii name in
    List.find_map
      (fun (k, v) ->
        if String.equal (String.lowercase_ascii k) name_lower then Some v
        else None)
      t.headers

  let cache_key t =
    let sorted_query =
      List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) t.query
    in
    let query_string =
      String.concat "&"
        (List.map (fun (k, v) -> k ^ "=" ^ v) sorted_query)
    in
    if query_string = "" then t.path else t.path ^ "?" ^ query_string
end

module Response = struct
  type t = {
    status : Httpz.Res.status;
    headers : (string * string) list;
    body : string;
  }

  let html content =
    {
      status = Httpz.Res.Success;
      headers = [ ("content-type", "text/html; charset=utf-8") ];
      body = content;
    }

  let json content =
    {
      status = Httpz.Res.Success;
      headers = [ ("content-type", "application/json; charset=utf-8") ];
      body = content;
    }

  let xml content =
    {
      status = Httpz.Res.Success;
      headers = [ ("content-type", "application/xml") ];
      body = content;
    }

  let atom content =
    {
      status = Httpz.Res.Success;
      headers = [ ("content-type", "application/atom+xml; charset=utf-8") ];
      body = content;
    }

  let plain content =
    {
      status = Httpz.Res.Success;
      headers = [ ("content-type", "text/plain") ];
      body = content;
    }

  let redirect ~status ~location =
    { status; headers = [ ("Location", location) ]; body = "" }

  let not_found = { status = Httpz.Res.Not_found; headers = []; body = "Not Found" }
  let raw ~status ~headers body = { status; headers; body }
  let status t = t.status
  let headers t = t.headers
  let body t = t.body
end

(* Path pattern implementation using GADTs *)

type _ pattern =
  | Root : unit pattern
  | Exact : string -> unit pattern
  | Param : string pattern
  | Rest : string pattern
  | Compose : 'a pattern * 'b pattern -> ('a * 'b) pattern

let root = Root
let exact s = Exact s
let param = Param
let rest = Rest
let ( @/ ) p1 p2 = Compose (p1, p2)

(* Pattern matching helper *)
let split_path path =
  let path = if String.length path > 0 && path.[0] = '/' then
    String.sub path 1 (String.length path - 1)
  else path
  in
  if path = "" then []
  else String.split_on_char '/' path

type 'a match_result =
  | Match of 'a * string list
  | NoMatch

let rec match_pattern : type a. a pattern -> string list -> a match_result =
 fun pattern segments ->
  match (pattern, segments) with
  | Root, [] -> Match ((), [])
  | Root, [ "" ] -> Match ((), [])
  | Exact s, seg :: rest when String.equal s seg -> Match ((), rest)
  | Param, seg :: rest -> Match (seg, rest)
  | Rest, segments -> Match (String.concat "/" segments, [])
  | Compose (p1, p2), segments -> (
      match match_pattern p1 segments with
      | NoMatch -> NoMatch
      | Match (v1, remaining) -> (
          match match_pattern p2 remaining with
          | NoMatch -> NoMatch
          | Match (v2, final) -> Match ((v1, v2), final)))
  | _ -> NoMatch

(** {2 Span-Based Pattern Matching}

    Match patterns against path spans without allocating intermediate strings.
    Captured parameters are converted to strings only on successful match. *)

(** Span-based match result. Keeps captured values as spans. *)
type 'a span_result =
  | SpanOk of 'a * Httpz.Span.t  (* captured, remaining path *)
  | SpanFail

(** Match pattern against path span. Returns captured values as spans where applicable.
    For Param and Rest patterns, the captured value type is Httpz.Span.t.
    The caller must convert to string after successful match. *)
let rec match_pattern_span : type a. Httpz.buffer -> a pattern -> Httpz.Span.t -> a span_result =
 fun buf pattern path ->
  match pattern with
  | Root ->
      if Httpz.Target.path_is_empty buf path
      then SpanOk ((), Httpz.Span.make ~off:(Httpz.Span.of_int 0) ~len:(Httpz.Span.of_int 0))
      else SpanFail
  | Exact s ->
      let #(matched, rest) = Httpz.Target.match_segment buf path s in
      if matched then SpanOk ((), rest) else SpanFail
  | Param ->
      let #(matched, seg, rest) = Httpz.Target.match_param buf path in
      if matched then SpanOk (Httpz.Span.to_string buf seg, rest) else SpanFail
  | Rest ->
      SpanOk (Httpz.Span.to_string buf path, Httpz.Span.make ~off:(Httpz.Span.of_int 0) ~len:(Httpz.Span.of_int 0))
  | Compose (p1, p2) -> (
      match match_pattern_span buf p1 path with
      | SpanFail -> SpanFail
      | SpanOk (v1, remaining) -> (
          match match_pattern_span buf p2 remaining with
          | SpanFail -> SpanFail
          | SpanOk (v2, final) -> SpanOk ((v1, v2), final)))

(** Check if path span ends cleanly (empty or just trailing slash). *)
let span_match_complete buf path =
  Httpz.Target.path_is_empty buf path

type handler = Request.t -> Response.t
type 'a param_handler = 'a -> Request.t -> Response.t

(* Existential wrapper for routes *)
type t =
  | Route : {
      meth : meth;
      pattern : 'a pattern;
      handler : 'a param_handler;
    }
      -> t
  | ExactRoute : {
      meth : meth;
      segments : string list;
      handler : handler;
    }
      -> t

let get pattern handler = Route { meth = Httpz.Method.Get; pattern; handler }
let post pattern handler = Route { meth = Httpz.Method.Post; pattern; handler }
let route meth pattern handler = Route { meth; pattern; handler }

(* Simple exact segment matching without GADTs *)
let get_ segments handler = ExactRoute { meth = Httpz.Method.Get; segments; handler }
let post_ segments handler = ExactRoute { meth = Httpz.Method.Post; segments; handler }

module Routes = struct
  type route = t
  type t = route list

  let empty = []
  let add route routes = route :: routes
  let of_list routes = routes

  let match_segments expected actual =
    List.length expected = List.length actual &&
    List.for_all2 String.equal expected actual

  let try_route route req =
    match route with
    | Route { meth; pattern; handler } ->
        if Request.meth req <> meth then None
        else
          let segments = split_path (Request.path req) in
          (match match_pattern pattern segments with
          | Match (params, []) -> Some (handler params req)
          | Match (params, [ "" ]) -> Some (handler params req)
          | _ -> None)
    | ExactRoute { meth; segments; handler } ->
        if Request.meth req <> meth then None
        else
          let path_segments = split_path (Request.path req) in
          (* Handle trailing empty segment from trailing slash *)
          let path_segments =
            match List.rev path_segments with
            | "" :: rest -> List.rev rest
            | _ -> path_segments
          in
          if match_segments segments path_segments then
            Some (handler req)
          else
            None

  let dispatch routes req =
    List.find_map (fun route -> try_route route req) routes

  let fold f routes acc = List.fold_right f routes acc

  (** {2 Span-Based Dispatch}

      Zero-allocation route matching using httpz spans.
      Only allocates strings when a route matches and needs captured params. *)

  (** Match exact segments against path span without allocation. *)
  let match_exact_segments buf segments path =
    let rec loop segs path =
      match segs with
      | [] -> Httpz.Target.path_is_empty buf path
      | seg :: rest ->
          let #(matched, remaining) = Httpz.Target.match_segment buf path seg in
          if matched then loop rest remaining else false
    in
    loop segments path

  (** Try to match a route against the path span. Only builds Request.t on match. *)
  let try_route_span buf meth target headers route =
    match route with
    | Route { meth = route_meth; pattern; handler } ->
        if meth <> route_meth then None
        else
          let parsed = Httpz.Target.parse buf target in
          let path = Httpz.Target.skip_leading_slash buf (Httpz.Target.path parsed) in
          (match match_pattern_span buf pattern path with
          | SpanOk (params, remaining) when span_match_complete buf remaining ->
              (* Only now do we build the full request *)
              let path_str = Httpz.Span.to_string buf (Httpz.Target.path parsed) in
              let query_pairs = Httpz.Target.query_to_string_pairs buf (Httpz.Target.query parsed) in
              let req = Request.create ~meth ~path:path_str ~query:query_pairs ~headers in
              Some (handler params req)
          | _ -> None)
    | ExactRoute { meth = route_meth; segments; handler } ->
        if meth <> route_meth then None
        else
          let parsed = Httpz.Target.parse buf target in
          let path = Httpz.Target.skip_leading_slash buf (Httpz.Target.path parsed) in
          if match_exact_segments buf segments path then
            let path_str = Httpz.Span.to_string buf (Httpz.Target.path parsed) in
            let query_pairs = Httpz.Target.query_to_string_pairs buf (Httpz.Target.query parsed) in
            let req = Request.create ~meth ~path:path_str ~query:query_pairs ~headers in
            Some (handler req)
          else
            None

  (** Dispatch using span-based matching.
      [buf] is the httpz buffer containing the request.
      [meth] is the HTTP method.
      [target] is the target span (path + query).
      [headers] are the pre-converted headers (string pairs).

      This avoids allocating path strings and query parameters unless a route matches. *)
  let dispatch_span buf ~meth ~target ~headers routes =
    List.find_map (fun route -> try_route_span buf meth target headers route) routes
end
