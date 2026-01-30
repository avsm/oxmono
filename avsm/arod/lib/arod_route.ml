(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Framework-agnostic HTTP routing *)

type meth = [ `GET | `POST | `PUT | `DELETE | `HEAD | `OPTIONS ]

let meth_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"

let meth_of_string = function
  | "GET" -> Some `GET
  | "POST" -> Some `POST
  | "PUT" -> Some `PUT
  | "DELETE" -> Some `DELETE
  | "HEAD" -> Some `HEAD
  | "OPTIONS" -> Some `OPTIONS
  | _ -> None

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
    status : int;
    headers : (string * string) list;
    body : string;
  }

  let html content =
    {
      status = 200;
      headers = [ ("content-type", "text/html; charset=utf-8") ];
      body = content;
    }

  let json content =
    {
      status = 200;
      headers = [ ("content-type", "application/json; charset=utf-8") ];
      body = content;
    }

  let xml content =
    {
      status = 200;
      headers = [ ("content-type", "application/xml") ];
      body = content;
    }

  let atom content =
    {
      status = 200;
      headers = [ ("content-type", "application/atom+xml; charset=utf-8") ];
      body = content;
    }

  let plain content =
    {
      status = 200;
      headers = [ ("content-type", "text/plain") ];
      body = content;
    }

  let redirect ~code ~location =
    { status = code; headers = [ ("Location", location) ]; body = "" }

  let not_found = { status = 404; headers = []; body = "Not Found" }
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

let get pattern handler = Route { meth = `GET; pattern; handler }
let post pattern handler = Route { meth = `POST; pattern; handler }
let route meth pattern handler = Route { meth; pattern; handler }

(* Simple exact segment matching without GADTs *)
let get_ segments handler = ExactRoute { meth = `GET; segments; handler }
let post_ segments handler = ExactRoute { meth = `POST; segments; handler }

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
end
