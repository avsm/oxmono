(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cache = Cache
module Logger = Logger
module P = Proffer

type mapping = { prefix : string; upstream : string }
type state = { cache : Cache.t; mappings : mapping list; allow_query : bool }

let upstream m = m.upstream

let mapping ~prefix ~upstream =
  if
    prefix = ""
    || prefix.[0] <> '/'
    || String.contains prefix '?'
    || (prefix <> "/" && String.ends_with ~suffix:"/" prefix)
  then
    invalid_arg
      "Proxy: prefix must be an absolute path without a trailing slash";
  if
    (not
       (String.starts_with ~prefix:"https://" upstream
       || String.starts_with ~prefix:"http://" upstream))
    || String.ends_with ~suffix:"/" upstream
    || String.contains upstream '?'
    || String.contains upstream '#'
  then
    invalid_arg
      "Proxy: upstream must be an HTTP URL without a query or trailing slash";
  { prefix; upstream }

let create_state ~cache ~allow_query mappings =
  if mappings = [] then invalid_arg "Proxy: at least one mapping is required";
  {
    cache;
    mappings =
      List.sort
        (fun a b -> compare (String.length b.prefix) (String.length a.prefix))
        mappings;
    allow_query;
  }

let find_map t path =
  List.find_map
    (fun m ->
      if m.prefix = "/" then Some (m, path)
      else if path = m.prefix then Some (m, "")
      else if String.starts_with ~prefix:(m.prefix ^ "/") path then
        Some
          ( m,
            String.sub path (String.length m.prefix)
              (String.length path - String.length m.prefix) )
      else None)
    t.mappings

let parse_range s =
  match Fetch.Header.decode Fetch.Header.range s with
  | Some { unit = "bytes"; ranges = [ `Range (start, stop) ] }
    when start >= 0L && start <= Int64.of_int max_int ->
      let off = Int64.to_int start in
      let len =
        match stop with
        | None -> None
        | Some last when last >= start && last < Int64.of_int max_int ->
            Some (Int64.to_int (Int64.sub last start) + 1)
        | _ -> invalid_arg "Proxy: invalid range end"
      in
      Cache.From (off, len)
  | Some { unit = "bytes"; ranges = [ `Suffix n ] }
    when n > 0L && n <= Int64.of_int max_int ->
      Cache.Suffix (Int64.to_int n)
  | _ -> invalid_arg "Proxy: use one byte range per request"

let cors =
  [
    ("Access-Control-Allow-Origin", "*");
    ( "Access-Control-Expose-Headers",
      "Content-Range, Content-Length, ETag, X-Cache, X-Cache-Complete" );
    ("Accept-Ranges", "bytes");
  ]

let rec safe_segments (segments @ local) =
  match segments with
  | [] -> true
  | s :: rest ->
      let s = P.Req.globalize s in
      s <> "." && s <> ".."
      && (not (String.contains s '/'))
      && (not (String.contains s '\\'))
      && (not (String.contains s '\000'))
      && safe_segments rest

type response =
  | Text of Httpz.Res.status * (string * string) list * string
  | Object of
      Cache.entry
      * Cache.range
      * Httpz.Res.status
      * (string * string) list
      * int

let prepare t (req @ local) =
  let path = P.Req.globalize (P.Req.path req) in
  match find_map t path with
  | None -> Text (Httpz.Res.Not_found, cors, "No upstream mapping.\n")
  | Some (m, suffix) -> (
      let target = P.Req.globalize (P.Req.target req) in
      let query =
        match String.index_opt target '?' with
        | None -> ""
        | Some i -> String.sub target i (String.length target - i)
      in
      let url = m.upstream ^ suffix ^ query in
      let head = P.Req.meth req = Httpz.Method.Head in
      let range = P.Req.header req Httpz.Header_name.Range in
      let selection =
        if head then Cache.Whole
        else
          match range with
          | None -> Cache.Whole
          | Some s -> parse_range (P.Req.globalize s)
      in
      if query <> "" && not t.allow_query then
        invalid_arg "Query strings are disabled for this cache";
      let result, outcome = Cache.get t.cache ~url in
      let cache_status =
        match outcome with
        | Cache.Hit -> "HIT"
        | Miss -> "MISS"
        | Shared -> "SHARED"
      in
      match result with
      | Cache.Missing status ->
          Text
            ( (if status = 410 then Httpz.Res.Gone else Httpz.Res.Not_found),
              cors @ [ ("X-Cache", cache_status) ],
              "Upstream object not found.\n" )
      | Cache.Found e ->
          let selection =
            match P.Req.header req Httpz.Header_name.If_range with
            | None -> selection
            | Some value ->
                let value = P.Req.globalize value in
                if
                  Cache.etag e = Some value
                  && not (String.starts_with ~prefix:"W/" value)
                then selection
                else Cache.Whole
          in
          let r = Cache.resolve (Cache.size e) selection in
          let fields =
            cors
            @ [ ("X-Cache", cache_status); ("X-Cache-Complete", "true") ]
            @ Option.fold ~none:[]
                ~some:(fun v -> [ ("ETag", v) ])
                (Cache.etag e)
            @ Option.fold ~none:[]
                ~some:(fun v -> [ ("Last-Modified", v) ])
                (Cache.modified e)
          in
          let status, fields =
            match selection with
            | Cache.Whole -> (Httpz.Res.Success, fields)
            | _ when head -> (Httpz.Res.Success, fields)
            | _ ->
                ( Httpz.Res.Partial_content,
                  fields
                  @ [
                      ( "Content-Range",
                        Printf.sprintf "bytes %d-%d/%d" r.start (r.stop - 1)
                          (Cache.size e) );
                    ] )
          in
          let length = if head then Cache.size e else r.stop - r.start in
          Object (e, r, status, fields, length))

let answer (respond @ local) = function
  | Text (status, fields, body) ->
      P.Resp.text respond ~status ~headers:(P.Headers.of_list fields) body
  | Object (entry, range, status, fields, length) ->
      P.Resp.stream respond ~status ~headers:(P.Headers.of_list fields)
        ~length:(Int64.of_int length) (Cache.content_type entry) (fun sink ->
          Cache.write entry range sink)

let prepared t (req @ local) =
  try prepare t req with
  | Cache.Unsatisfiable size ->
      Text
        ( Httpz.Res.Range_not_satisfiable,
          cors @ [ ("Content-Range", Printf.sprintf "bytes */%d" size) ],
          "Range outside object.\n" )
  | Invalid_argument message ->
      Text (Httpz.Res.Bad_request, cors, message ^ "\n")
  | Cache.Limit message ->
      Text (Httpz.Res.Service_unavailable, cors, message ^ "\n")
  | Eio.Time.Timeout ->
      Text (Httpz.Res.Gateway_timeout, cors, "Upstream download timed out.\n")
  | Cache.Upstream _ | Eio.Io _ ->
      Text
        ( Httpz.Res.Bad_gateway,
          cors,
          "Unable to retrieve the upstream object.\n" )

type t = {
  state : state;
  request : P.Req.t @ local -> P.Resp.respond @ local -> unit;
}

let create ~cache ?(allow_query = false) mappings =
  let state = create_state ~cache ~allow_query mappings in
  {
    state;
    request =
      (fun (req @ local) (respond @ local) ->
        answer respond (prepared state req));
  }

let object_key t target =
  let path =
    match String.index_opt target '?' with
    | None -> target
    | Some i -> String.sub target 0 i
  in
  match find_map t.state path with
  | None -> None
  | Some (m, suffix) ->
      let query =
        String.sub target (String.length path)
          (String.length target - String.length path)
      in
      if query <> "" && not t.state.allow_query then None
      else Some (Cache.key (m.upstream ^ suffix ^ query))

let site =
  P.Site.of_routes
    P.Route.
      [
        get rest (fun segments t req respond ->
            if not (safe_segments segments) then
              P.Resp.text respond ~status:Httpz.Res.Bad_request
                ~headers:(P.Headers.of_list cors) "Invalid object path.\n"
            else t.request req respond);
        route Httpz.Method.Options rest (fun _ _ _ respond ->
            P.Resp.text respond ~status:Httpz.Res.No_content
              ~headers:
                (P.Headers.of_list
                   (cors
                   @ [
                       ("Access-Control-Allow-Methods", "GET, HEAD, OPTIONS");
                       ("Access-Control-Allow-Headers", "Range, If-Range");
                     ]))
              "");
      ]
