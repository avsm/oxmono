(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Manifest = Manifest
module Cache = Cache
module Coverage = Coverage
module Shard = Shard

module P = Proffer
module R = Coverage

type mapping = { prefix : string; upstream : string }
type state = {
  cache : Cache.t;
  mappings : mapping list;
  layouts : (string, Shard.t option) Hashtbl.t;
  indexes : (string, string * R.t list) Hashtbl.t;
}

let mapping ~prefix ~upstream =
  if prefix = "" || prefix.[0] <> '/' || String.contains prefix '?'
      || (prefix <> "/" && String.ends_with ~suffix:"/" prefix) then
    invalid_arg "Proxy: prefix must be an absolute path without a trailing slash";
  if not (String.starts_with ~prefix:"https://" upstream
          || String.starts_with ~prefix:"http://" upstream)
      || String.ends_with ~suffix:"/" upstream || String.contains upstream '?'
      || String.contains upstream '#' then
    invalid_arg "Proxy: upstream must be an HTTP URL without a query or trailing slash";
  { prefix; upstream }

let create_state ~cache mappings =
  if mappings = [] then invalid_arg "Proxy: at least one mapping is required";
  { cache; mappings = List.sort (fun a b ->
        compare (String.length b.prefix) (String.length a.prefix)) mappings;
    layouts = Hashtbl.create 64; indexes = Hashtbl.create 64 }

let find_map t path =
  List.find_map (fun m ->
      if m.prefix = "/" then Some (m, path)
      else if path = m.prefix then Some (m, "")
      else if String.starts_with ~prefix:(m.prefix ^ "/") path then
        Some (m, String.sub path (String.length m.prefix)
                (String.length path - String.length m.prefix))
      else None) t.mappings

let read_json cache url =
  match Cache.cached cache ~url with
  | Some e when Cache.complete e && Cache.size e <= 32 * 1024 * 1024 ->
      (match Jsont_bytesrw.decode_string Jsont.json
               (Cache.read e (R.v 0 (Cache.size e))) with
       | Ok j -> Some j
       | Error message -> raise (Cache.Upstream ("Invalid Zarr metadata: " ^ message)))
  | _ -> None

let array_url url =
  if String.contains url '?' then None else
  let parts = String.split_on_char '/' url in
  let rec loop prefix = function
    | part :: _ when part = "c" || String.starts_with ~prefix:"c." part ->
        Some (String.concat "/" (List.rev prefix))
    | x :: xs -> loop (x :: prefix) xs
    | [] -> None in
  loop [] parts

(* Readers ordinarily fetch metadata before data. Consult their cached
   metadata rather than adding a network lookup just to classify a range. *)
let layout t m url =
  match array_url url with
  | None -> None
  | Some array ->
      match Hashtbl.find_opt t.layouts array with
      | Some layout -> layout
      | None ->
          let json = match read_json t.cache (array ^ "/zarr.json") with
            | Some j -> Some j
            | None ->
                match read_json t.cache (m.upstream ^ "/zarr.json") with
                | None -> None
                | Some root ->
                    match Zarrz.Metadata.group_of_json root with
                    | Error _ -> None
                    | Ok g ->
                        let relative = String.sub array (String.length m.upstream + 1)
                            (String.length array - String.length m.upstream - 1) in
                        Option.bind (Zarrz.Consolidated.of_group g)
                          (fun c -> Zarrz.Consolidated.node c relative) in
          match json with
          | None -> None
          | Some j ->
              let layout = Shard.of_json j in
              Hashtbl.replace t.layouts array layout;
              layout

let prepare_raw t m url ~head selection =
  let was_cached = match Cache.cached t.cache ~url with
    | Some e when head -> true
    | Some e -> (try Cache.covered e (Cache.resolve (Cache.size e) selection)
                 with Cache.Unsatisfiable _ -> true)
    | None -> false in
  match Cache.ensure t.cache ~url ~head_only:head selection with
  | None -> None
  | Some (e, requested) when head || was_cached -> Some (e, requested, was_cached)
  | Some (e, requested) ->
      let e = match layout t m url with
        | None -> e
        | Some layout ->
            let size = Cache.size e in
            let index = Shard.index_range layout ~size in
            let indexed, chunks =
              match Hashtbl.find_opt t.indexes url with
              | Some (revision, chunks) when revision = Cache.revision e -> e, chunks
              | _ ->
                  let indexed = Cache.ensure t.cache ~url ~head_only:false
                      (Cache.From (index.start, Some (index.stop - index.start))) in
                  let indexed = match indexed with
                    | Some (ie, _) when Cache.size ie = size
                        && Cache.revision ie = Cache.revision e -> ie
                    | _ -> raise (Cache.Upstream "Shard changed while reading index") in
                  let chunks = Shard.ranges layout ~size (Cache.read indexed index) in
                  if Hashtbl.length t.indexes >= 64 then Hashtbl.clear t.indexes;
                  Hashtbl.replace t.indexes url (Cache.revision indexed, chunks);
                  indexed, chunks in
            List.fold_left (fun _ r ->
                match Cache.ensure t.cache ~url ~head_only:false
                        (Cache.From (r.R.start, Some (r.stop - r.start))) with
                | Some (ce, _) when Cache.size ce = size
                    && Cache.revision ce = Cache.revision e -> ce
                | _ -> raise (Cache.Upstream "Shard disappeared while reading chunks"))
              indexed (Shard.expand chunks requested) in
      Some (e, requested, false)

let prepare t m url ~head selection =
  try prepare_raw t m url ~head selection with
  | Invalid_argument message -> raise (Cache.Upstream message)
  | Zarrz.Error.E e -> raise (Cache.Upstream (Zarrz.Error.to_string e))

let parse_range s =
  match Fetch.Header.decode Fetch.Header.range s with
  | Some { unit = "bytes"; ranges = [ `Range (start, stop) ] }
      when start >= 0L && start <= Int64.of_int max_int ->
      let off = Int64.to_int start in
      let len = match stop with
        | None -> None
        | Some last when last >= start && last < Int64.of_int max_int ->
            Some (Int64.to_int (Int64.sub last start) + 1)
        | _ -> invalid_arg "Proxy: invalid range end" in
      Cache.From (off, len)
  | Some { unit = "bytes"; ranges = [ `Suffix n ] } when n > 0L && n <= Int64.of_int max_int ->
      Cache.Suffix (Int64.to_int n)
  | _ -> invalid_arg "Proxy: use one byte range per request"

let cors =
  [ "Access-Control-Allow-Origin", "*";
    "Access-Control-Expose-Headers", "Content-Range, Content-Length, ETag, X-Cache, X-Cache-Complete";
    "Accept-Ranges", "bytes" ]

let rec safe_segments (segments @ local) =
  match segments with
  | [] -> true
  | s :: rest ->
      let s = P.Req.globalize s in
      s <> "." && s <> ".." && not (String.contains s '/')
      && not (String.contains s '\\') && not (String.contains s '\000')
      && safe_segments rest

let handler t (req @ local) (respond @ local) =
  let path = P.Req.globalize (P.Req.path req) in
  match find_map t path with
  | None -> P.Resp.text respond ~status:Httpz.Res.Not_found
      ~headers:(P.Headers.of_list cors) "No upstream mapping.\n"
  | Some (m, suffix) ->
      let target = P.Req.globalize (P.Req.target req) in
      let query = match String.index_opt target '?' with
        | None -> "" | Some i -> String.sub target i (String.length target - i) in
      let url = m.upstream ^ suffix ^ query in
      let head = P.Req.meth req = Httpz.Method.Head in
      let range = P.Req.header req Httpz.Header_name.Range in
      let selection = if head then Cache.Whole else
        match range with None -> Cache.Whole
        | Some s -> parse_range (P.Req.globalize s) in
      let selection = match P.Req.header req Httpz.Header_name.If_range with
        | None -> selection
        | Some value ->
            let value = P.Req.globalize value in
            match Cache.cached t.cache ~url with
            | Some e when Cache.etag e = Some value
                && not (String.starts_with ~prefix:"W/" value) -> selection
            | _ -> Cache.Whole in
      match prepare t m url ~head selection with
      | None -> P.Resp.text respond ~status:Httpz.Res.Not_found
          ~headers:(P.Headers.of_list cors) "Upstream object not found.\n"
      | Some (e, r, hit) ->
          if not hit && String.ends_with ~suffix:"/zarr.json" url then
            Hashtbl.clear t.layouts;
          let fields = cors
            @ [ "X-Cache", (if hit then "HIT" else "MISS");
                "X-Cache-Complete", string_of_bool (Cache.complete e) ]
            @ Option.fold ~none:[] ~some:(fun v -> ["ETag", v]) (Cache.etag e)
            @ Option.fold ~none:[] ~some:(fun v -> ["Last-Modified", v]) (Cache.modified e) in
          let status, fields = match selection with
            | Cache.Whole -> Httpz.Res.Success, fields
            | _ when head -> Httpz.Res.Success, fields
            | _ -> Httpz.Res.Partial_content, fields @ ["Content-Range",
                Printf.sprintf "bytes %d-%d/%d" r.start (r.stop - 1) (Cache.size e)] in
          let length = if head then Cache.size e else r.stop - r.start in
          P.Resp.stream respond ~status ~headers:(P.Headers.of_list fields)
            ~length:(Int64.of_int length) (Cache.content_type e)
            (fun sink -> Cache.write e r sink)

type t = {
  request : P.Req.t @ local -> P.Resp.respond @ local -> unit;
}

let create ~cache mappings =
  let state = create_state ~cache mappings in
  { request = (fun (req @ local) (respond @ local) ->
      try handler state req respond with
      | Eio.Io _ as exn -> raise (Cache.Upstream (Printexc.to_string exn))) }

let site =
  P.Site.of_routes P.Route.[
    get rest (fun segments t req respond ->
        if not (safe_segments segments) then
          P.Resp.text respond ~status:Httpz.Res.Bad_request
            ~headers:(P.Headers.of_list cors) "Invalid object path.\n"
        else try t.request req respond with
          | Cache.Unsatisfiable size ->
              P.Resp.text respond ~status:Httpz.Res.Range_not_satisfiable
                ~headers:(P.Headers.of_list (cors @ ["Content-Range",
                    Printf.sprintf "bytes */%d" size])) "Range outside object.\n"
          | Invalid_argument message ->
              P.Resp.text respond ~status:Httpz.Res.Bad_request
                ~headers:(P.Headers.of_list cors) (message ^ "\n")
          | Cache.Upstream message ->
              P.Resp.text respond ~status:Httpz.Res.Bad_gateway
                ~headers:(P.Headers.of_list cors) (message ^ "\n"));
    route Httpz.Method.Options rest (fun _ _ _ respond ->
        P.Resp.text respond ~status:Httpz.Res.No_content
          ~headers:(P.Headers.of_list (cors @
            [ "Access-Control-Allow-Methods", "GET, HEAD, OPTIONS";
              "Access-Control-Allow-Headers", "Range, If-Range" ])) "")
  ]
