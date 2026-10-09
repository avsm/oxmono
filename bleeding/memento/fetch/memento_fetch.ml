(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
type discovery = {
  url : string; status : int; metadata : Memento.Headers.metadata;
  capture : Memento.capture option;
}
let resolve ~base uri =
  match Fetch.Middleware.Url.of_string base with
  | Error e -> Error e
  | Ok base -> Result.map Fetch.Middleware.Url.to_string
      (Fetch.Middleware.Url.resolve ~base uri)
let metadata response =
  match Memento.Headers.read (Fetch.headers response) with
  | Error e -> Error e
  | Ok metadata ->
      let url = Fetch.url response in
      let capture_uri =
        if not metadata.is_memento then Ok None
        else
          let raw = Http.Header.get_multi (Fetch.headers response) "content-location" in
          match Fetch.header Fetch.Header.content_location response with
          | None when raw <> [] -> Error "Malformed or repeated Content-Location"
          | Some uri -> Result.map Option.some (resolve ~base:url uri)
          | None -> Ok (if metadata.is_timegate then None else Some url)
      in
      Result.map (fun uri ->
        let capture = match uri, metadata.datetime with
          | Some uri, Some datetime -> Some Memento.{ uri; datetime }
          | _ -> None in
        { url; status = Fetch.status response; metadata; capture }) capture_uri
let discover ?(redirects = 5) client url =
  Fetch.with_response ~redirects client `HEAD url metadata
let negotiate ?(redirects = 5) client ~datetime timegate =
  Fetch.with_response ~redirects
    ~headers:Fetch.Header.[Memento.Headers.accept_datetime, datetime]
    client `HEAD timegate (fun response ->
      match metadata response with
      | Error e -> Error e
      | Ok d when d.status < 200 || d.status >= 300 ->
          Error (Printf.sprintf "TimeGate returned HTTP %d" d.status)
      | Ok d when d.metadata.do_not_negotiate ->
          Error "Resource excludes datetime negotiation"
      | Ok d when not d.metadata.is_memento ->
          Error "TimeGate did not select a Memento"
      | Ok d -> Ok d)
let find ?redirects ?timegate client ~datetime original =
  match timegate with
  | Some uri -> negotiate ?redirects client ~datetime uri
  | None ->
      match discover ?redirects client original with
      | Error e -> Error e
      | Ok d when d.metadata.do_not_negotiate ->
          Error "Resource excludes datetime negotiation"
      | Ok d ->
          if d.metadata.is_timegate then negotiate ?redirects client ~datetime d.url
          else match Fetch.Header.link_rel "timegate" d.metadata.links with
            | None -> Error "No TimeGate advertised. Supply an archive endpoint"
            | Some link -> match resolve ~base:d.url link.target with
              | Error e -> Error e
              | Ok uri -> negotiate ?redirects client ~datetime uri

type timemap = Json of Memento.Timemap.t | Links of Memento.Link.t list
let json_codec = Fetch.Json.v ~accept:[] Memento.Timemap.jsont
let timemap ?(limit = 16 * 1024 * 1024) ?(redirects = 5) client uri =
  if limit < 0 then invalid_arg "Memento_fetch.timemap: negative limit";
  Fetch.with_response ~redirects
    ~headers:Fetch.Header.[raw "Accept" "application/link-format, application/json"]
    client `GET uri (fun response ->
      if Fetch.status response <> 200 then
        Error (Printf.sprintf "TimeMap returned HTTP %d" (Fetch.status response))
      else
        let media = Fetch.header Fetch.Header.content_type response in
        try match media with
        | Some { media = "application/json"; _ } ->
            Ok (Json (Fetch.decode ~limit json_codec response))
        | Some { media = "application/link-format"; _ } ->
            Ok (Links (Fetch.decode ~limit Memento.Link.timemap_media response))
        | _ -> Error "Unsupported TimeMap Content-Type"
        with Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) ->
          Error (Fetch.Media.error_to_string error))
let captures = function
  | Json t -> Ok (Memento.Timemap.captures t)
  | Links links -> Memento.Link.captures links
