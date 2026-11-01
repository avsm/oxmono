(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
type version = {
  capture : Memento.capture; original_uri : string;
  status : int option; media_type : string;
}
let invalid message = Jsont.Error.msg Jsont.Meta.none message
let datetime timestamp =
  if String.length timestamp <> 14 || not (String.for_all
      (function '0'..'9' -> true | _ -> false) timestamp) then
    invalid "CDX timestamp must contain fourteen digits";
  let sub = String.sub timestamp in
  let date = Printf.sprintf "%s-%s-%sT%s:%s:%sZ"
    (sub 0 4) (sub 4 2) (sub 6 2) (sub 8 2) (sub 10 2) (sub 12 2) in
  match Memento.Datetime.of_json date with
  | Ok t -> t | Error e -> invalid e
let http_url url =
  match Fetch.Middleware.Url.of_string url with
  | Ok url -> Ok (Fetch.Middleware.Url.to_string url)
  | Error _ -> Error "Expected an absolute HTTP(S) URL without credentials"
let column name header =
  let rec find i found = function
    | [] -> (match found with Some i -> i | None -> invalid ("Missing CDX column: " ^ name))
    | h :: rest when h = name ->
        (match found with Some _ -> invalid ("Repeated CDX column: " ^ name)
         | None -> find (i + 1) (Some i) rest)
    | _ :: rest -> find (i + 1) found rest in
  find 0 None header
let rows fields make = function
  | [] -> []
  | header :: records ->
      let indexes = List.map (fun name -> column name header) fields in
      List.map (fun row ->
        if List.length row <> List.length header then invalid "CDX row width differs from header";
        make (List.map (List.nth row) indexes)) records
let versions_jsont = Jsont.map ~dec:(rows
    ["timestamp"; "original"; "statuscode"; "mimetype"] (function
      | [timestamp; original_uri; code; media_type] ->
          let original_uri = match http_url original_uri with
            | Ok s -> s | Error e -> invalid e in
          let status = if code = "-" then None else
            match int_of_string_opt code with
            | Some n when n >= 100 && n <= 599 -> Some n
            | _ -> invalid "Invalid CDX HTTP status" in
          let capture = Memento.{ datetime = datetime timestamp;
            uri = "https://web.archive.org/web/" ^ timestamp ^ "/" ^ original_uri } in
          { capture; original_uri; status; media_type }
      | _ -> assert false)) (Jsont.list (Jsont.list Jsont.string))
let urls_jsont = Jsont.map ~dec:(rows ["original"] (function
    | [url] -> (match http_url url with Ok s -> s | Error e -> invalid e)
    | _ -> assert false)) (Jsont.list (Jsont.list Jsont.string))
let query client codec params =
  let uri = Httpz_uri.of_string_exn "https://web.archive.org/cdx/search/cdx"
    |> fun uri -> Httpz_uri.set_query_params uri (("output", "json") :: params) in
  Fetch.with_response
    ~headers:Fetch.Header.[user_agent, "memento/0.1.0"; raw "Accept" "application/json"]
    client `GET (Httpz_uri.to_string uri) (fun response ->
      if Fetch.status response <> 200 then
        Error (Printf.sprintf "Wayback returned HTTP %d" (Fetch.status response))
      else try Ok (Fetch.decode ~limit:(4 * 1024 * 1024) (Fetch.Json.v codec) response)
        with Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) ->
          Error (Fetch.Media.error_to_string error))
let validate_limit limit =
  if limit < 1 || limit > 10000 then Error "limit must be between 1 and 10000"
  else Ok ()
let bound name = function
  | None -> Ok []
  | Some s when String.length s >= 1 && String.length s <= 14 &&
      String.for_all (function '0'..'9' -> true | _ -> false) s -> Ok [name, s]
  | Some _ -> Error (name ^ " must contain one to fourteen timestamp digits")
let versions ?from ?until ?(latest = false) ?(limit = 100) client url =
  let ( let* ) = Result.bind in
  let* () = validate_limit limit in
  let* url = http_url url in
  let* from = bound "from" from in
  let* until = bound "to" until in
  let* values = query client versions_jsont
    (["url", url; "matchType", "exact";
      "fl", "timestamp,original,statuscode,mimetype";
      "limit", string_of_int (if latest then -limit else limit)] @ from @ until) in
  if List.length values > limit then Error "Wayback exceeded the requested row limit"
  else Ok values
let urls ?(limit = 100) client prefix =
  let ( let* ) = Result.bind in
  let* () = validate_limit limit in
  let* url = http_url prefix in
  let* values = query client urls_jsont
    ["url", url; "matchType", "prefix"; "collapse", "urlkey";
     "fl", "original"; "limit", string_of_int limit] in
  if List.length values > limit then Error "Wayback exceeded the requested row limit"
  else Ok values
