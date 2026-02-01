(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Repository record *)
type repo = {
  name : string;
  knot : string;
  description : string option;
  created_at : string;
  spindle : string option;
  private_ : bool;
}

let repo_jsont =
  Jsont.Object.map ~kind:"Repo"
    (fun name knot description created_at spindle private_opt ->
      let private_ = Option.value ~default:false private_opt in
      { name; knot; description; created_at; spindle; private_ })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "knot" Jsont.string ~enc:(fun r -> r.knot)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun r ->
      r.description)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun r -> r.created_at)
  |> Jsont.Object.opt_mem "spindle" Jsont.string ~enc:(fun r -> r.spindle)
  |> Jsont.Object.opt_mem "private" Jsont.bool ~enc:(fun r ->
      if r.private_ then Some true else None)
  |> Jsont.Object.finish

let pp_repo ppf r =
  Fmt.pf ppf "@[<v>%s@,  knot: %s%a%a@]" r.name r.knot
    (Fmt.option (fun ppf d -> Fmt.pf ppf "@,  description: %s" d))
    r.description
    (fun ppf p -> if p then Fmt.pf ppf "@,  (private)" else ())
    r.private_

(* Repository info from knot - type compatible with generated tangled-lexicons *)
type language = {
  name : string;
  size : int;
  percentage : int;
  file_count : int option;
  color : string option;
  extensions : string list option;
}

let language_jsont =
  Jsont.Object.map ~kind:"Language"
    (fun name size percentage file_count color extensions ->
      { name; size; percentage; file_count; color; extensions })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun r -> r.name)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun r -> r.size)
  |> Jsont.Object.mem "percentage" Jsont.int ~enc:(fun r -> r.percentage)
  |> Jsont.Object.opt_mem "fileCount" Jsont.int ~enc:(fun r -> r.file_count)
  |> Jsont.Object.opt_mem "color" Jsont.string ~enc:(fun r -> r.color)
  |> Jsont.Object.opt_mem "extensions" (Jsont.list Jsont.string) ~enc:(fun r ->
      r.extensions)
  |> Jsont.Object.finish

type repo_info = { default_branch : string; languages : language list }

let pp_language ppf lang =
  Fmt.pf ppf "%s: %d bytes (%d%%)" lang.name lang.size lang.percentage

let pp_repo_info ppf info =
  Fmt.pf ppf "@[<v>Default branch: %s@,Languages: %a@]" info.default_branch
    (Fmt.list ~sep:Fmt.comma pp_language)
    info.languages

(* AT URI handling *)
type at_uri = { did : string; collection : string; rkey : string }

let parse_at_uri uri =
  if not (String.starts_with ~prefix:"at://" uri) then None
  else
    let rest = String.sub uri 5 (String.length uri - 5) in
    match String.split_on_char '/' rest with
    | [ did; collection; rkey ] -> Some { did; collection; rkey }
    | _ -> None

let make_at_uri ~did ~collection ~rkey =
  "at://" ^ did ^ "/" ^ collection ^ "/" ^ rkey

let pp_at_uri ppf uri =
  Fmt.pf ppf "at://%s/%s/%s" uri.did uri.collection uri.rkey

(* List records response *)
type 'a list_response = { records : (string * 'a) list; cursor : string option }

(* Raw record type for extracting uri before filtering *)
type raw_record = { uri : string; value : Jsont.json }

let raw_record_jsont =
  Jsont.Object.map ~kind:"RawRecord" (fun uri value -> { uri; value })
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun r -> r.value)
  |> Jsont.Object.finish

(* Filtering list response - skips records that fail to parse *)
let list_response_jsont value_jsont =
  (* First parse records as raw json *)
  let raw_response_jsont =
    Jsont.Object.map ~kind:"RawListResponse" (fun records cursor ->
        (records, cursor))
    |> Jsont.Object.mem "records" (Jsont.list raw_record_jsont)
         ~enc:(fun (r, _) -> r)
    |> Jsont.Object.opt_mem "cursor" Jsont.string ~enc:(fun (_, c) -> c)
    |> Jsont.Object.finish
  in
  Jsont.map ~kind:"ListResponse"
    ~dec:(fun (raw_records, cursor) ->
      (* Filter and parse each record individually *)
      let records =
        List.filter_map
          (fun raw ->
            match Jsont.Json.decode value_jsont raw.value with
            | Ok value ->
                let rkey =
                  match parse_at_uri raw.uri with
                  | Some u -> u.rkey
                  | None -> raw.uri
                in
                Some (rkey, value)
            | Error _ -> None)
          raw_records
      in
      { records; cursor })
    ~enc:(fun _ -> failwith "encoding list_response not supported")
    raw_response_jsont

(* Create record response *)
type create_response = { uri : string; cid : string }

let create_response_jsont =
  Jsont.Object.map ~kind:"CreateResponse" (fun uri cid -> { uri; cid })
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun r -> r.uri)
  |> Jsont.Object.mem "cid" Jsont.string ~enc:(fun r -> r.cid)
  |> Jsont.Object.finish

(* Service auth response *)
type service_auth_response = { token : string }

let service_auth_response_jsont =
  Jsont.Object.map ~kind:"ServiceAuthResponse" (fun token -> { token })
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun r -> r.token)
  |> Jsont.Object.finish

(* Resolve handle response *)
type resolve_handle_response = { did : string }

let resolve_handle_response_jsont =
  Jsont.Object.map ~kind:"ResolveHandleResponse" (fun did -> { did })
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun r -> r.did)
  |> Jsont.Object.finish
