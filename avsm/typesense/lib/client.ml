(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  client : Typesense.t;
  session : Session.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  profile : string option;
}

let create_with_session ~sw ~env ?requests_config ?profile ~session () =
  let fs = env#fs in
  let server_url = Session.server_url session in
  let api_key = Session.api_key session in
  (* Create a Requests session, optionally from cmdliner config *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  (* Set the X-TYPESENSE-API-KEY header *)
  let requests_session =
    Requests.set_default_header requests_session "X-TYPESENSE-API-KEY" api_key
  in
  let client = Typesense.create ~session:requests_session ~sw env ~base_url:server_url in
  { client; session; fs; profile }

let login ~sw ~env ?requests_config ?profile ~server_url ~api_key () =
  let fs = env#fs in
  (* Create session with API key header *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let requests_session =
    Requests.set_default_header requests_session "X-TYPESENSE-API-KEY" api_key
  in
  let client = Typesense.create ~session:requests_session ~sw env ~base_url:server_url in
  (* Validate by calling the health endpoint *)
  let health = Typesense.Health.health client () in
  if not (Typesense.Health.Status.ok health) then
    failwith "Health check failed - server not OK";
  (* Create and save session *)
  let session = Session.create ~server_url ~api_key () in
  Session.save fs ?profile session;
  (* Set as current profile if first login or explicitly requested *)
  let profiles = Session.list_profiles fs in
  let profile_name = Option.value ~default:Session.default_profile profile in
  if profiles = [] || Option.is_some profile then
    Session.set_current_profile fs profile_name;
  { client; session; fs; profile }

let resume ~sw ~env ?requests_config ?profile ~session () =
  create_with_session ~sw ~env ?requests_config ?profile ~session ()

let logout t =
  Session.clear t.fs ?profile:t.profile ()

let client t = t.client
let session t = t.session
let profile t = t.profile
let fs t = t.fs

(** {1 JSONL Helpers} *)

let parse_jsonl codec response =
  String.split_on_char '\n' response
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" then None
         else
           match Jsont_bytesrw.decode_string codec line with
           | Ok result -> Some result
           | Error _ -> None)

let to_json_string codec value =
  match Jsont_bytesrw.encode_string' codec value with
  | Ok s -> s
  | Error e -> failwith ("JSON encoding error: " ^ Jsont.Error.to_string e)

(** {1 JSONL Import/Export} *)

type import_action = Create | Upsert | Update | Emplace

let action_to_string = function
  | Create -> "create"
  | Upsert -> "upsert"
  | Update -> "update"
  | Emplace -> "emplace"

type import_result = {
  success : bool;
  error : string option;
  document : string option;
}

let import_result_jsont =
  let make success error document = { success; error; document } in
  Jsont.Object.map ~kind:"ImportResult" make
  |> Jsont.Object.mem "success" Jsont.bool ~enc:(fun r -> r.success)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun r -> r.error)
  |> Jsont.Object.opt_mem "document" Jsont.string ~enc:(fun r -> r.document)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let import t ~collection ?(action = Upsert) ?(batch_size = 40)
    ?(return_doc = false) ?(return_id = false) documents =
  let base_url = Typesense.base_url t.client in
  let session = Typesense.session t.client in
  (* Build URL with query params *)
  let params =
    [
      ("action", action_to_string action);
      ("batch_size", string_of_int batch_size);
      ("return_doc", string_of_bool return_doc);
      ("return_id", string_of_bool return_id);
    ]
  in
  let query_string =
    params
    |> List.map (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
    |> String.concat "&"
  in
  let url =
    base_url ^ "/collections/" ^ Uri.pct_encode collection ^ "/documents/import?" ^ query_string
  in
  (* Convert documents to JSONL format *)
  let body =
    documents
    |> List.map (fun doc -> to_json_string Jsont.json doc)
    |> String.concat "\n"
  in
  let response = Requests.post session ~body:(Requests.Body.text body) url in
  if not (Requests.Response.ok response) then
    failwith ("Import failed: " ^ string_of_int (Requests.Response.status_code response));
  parse_jsonl import_result_jsont (Requests.Response.text response)

type export_params = {
  filter_by : string option;
  include_fields : string list option;
  exclude_fields : string list option;
}

let export_params ?filter_by ?include_fields ?exclude_fields () =
  { filter_by; include_fields; exclude_fields }

let export t ~collection ?params () =
  let base_url = Typesense.base_url t.client in
  let session = Typesense.session t.client in
  let query_params =
    match params with
    | None -> []
    | Some p ->
        List.filter_map Fun.id
          [
            Option.map (fun v -> ("filter_by", v)) p.filter_by;
            Option.map
              (fun v -> ("include_fields", String.concat "," v))
              p.include_fields;
            Option.map
              (fun v -> ("exclude_fields", String.concat "," v))
              p.exclude_fields;
          ]
  in
  let url =
    base_url ^ "/collections/" ^ Uri.pct_encode collection ^ "/documents/export"
  in
  let response = Requests.get session ~params:query_params url in
  if not (Requests.Response.ok response) then
    failwith ("Export failed: " ^ string_of_int (Requests.Response.status_code response));
  parse_jsonl Jsont.json (Requests.Response.text response)
