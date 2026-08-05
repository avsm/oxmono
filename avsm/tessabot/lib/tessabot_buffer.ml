(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "tessabot.buffer" ~doc:"Buffer API client"
module Log = (val Logs.src_log src : Logs.LOG)

let api_url = "https://api.buffer.com"

type scheduling_type = Automatic | Notification

type post_mode = Share_now | Custom_schedule | Share_next

type create_post_input = {
  text : string;
  channel_id : string;
  service : string;
  scheduling_type : scheduling_type;
  mode : post_mode;
  due_at : Ptime.t option;
  image_urls : string list;
  link_attachment : string option;
}

type post_result = {
  id : string;
  text : string;
}

type channel = {
  id : string;
  name : string;
  service : string;
}

type organization = {
  id : string;
}

let scheduling_type_str = function
  | Automatic -> "automatic"
  | Notification -> "notification"

let post_mode_str = function
  | Share_now -> "shareNow"
  | Custom_schedule -> "customSchedule"
  | Share_next -> "shareNext"

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf {|\"|}
    | '\\' -> Buffer.add_string buf {|\\|}
    | '\n' -> Buffer.add_string buf {|\n|}
    | '\r' -> Buffer.add_string buf {|\r|}
    | '\t' -> Buffer.add_string buf {|\t|}
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let escape_graphql_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf {|\"|}
    | '\\' -> Buffer.add_string buf {|\\|}
    | '\n' -> Buffer.add_string buf {|\n|}
    | '\r' -> Buffer.add_string buf {|\r|}
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* GraphQL query builders *)

let create_post_query =
  {|mutation CreatePost($input: CreatePostInput!) {
  createPost(input: $input) {
    ... on PostActionSuccess {
      post {
        id
        text
      }
    }
    ... on MutationError {
      message
    }
  }
}|}

let create_post_graphql input =
  let due_at_str = match input.due_at with
    | Some t ->
      let (y, m, d), ((hh, mm, ss), _tz) = Ptime.to_date_time t in
      Fmt.str {|, "dueAt": "%04d-%02d-%02dT%02d:%02d:%02d.000Z"|} y m d hh mm ss
    | None -> ""
  in
  let assets_str = match input.image_urls with
    | [] -> ""
    | urls ->
      let imgs = List.map (fun url ->
        Fmt.str {|{"url": "%s"}|} (escape_json_string url)
      ) urls in
      Fmt.str {|, "assets": {"images": [%s]}|} (String.concat ", " imgs)
  in
  let metadata_str = match input.link_attachment with
    | None -> ""
    | Some url ->
      let link = Fmt.str {|{"url": "%s"}|} (escape_json_string url) in
      let svc = String.lowercase_ascii input.service in
      (* Only services that support linkAttachment *)
      let meta_value = match svc with
        | "bluesky" -> Some (Fmt.str {|{"linkAttachment": %s}|} link)
        | "linkedin" -> Some (Fmt.str {|{"linkAttachment": %s}|} link)
        | "threads" -> Some (Fmt.str {|{"linkAttachment": %s}|} link)
        | _ -> None
      in
      match meta_value with
      | Some v -> Fmt.str {|, "metadata": {"%s": %s}|} svc v
      | None -> ""
  in
  let variables = Fmt.str
    {|{"input": {"text": "%s", "channelId": "%s", "schedulingType": "%s", "mode": "%s"%s%s%s}}|}
    (escape_json_string input.text)
    (escape_json_string input.channel_id)
    (scheduling_type_str input.scheduling_type)
    (post_mode_str input.mode)
    due_at_str
    assets_str
    metadata_str
  in
  (create_post_query, variables)

let organizations_query =
  {|query GetOrganizations {
  account {
    organizations {
      id
    }
  }
}|}

let organizations_graphql () = organizations_query

let channels_query =
  {|query GetChannels($input: ChannelsInput!) {
  channels(input: $input) {
    id
    name
    service
  }
}|}

let channels_graphql org_id =
  let variables = Fmt.str {|{"input": {"organizationId": "%s"}}|}
    (escape_json_string org_id) in
  (channels_query, variables)

(* HTTP helpers *)

let graphql_request ~session query variables =
  let body_str = Fmt.str {|{"query": "%s", "variables": %s}|}
    (escape_graphql_string query) variables
  in
  Log.debug (fun m -> m "POST %s@.Body: %s" api_url body_str);
  let resp = Requests.post session
    ~body:(Requests.Body.of_string Requests.Mime.json body_str)
    api_url
  in
  let status = Requests.Response.status_code resp in
  let body = Requests.Response.body resp |> Eio.Flow.read_all in
  Log.debug (fun m -> m "Response %d: %s" status body);
  if status >= 200 && status < 300 then
    Ok body
  else
    Error (Fmt.str "Buffer API error (HTTP %d): %s" status body)

(* Simple JSON extraction helpers — avoids needing a full JSON parser
   for the small response payloads *)

let extract_string_field json field =
  let pattern = Fmt.str {|"%s":"|} field in
  match String.split_on_char '"' json with
  | _ ->
    (* Find the field pattern and extract value after it *)
    let rec find_in parts = match parts with
      | [] -> None
      | k :: _ :: v :: rest when k = "" || true ->
        let full = String.concat "\"" (k :: "" :: v :: rest) in
        (match Astring.String.find_sub ~sub:pattern full with
         | None -> None
         | Some pos ->
           let start = pos + String.length pattern in
           let rest_str = String.sub full start (String.length full - start) in
           match String.index_opt rest_str '"' with
           | Some end_pos -> Some (String.sub rest_str 0 end_pos)
           | None -> None)
      | _ -> None
    in
    find_in [json]

let extract_string_field json field =
  let pattern = Fmt.str {|"%s"|} field in
  match Astring.String.find_sub ~sub:pattern json with
  | None -> None
  | Some pos ->
    (* Skip past the field name, colon, optional whitespace, and opening quote *)
    let after_field = pos + String.length pattern in
    let rest = String.sub json after_field (String.length json - after_field) in
    (* Find the colon then the opening quote *)
    match Astring.String.find_sub ~sub:{|"|} rest with
    | None -> None
    | Some q_pos ->
      let val_start = q_pos + 1 in
      let val_rest = String.sub rest val_start (String.length rest - val_start) in
      match String.index_opt val_rest '"' with
      | None -> None
      | Some end_pos -> Some (String.sub val_rest 0 end_pos)

(* API call implementations *)

let create_post ~session input =
  let query, variables = create_post_graphql input in
  match graphql_request ~session query variables with
  | Error _ as e -> e
  | Ok body ->
    (* Check for MutationError first *)
    if Astring.String.is_infix ~affix:"MutationError" body ||
       Astring.String.is_infix ~affix:{|"message"|} body then begin
      let msg = extract_string_field body "message"
        |> Option.value ~default:"Unknown error" in
      Error (Fmt.str "Buffer mutation error: %s" msg)
    end else begin
      let id = extract_string_field body "id"
        |> Option.value ~default:"unknown" in
      let text = extract_string_field body "text"
        |> Option.value ~default:input.text in
      Ok { id; text }
    end

let list_organizations ~session =
  let query = organizations_graphql () in
  match graphql_request ~session query "{}" with
  | Error _ as e -> e
  | Ok body ->
    (* Extract all "id" values from organizations array *)
    let rec extract_ids start acc =
      let sub = String.sub body start (String.length body - start) in
      match Astring.String.find_sub ~sub:{|"id"|} sub with
      | None -> List.rev acc
      | Some pos ->
        let abs_pos = start + pos in
        let after = String.sub body abs_pos (String.length body - abs_pos) in
        match extract_string_field after "id" with
        | None -> List.rev acc
        | Some id -> extract_ids (abs_pos + 4) ({ id } :: acc)
    in
    Ok (extract_ids 0 [])

let list_channels ~session ~org_id =
  let query, variables = channels_graphql org_id in
  match graphql_request ~session query variables with
  | Error _ as e -> e
  | Ok body ->
    (* Extract channel objects — look for "id", "name", "service" triples *)
    let rec extract_channels start acc =
      let remaining = String.length body - start in
      if remaining <= 0 then List.rev acc
      else
        let sub = String.sub body start remaining in
        match Astring.String.find_sub ~sub:{|"id"|} sub with
        | None -> List.rev acc
        | Some pos ->
          let abs_pos = start + pos in
          (* Extract a chunk around this id for field extraction *)
          let chunk_end = min (abs_pos + 500) (String.length body) in
          let chunk = String.sub body abs_pos (chunk_end - abs_pos) in
          let id = extract_string_field chunk "id"
            |> Option.value ~default:"" in
          let name = extract_string_field chunk "name"
            |> Option.value ~default:"" in
          let service = extract_string_field chunk "service"
            |> Option.value ~default:"" in
          if id <> "" then
            extract_channels (abs_pos + 4) ({ id; name; service } :: acc)
          else
            extract_channels (abs_pos + 4) acc
    in
    Ok (extract_channels 0 [])
