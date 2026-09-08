module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Int_set = Set.Make (Int)

module Persisted_node_set = Set.Make (struct
  type t = string * int

  let compare = compare
end)

module String_map = Map.Make (String)
module String_set = Set.Make (String)

let src = Logs.Src.create "matrix.send_queue" ~doc:"Matrix offline send queue"

module Log = (val Logs.src_log src : Logs.LOG)

type kind =
  | Event of { event_type : string; content : Jsont.json }
  | Reaction of { relates_to : Id.Event_id.t; key : string }
  | Redaction of { event_id : Id.Event_id.t; reason : string option }
  | Upload_request of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Encrypted_attachment.metadata option;
    }
  | Attachment of {
      content : Jsont.json;
      original_upload : int;
      thumbnail_upload : int option;
    }

(* The sentinel is private to persistence. A malformed record must not become
   an ordinary event, but it also must not make the whole queue unreadable.
   Invalid attachments retain their upload references so restore can quarantine
   the generated parent graph rather than uploading now-orphaned bytes. *)
type persisted_kind =
  | Persisted_kind of kind
  | Invalid_kind_state of { attachment_uploads : int list }

type attachment_upload = {
  content_type : string;
  data : string;
  filename : string option;
  encrypted_metadata : Encrypted_attachment.metadata option;
}

type upload_result =
  | Clear_upload of { mxc : Media.Mxc.t }
  | Encrypted_upload of {
      mxc : Media.Mxc.t;
      metadata : Encrypted_attachment.metadata;
    }

(* Decode this private persistence field through [Jsont.json], so one malformed
   transitional result invalidates only its upload graph rather than making the
   entire queue slot unreadable. *)
type persisted_upload_result =
  | Persisted_upload_result of upload_result
  | Invalid_upload_result

type dependency_result = Event_id of Id.Event_id.t | Upload of upload_result
type attachment_edit_result = Updated | Deferred | Already_sent
type pending_attachment_edit = { content : Jsont.json; txn_id : string }

type status =
  | Pending
  | Sending
  | Sent of Id.Event_id.t
  | Uploaded of upload_result
  | Wedged
  | Cancelled

type progress = { current_bytes : int64; total_bytes : int64 }

type request = {
  r_id : int;
  r_room_id : Id.Room_id.t;
  mutable r_kind : kind;
  mutable r_txn_id : string;
  r_created_at : Event.Timestamp.t;
  mutable r_status : status;
  mutable r_attempts : int;
  mutable r_last_error : Error.t option;
  mutable r_dependencies : int list;
  mutable r_resolved : (int * dependency_result) list;
  mutable r_extra_content : Jsont.json option;
  r_attachment_filename : string option;
  mutable r_pending_edit : pending_attachment_edit option;
  mutable r_cancel_requested : bool;
  mutable r_cancel_reason : string option;
  mutable r_cancel_txn_id : string option;
  (* A forgotten room detaches its requests before an in-flight transport has
     returned.  Detached requests are never persisted or turned into a
     compensating redaction; the flag remains set until that callback reaches
     its terminal branch. *)
  mutable r_detached : bool;
  (* Recorded before the media-cache transition.  This is the durable hand-off
     point that lets a restart finish a successful upload without reuploading. *)
  mutable r_upload_result : upload_result option;
  (* When present, the upload bytes live in [q_media_store] under this exact
     key. A live request keeps its inline payload for API compatibility, but
     persistence elides it and [send_one] never uses it for a cache-backed
     request. *)
  mutable r_upload_cache_key : Media_store.key option;
}

let id r = r.r_id
let room_id r = r.r_room_id
let dependencies r = r.r_dependencies
let dependency_results r = r.r_resolved

let resolved_dependencies r =
  List.filter_map
    (fun (id, result) ->
      match result with
      | Event_id event_id -> Some (id, event_id)
      | Upload _ -> None)
    r.r_resolved

let kind r = r.r_kind
let txn_id r = r.r_txn_id
let status r = r.r_status
let attempts r = r.r_attempts
let created_at r = r.r_created_at
let last_error r = r.r_last_error
let jstring = Jsont.Json.string

let jobject mems =
  Jsont.Json.object'
    (List.map (fun (n, v) -> Jsont.Json.mem (Jsont.Json.name n) v) mems)

let persisted_kind_value = function
  | Persisted_kind kind -> kind
  | Invalid_kind_state _ ->
      Event { event_type = "m.invalid"; content = jobject [] }

let kind_jsont : persisted_kind Jsont.t =
  Jsont.Object.(
    map
      (fun
        tag
        event_type
        content
        relates_to
        key
        event_id
        reason
        upload_role
        upload_content_type
        upload_filename
        upload_data
        upload_data_base64
        upload_metadata
        attachment_original
        attachment_thumbnail
      ->
        match tag with
        | "reaction" -> (
            match (relates_to, key) with
            | Some relates_to, Some key ->
                Persisted_kind (Reaction { relates_to; key })
            | _ -> Invalid_kind_state { attachment_uploads = [] })
        | "redaction" -> (
            match event_id with
            | Some event_id -> Persisted_kind (Redaction { event_id; reason })
            | None -> Invalid_kind_state { attachment_uploads = [] })
        | "upload" -> (
            let upload_data =
              match upload_data_base64 with
              | None -> upload_data
              | Some encoded ->
                  Result.to_option (Matrix_proto.Base64.decode encoded)
            in
            let metadata =
              match upload_metadata with
              | None -> `Absent
              | Some json -> (
                  match Encrypted_attachment.Metadata.of_json json with
                  | Ok metadata -> `Valid metadata
                  | Error _ -> `Invalid)
            in
            match (upload_role, upload_content_type, upload_data, metadata) with
            | Some "original", Some content_type, Some data, `Valid metadata ->
                Persisted_kind
                  (Upload_request
                     {
                       role = `Original;
                       content_type;
                       filename = upload_filename;
                       data;
                       encrypted_metadata = Some metadata;
                     })
            | Some "original", Some content_type, Some data, `Absent ->
                Persisted_kind
                  (Upload_request
                     {
                       role = `Original;
                       content_type;
                       filename = upload_filename;
                       data;
                       encrypted_metadata = None;
                     })
            | Some "thumbnail", Some content_type, Some data, `Valid metadata ->
                Persisted_kind
                  (Upload_request
                     {
                       role = `Thumbnail;
                       content_type;
                       filename = upload_filename;
                       data;
                       encrypted_metadata = Some metadata;
                     })
            | Some "thumbnail", Some content_type, Some data, `Absent ->
                Persisted_kind
                  (Upload_request
                     {
                       role = `Thumbnail;
                       content_type;
                       filename = upload_filename;
                       data;
                       encrypted_metadata = None;
                     })
            | _ -> Invalid_kind_state { attachment_uploads = [] })
        | "attachment" -> (
            let attachment_uploads =
              List.filter_map
                (function
                  | Some id when id > 0 -> Some id | Some _ | None -> None)
                [ attachment_original; attachment_thumbnail ]
              |> List.sort_uniq Int.compare
            in
            match attachment_original with
            | Some original
              when original > 0
                   && Option.for_all
                        (fun thumbnail ->
                          thumbnail > 0 && thumbnail <> original)
                        attachment_thumbnail -> (
                match content with
                | Jsont.Object _ ->
                    Persisted_kind
                      (Attachment
                         {
                           content;
                           original_upload = original;
                           thumbnail_upload = attachment_thumbnail;
                         })
                | _ -> Invalid_kind_state { attachment_uploads })
            | _ -> Invalid_kind_state { attachment_uploads })
        | "event" -> (
            match content with
            | Jsont.Object _ -> Persisted_kind (Event { event_type; content })
            | _ -> Invalid_kind_state { attachment_uploads = [] })
        | _ -> Invalid_kind_state { attachment_uploads = [] })
    |> mem "tag" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "event")
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Event _ -> "event"
           | Reaction _ -> "reaction"
           | Redaction _ -> "redaction"
           | Upload_request _ -> "upload"
           | Attachment _ -> "attachment")
    |> mem "event_type" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "m.room.message")
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Event e -> e.event_type
           | _ -> "m.room.message")
    |> mem "content" Matrix_proto.Json.Codec.json
         ~dec_absent:(fun () -> jobject [])
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Event e -> e.content
           | Attachment { content; _ } -> content
           | _ -> jobject [])
    |> opt_mem "relates_to" Id.Event_id.jsont ~enc:(fun value ->
        match persisted_kind_value value with
        | Reaction r -> Some r.relates_to
        | _ -> None)
    |> opt_mem "key" Matrix_proto.Json.Codec.string ~enc:(fun value ->
        match persisted_kind_value value with
        | Reaction r -> Some r.key
        | _ -> None)
    |> opt_mem "event_id" Id.Event_id.jsont ~enc:(fun value ->
        match persisted_kind_value value with
        | Redaction r -> Some r.event_id
        | _ -> None)
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:(fun value ->
        match persisted_kind_value value with
        | Redaction r -> r.reason
        | _ -> None)
    |> opt_mem "upload_role" Matrix_proto.Json.Codec.string ~enc:(fun value ->
        match persisted_kind_value value with
        | Upload_request { role = `Original; _ } -> Some "original"
        | Upload_request { role = `Thumbnail; _ } -> Some "thumbnail"
        | _ -> None)
    |> opt_mem "upload_content_type" Matrix_proto.Json.Codec.string
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Upload_request { content_type; _ } -> Some content_type
           | _ -> None)
    |> opt_mem "upload_filename" Matrix_proto.Json.Codec.string
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Upload_request { filename; _ } -> filename
           | _ -> None)
    (* [upload_data] was the original persistence field.  It can only represent
       UTF-8 safely, so retain it for old stores but write arbitrary upload
       bytes to an explicitly Base64-encoded field. *)
    |> opt_mem "upload_data" Matrix_proto.Json.Codec.string ~enc:(fun _ -> None)
    |> opt_mem "upload_data_base64" Matrix_proto.Json.Codec.string
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Upload_request { data; _ } ->
               Some (Matrix_proto.Base64.encode data)
           | _ -> None)
    |> opt_mem "upload_metadata" Matrix_proto.Json.Codec.json ~enc:(fun value ->
        match persisted_kind_value value with
        | Upload_request { encrypted_metadata = Some metadata; _ } ->
            Some (Encrypted_attachment.Metadata.to_json metadata)
        | _ -> None)
    |> opt_mem "attachment_original" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Attachment { original_upload; _ } -> Some original_upload
           | _ -> None)
    |> opt_mem "attachment_thumbnail" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun value ->
           match persisted_kind_value value with
           | Attachment { thumbnail_upload; _ } -> thumbnail_upload
           | _ -> None)
    |> finish)

type persisted = {
  p_id : int;
  p_room_id : Id.Room_id.t;
  p_kind : persisted_kind;
  p_txn_id : string;
  p_created_at : Event.Timestamp.t;
  p_attempts : int;
  p_wedged : bool;
  p_dependencies : int list;
  p_resolved : persisted_resolution list;
  p_extra_content : Jsont.json option;
  p_attachment_filename : string option;
  p_pending_edit_content : Jsont.json option;
  p_pending_edit_txn_id : string option;
  p_cancel_requested : bool;
  p_cancel_reason : string option;
  p_cancel_txn_id : string option;
  p_upload_cache_uri : string option;
  p_upload_result : persisted_upload_result option;
}

and persisted_resolution = {
  pr_id : int;
  pr_event_id : Id.Event_id.t option;
  pr_result : Jsont.json option;
}

let upload_result_jsont : upload_result Jsont.t =
  let wire_jsont =
    Jsont.Object.(
      map (fun kind mxc metadata -> (kind, mxc, metadata))
      |> mem "kind" Matrix_proto.Json.Codec.string
      |> mem "mxc" Media.Mxc.jsont
      |> opt_mem "metadata" Matrix_proto.Json.Codec.json
      |> finish)
  in
  Jsont.map
    ~dec:(fun json ->
      match Jsont.Json.decode wire_jsont json with
      | Error e -> Jsont.Error.msg Jsont.Meta.none e
      | Ok ("clear", mxc, None) -> Clear_upload { mxc }
      | Ok ("encrypted", mxc, Some metadata) -> (
          match Encrypted_attachment.Metadata.of_json metadata with
          | Ok metadata -> Encrypted_upload { mxc; metadata }
          | Error e ->
              Jsont.Error.msgf Jsont.Meta.none
                "invalid encrypted upload metadata: %a"
                Encrypted_attachment.pp_error e)
      | Ok (kind, _, _) ->
          Jsont.Error.msgf Jsont.Meta.none "invalid upload result kind %S" kind)
    ~enc:(fun result ->
      match result with
      | Clear_upload { mxc } ->
          jobject
            [
              ("kind", jstring "clear");
              ("mxc", Jsont.Json.string (Media.Mxc.to_string mxc));
            ]
      | Encrypted_upload { mxc; metadata } ->
          jobject
            [
              ("kind", jstring "encrypted");
              ("mxc", Jsont.Json.string (Media.Mxc.to_string mxc));
              ("metadata", Encrypted_attachment.Metadata.to_json metadata);
            ])
    Matrix_proto.Json.Codec.json

let upload_result_json result =
  match result with
  | Clear_upload { mxc } ->
      jobject
        [
          ("kind", jstring "clear"); ("mxc", jstring (Media.Mxc.to_string mxc));
        ]
  | Encrypted_upload { mxc; metadata } ->
      jobject
        [
          ("kind", jstring "encrypted");
          ("mxc", jstring (Media.Mxc.to_string mxc));
          ("metadata", Encrypted_attachment.Metadata.to_json metadata);
        ]

let persisted_upload_result_jsont : persisted_upload_result Jsont.t =
  Jsont.map
    ~dec:(fun json ->
      match Jsont.Json.decode upload_result_jsont json with
      | Ok result -> Persisted_upload_result result
      | Error _ -> Invalid_upload_result)
    ~enc:(function
      | Persisted_upload_result result -> upload_result_json result
      | Invalid_upload_result -> jobject [])
    Matrix_proto.Json.Codec.json

let dependency_result_json result =
  match result with
  | Event_id event_id ->
      jobject
        [
          ("kind", jstring "event");
          ("event_id", jstring (Id.Event_id.to_string event_id));
        ]
  | Upload result -> upload_result_json result

let dependency_result_jsont : dependency_result Jsont.t =
  let wire_jsont =
    Jsont.Object.(
      map (fun kind event_id mxc metadata -> (kind, event_id, mxc, metadata))
      |> mem "kind" Matrix_proto.Json.Codec.string
      |> opt_mem "event_id" Id.Event_id.jsont
      |> opt_mem "mxc" Media.Mxc.jsont
      |> opt_mem "metadata" Matrix_proto.Json.Codec.json
      |> finish)
  in
  Jsont.map
    ~dec:(fun json ->
      match Jsont.Json.decode wire_jsont json with
      | Ok ("event", Some event_id, None, None) -> Event_id event_id
      | Ok ("clear", None, Some _, None) | Ok ("encrypted", None, Some _, Some _)
        -> (
          match Jsont.Json.decode upload_result_jsont json with
          | Ok result -> Upload result
          | Error e -> Jsont.Error.msg Jsont.Meta.none e)
      | Ok (kind, _, _, _) ->
          Jsont.Error.msgf Jsont.Meta.none "invalid dependency result kind %S"
            kind
      | Error e -> Jsont.Error.msg Jsont.Meta.none e)
    ~enc:(function
      | Event_id event_id ->
          jobject
            [
              ("kind", jstring "event");
              ("event_id", Jsont.Json.string (Id.Event_id.to_string event_id));
            ]
      | Upload result -> upload_result_json result)
    Matrix_proto.Json.Codec.json

let persisted_resolution_jsont : persisted_resolution Jsont.t =
  Jsont.Object.(
    map (fun pr_id pr_event_id pr_result -> { pr_id; pr_event_id; pr_result })
    |> mem "id" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t -> t.pr_id)
    |> opt_mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.pr_event_id)
    |> opt_mem "result" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.pr_result)
    |> finish)

let persisted_resolution_of_result (id, result) =
  {
    pr_id = id;
    pr_event_id = None;
    pr_result = Some (dependency_result_json result);
  }

let dependency_result_of_persisted p =
  match (p.pr_event_id, p.pr_result) with
  | Some event_id, None -> Some (p.pr_id, Event_id event_id)
  | None, Some json -> (
      match Jsont.Json.decode dependency_result_jsont json with
      | Ok result -> Some (p.pr_id, result)
      | Error _ -> None)
  | _ -> None

let persisted_jsont : persisted Jsont.t =
  Jsont.Object.(
    map
      (fun
        p_id
        p_room_id
        p_kind
        p_txn_id
        p_created_at
        p_attempts
        p_wedged
        p_dependencies
        p_resolved
        p_extra_content
        p_attachment_filename
        p_pending_edit_content
        p_pending_edit_txn_id
        p_cancel_requested
        p_cancel_reason
        p_cancel_txn_id
        p_upload_cache_uri
        p_upload_result
      ->
        {
          p_id;
          p_room_id;
          p_kind;
          p_txn_id;
          p_created_at;
          p_attempts;
          p_wedged;
          p_dependencies;
          p_resolved;
          p_extra_content;
          p_attachment_filename;
          p_pending_edit_content;
          p_pending_edit_txn_id;
          p_cancel_requested;
          p_cancel_reason;
          p_cancel_txn_id;
          p_upload_cache_uri;
          p_upload_result;
        })
    |> mem "id" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.p_id)
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.p_room_id)
    |> mem "kind" kind_jsont ~enc:(fun t -> t.p_kind)
    |> mem "txn_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.p_txn_id)
    |> mem "created_at" Json_codec.persisted_timestamp
         ~dec_absent:(fun () -> Event.Timestamp.of_ms 0L)
         ~enc:(fun t -> t.p_created_at)
    |> mem "attempts" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.p_attempts)
    |> mem "wedged" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.p_wedged)
    |> mem "dependencies"
         (Jsont.list Matrix_proto.Json.Codec.Legacy.int)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.p_dependencies)
    |> mem "resolved"
         (Jsont.list persisted_resolution_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.p_resolved)
    |> opt_mem "extra_content" Matrix_proto.Json.Codec.json ~enc:(fun t ->
        t.p_extra_content)
    |> opt_mem "attachment_filename" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.p_attachment_filename)
    |> opt_mem "pending_edit_content" Matrix_proto.Json.Codec.json
         ~enc:(fun t -> t.p_pending_edit_content)
    |> opt_mem "pending_edit_txn_id" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.p_pending_edit_txn_id)
    |> mem "cancel_requested" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.p_cancel_requested)
    |> opt_mem "cancel_reason" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.p_cancel_reason)
    |> opt_mem "cancel_txn_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.p_cancel_txn_id)
    |> opt_mem "upload_cache_uri" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.p_upload_cache_uri)
    |> opt_mem "upload_result" persisted_upload_result_jsont ~enc:(fun t ->
        t.p_upload_result)
    |> finish)

(* Decode each queue element through the generic JSON codec first.  The object
   codec otherwise rejects the whole list when one record has a wrong-typed
   required member (for example [resolved.id]), losing unrelated requests. A
   fallback retains the identity and attachment references that can safely be
   recovered so graph quarantine still removes any upload parent. *)
let quarantine_persisted json =
  let room_id =
    Option.bind (Matrix_proto.Json.find_string "room_id" json) (fun value ->
        Result.to_option (Id.Room_id.of_string value))
    |> Option.value ~default:(Id.Room_id.of_string_exn "!invalid")
  in
  let p_id = Option.value (Matrix_proto.Json.find_int "id" json) ~default:0 in
  let attachment_uploads =
    match Matrix_proto.Json.find_mem "kind" json with
    | Some kind
      when Matrix_proto.Json.find_string "tag" kind = Some "attachment" ->
        [
          Matrix_proto.Json.find_int "attachment_original" kind;
          Matrix_proto.Json.find_int "attachment_thumbnail" kind;
        ]
        |> List.filter_map (function
          | Some id when id > 0 -> Some id
          | Some _ | None -> None)
        |> List.sort_uniq Int.compare
    | _ -> []
  in
  {
    p_id;
    p_room_id = room_id;
    p_kind = Invalid_kind_state { attachment_uploads };
    p_txn_id = "";
    p_created_at = Event.Timestamp.of_ms 0L;
    p_attempts = 0;
    p_wedged = true;
    p_dependencies = [];
    p_resolved = [];
    p_extra_content = None;
    p_attachment_filename = None;
    p_pending_edit_content = None;
    p_pending_edit_txn_id = None;
    p_cancel_requested = false;
    p_cancel_reason = None;
    p_cancel_txn_id = None;
    p_upload_cache_uri = None;
    p_upload_result = None;
  }

let persisted_entry_jsont : persisted Jsont.t =
  Jsont.map
    ~dec:(fun json ->
      match Jsont.Json.decode persisted_jsont json with
      | Ok value -> value
      | Error _ -> quarantine_persisted json)
    ~enc:(fun value ->
      match Jsont.Json.encode persisted_jsont value with
      | Ok json -> json
      | Error error -> Jsont.Error.msg Jsont.Meta.none error)
    Matrix_proto.Json.Codec.json

let queue_jsont : persisted list Jsont.t = Jsont.list persisted_entry_jsont

let upload_result_of_persisted = function
  | Persisted_upload_result result -> Some result
  | Invalid_upload_result -> None

let persisted_result_is_valid = function
  | None | Some (Persisted_upload_result _) -> true
  | Some Invalid_upload_result -> false

let persisted_pending_upload_has_role records record upload_id expected_role =
  List.mem upload_id record.p_dependencies
  && List.exists
       (fun upload ->
         upload.p_id = upload_id && upload.p_id < record.p_id
         && upload.p_room_id = record.p_room_id
         && persisted_result_is_valid upload.p_upload_result
         &&
         match upload.p_kind with
         | Persisted_kind (Upload_request { role; _ }) -> role = expected_role
         | _ -> false)
       records

let persisted_has_unique_upload_result record upload_id =
  if upload_id >= record.p_id || List.mem upload_id record.p_dependencies then
    false
  else
    match
      List.filter
        (fun resolution -> resolution.pr_id = upload_id)
        record.p_resolved
    with
    | [ resolution ] -> (
        match dependency_result_of_persisted resolution with
        | Some (_, Upload _) -> true
        | Some (_, Event_id _) | None -> false)
    | [] | _ :: _ :: _ -> false

let valid_persisted_kind records record =
  if not (persisted_result_is_valid record.p_upload_result) then false
  else
    match (record.p_kind, record.p_upload_result) with
    | Persisted_kind (Upload_request _), _ -> true
    | _, Some (Persisted_upload_result _) -> false
    | _, Some Invalid_upload_result -> false
    | Invalid_kind_state _, None -> false
    | Persisted_kind (Attachment { original_upload; thumbnail_upload; _ }), None
      ->
        (persisted_pending_upload_has_role records record original_upload
           `Original
        || persisted_has_unique_upload_result record original_upload)
        && Option.for_all
             (fun thumbnail_upload ->
               persisted_pending_upload_has_role records record thumbnail_upload
                 `Thumbnail
               || persisted_has_unique_upload_result record thumbnail_upload)
             thumbnail_upload
    | Persisted_kind _, None -> true

let valid_persisted_record records record =
  let unique ints =
    List.length ints = List.length (List.sort_uniq Int.compare ints)
  in
  let dependency_is_valid dependency =
    dependency > 0 && dependency < record.p_id
    && List.exists
         (fun parent ->
           parent.p_id = dependency && parent.p_room_id = record.p_room_id)
         records
  in
  let extra_content_is_valid =
    match record.p_extra_content with
    | None | Some (Jsont.Object _) -> true
    | Some _ -> false
  in
  let cache_shape_is_valid =
    match (record.p_kind, record.p_upload_cache_uri) with
    | Persisted_kind (Upload_request _), _ | _, None -> true
    | (Persisted_kind _ | Invalid_kind_state _), Some _ -> false
  in
  let optional_txn_is_valid = Option.for_all (fun txn_id -> txn_id <> "") in
  record.p_id > 0 && record.p_id < max_int && record.p_txn_id <> ""
  && record.p_attempts >= 0
  && record.p_attempts < max_int
  && unique record.p_dependencies
  && List.for_all dependency_is_valid record.p_dependencies
  && extra_content_is_valid && cache_shape_is_valid
  && optional_txn_is_valid record.p_pending_edit_txn_id
  && optional_txn_is_valid record.p_cancel_txn_id
  && valid_persisted_kind records record

let invalid_persisted_ids records =
  let node record = (Id.Room_id.to_string record.p_room_id, record.p_id) in
  let add_attachment_uploads record invalid upload_ids =
    List.fold_left
      (fun invalid upload_id ->
        (* Follow only actual upload nodes from the same room. A corrupt
           attachment must not be able to quarantine an unrelated event merely
           by copying its integer id. Forward references are invalid too, but
           still identify orphan upload bytes that must not be sent. *)
        if
          List.exists
            (fun upload ->
              upload.p_id = upload_id
              && upload.p_room_id = record.p_room_id
              &&
              match upload.p_kind with
              | Persisted_kind (Upload_request _) -> true
              | Persisted_kind _ | Invalid_kind_state _ -> false)
            records
        then
          Persisted_node_set.add
            (Id.Room_id.to_string record.p_room_id, upload_id)
            invalid
        else invalid)
      invalid upload_ids
  in
  (* Queue ids are allocated globally. If a damaged file reuses one, every
     reference to it is ambiguous, so quarantine all copies explicitly. Graph
     traversal below remains room-qualified and cannot spread merely because a
     dependency integer happens to collide in another room. *)
  let _, duplicate_ids =
    List.fold_left
      (fun (seen, duplicates) record ->
        if Int_set.mem record.p_id seen then
          (seen, Int_set.add record.p_id duplicates)
        else (Int_set.add record.p_id seen, duplicates))
      (Int_set.empty, Int_set.empty)
      records
  in
  let transaction_ids record =
    record.p_txn_id
    :: List.filter_map Fun.id
         [ record.p_pending_edit_txn_id; record.p_cancel_txn_id ]
  in
  let transaction_counts =
    List.fold_left
      (fun counts record ->
        List.fold_left
          (fun counts txn_id ->
            String_map.update txn_id
              (fun count -> Some (Option.value count ~default:0 + 1))
              counts)
          counts (transaction_ids record))
      String_map.empty records
  in
  let has_duplicate_transaction record =
    List.exists
      (fun txn_id ->
        Option.value (String_map.find_opt txn_id transaction_counts) ~default:0
        > 1)
      (transaction_ids record)
  in
  let initial =
    List.fold_left
      (fun invalid record ->
        let record_is_valid = valid_persisted_record records record in
        let invalid =
          if
            record_is_valid
            && (not (Int_set.mem record.p_id duplicate_ids))
            && not (has_duplicate_transaction record)
          then invalid
          else Persisted_node_set.add (node record) invalid
        in
        match record.p_kind with
        | Invalid_kind_state { attachment_uploads } ->
            add_attachment_uploads record invalid attachment_uploads
        | Persisted_kind (Attachment { original_upload; thumbnail_upload; _ })
          when not record_is_valid ->
            add_attachment_uploads record invalid
              (original_upload
              :: Option.value
                   (Option.map List.singleton thumbnail_upload)
                   ~default:[])
        | Persisted_kind _ -> invalid)
      Persisted_node_set.empty records
  in
  (* A request whose parent was quarantined must not be made independently
     sendable. Close over descendants until the malformed subgraphs are gone. *)
  let rec close invalid =
    let next =
      List.fold_left
        (fun invalid record ->
          let room = Id.Room_id.to_string record.p_room_id in
          if
            Persisted_node_set.mem (node record) invalid
            || List.exists
                 (fun dependency ->
                   Persisted_node_set.mem (room, dependency) invalid)
                 record.p_dependencies
          then Persisted_node_set.add (node record) invalid
          else invalid)
        invalid records
    in
    if Persisted_node_set.equal invalid next then invalid else close next
  in
  close initial

type t = {
  q_random : Random.t;
  q_user_id : Id.User_id.t;
  (* Stable queue ownership lets a new instance reclaim its own orphaned local
     entries after a crash, while preventing cleanup of another queue's media.
     It is absent for an in-memory queue unless the caller supplied an
     explicit identity. *)
  q_media_owner : string option;
  q_store : Store.t option;
  q_media_store : Media_store.t option;
  q_max_retries : int;
  q_base_delay_ms : int;
  q_max_delay_ms : int;
  mutable q_next_id : int;
  mutable q_rooms : request list String_map.t;  (** FIFO, oldest first. *)
  mutable q_enabled : bool;
  mutable q_disabled_rooms : String_set.t;
  mutable q_on_change : (request -> unit) list;
  mutable q_on_progress : (request -> progress -> unit) list;
}

let store_slot = Store.Slot.v ~name:"send_queue" queue_jsont

let upload_result_mxc = function
  | Clear_upload { mxc } | Encrypted_upload { mxc; _ } -> mxc

let persisted_local_cache_uri_matches ~txn_id uri =
  Media_store.is_local_uri uri
  &&
  let current = Media_store.local_uri ~txn_id in
  let media_id = Media.Mxc.media_id uri in
  (* Before v2, queue-created local media ids were the [m…] identifiers from
     [Random.txn_id]. Restrict the compatibility spelling to that namespace so
     an opaque transaction equal to another request's [v2_<digest>] media id
     cannot claim its bytes. *)
  Media.Mxc.equal uri current
  || (String.starts_with ~prefix:"m" media_id && String.equal media_id txn_id)

let cache_key_of_uri ~txn_id ~upload_result = function
  | None -> None
  | Some uri ->
      Result.to_option
        (Result.map
           (fun uri ->
             if
               persisted_local_cache_uri_matches ~txn_id uri
               || Option.exists
                    (fun result ->
                      Media.Mxc.equal uri (upload_result_mxc result))
                    upload_result
             then Some Media_store.{ uri; format = Media_store.File }
             else None)
           (Media.Mxc.of_string uri))
      |> Option.join

let valid_persisted_cache_uri ~txn_id uri upload_result =
  match Media.Mxc.of_string uri with
  | Error _ -> false
  | Ok uri ->
      persisted_local_cache_uri_matches ~txn_id uri
      || Option.exists
           (fun result -> Media.Mxc.equal uri (upload_result_mxc result))
           upload_result

let cache_upload_data t r =
  match (t.q_media_store, r.r_kind) with
  | Some media_store, Upload_request { data; _ } -> (
      let key =
        Media_store.
          { uri = local_uri ~txn_id:r.r_txn_id; format = Media_store.File }
      in
      match
        Media_store.add ~ignore_retention:true ~protected:true
          ?owner:t.q_media_owner media_store key ~data
      with
      | Ok () -> r.r_upload_cache_key <- Some key
      | Error error ->
          Log.warn (fun m ->
              m "Cannot cache upload %s, retaining inline payload: %s"
                r.r_txn_id (Error.to_string error)))
  | _ -> ()

let remove_local_upload_cache t r =
  match (t.q_media_store, r.r_upload_cache_key) with
  | Some media_store, Some key when Media_store.is_local_uri key.uri -> (
      match Media_store.remove media_store key with
      | Ok () -> r.r_upload_cache_key <- None
      | Error error ->
          Log.warn (fun m ->
              m "Cannot remove cancelled upload cache %s: %s"
                (Media.Mxc.to_string key.uri)
                (Error.to_string error)))
  | _ -> ()

let all t =
  String_map.bindings t.q_rooms
  |> List.concat_map snd
  |> List.sort (fun a b -> Int.compare a.r_id b.r_id)

(* Random sources are injectable and tests quite reasonably use a source that
   repeats bytes. No queued event, upload, edit or compensating redaction may
   nevertheless reuse another live transaction id. *)
let fresh_txn_id_with_used t used =
  let base = Random.txn_id t.q_random in
  let rec choose suffix =
    let candidate =
      if suffix = 0 then base else Printf.sprintf "%s-collision-%d" base suffix
    in
    if String_set.mem candidate used then choose (suffix + 1) else candidate
  in
  choose 0

let used_txn_ids t =
  let used =
    List.fold_left
      (fun used r ->
        let used = String_set.add r.r_txn_id used in
        Option.fold ~none:used
          ~some:(fun txn -> String_set.add txn used)
          r.r_cancel_txn_id)
      String_set.empty (all t)
  in
  let used =
    List.fold_left
      (fun used r ->
        Option.fold ~none:used
          ~some:(fun edit -> String_set.add edit.txn_id used)
          r.r_pending_edit)
      used (all t)
  in
  used

let fresh_txn_id t = fresh_txn_id_with_used t (used_txn_ids t)

let attachment_logical_filename base_content upload_filename =
  match Matrix_proto.Json.find_string "filename" base_content with
  | Some filename -> Some filename
  | None -> (
      match upload_filename with
      | Some _ as filename -> filename
      | None -> Matrix_proto.Json.find_string "body" base_content)

let persisted_attachment_filename records p =
  match p.p_attachment_filename with
  | Some _ as filename -> filename
  | None -> (
      match p.p_kind with
      | Persisted_kind (Attachment { content; original_upload; _ }) -> (
          match Matrix_proto.Json.find_string "filename" content with
          | Some _ as filename -> filename
          | None -> (
              match
                List.find_opt
                  (fun upload -> upload.p_id = original_upload)
                  records
              with
              | Some
                  {
                    p_kind =
                      Persisted_kind
                        (Upload_request { filename = Some filename; _ });
                    _;
                  } ->
                  Some filename
              | _ -> Matrix_proto.Json.find_string "body" content))
      | _ -> None)

let valid_pending_attachment_content = function
  | Jsont.Object _ as content -> (
      match Matrix_proto.Json.find_string "msgtype" content with
      | Some ("m.audio" | "m.file" | "m.image" | "m.video") ->
          let string_or_absent name =
            match Matrix_proto.Json.find_mem name content with
            | None | Some (Jsont.String _) -> true
            | Some _ -> false
          in
          string_or_absent "filename" && string_or_absent "body"
      | _ -> false)
  | _ -> false

let restore t =
  match t.q_store with
  | None -> ()
  | Some s -> (
      match Store.Slot.find s store_slot with
      | Ok None -> ()
      | Error e ->
          Log.warn (fun m ->
              m "Ignoring unreadable send queue: %s" (Error.to_string e))
      | Ok (Some l) ->
          let invalid_ids = invalid_persisted_ids l in
          (* Reserve all persisted ids before repairing a legacy intent whose
             cancellation id is absent.  The request is not in [q_rooms] yet,
             so consulting [fresh_txn_id t] here alone would miss its own id
             and records that appear later in [l]. *)
          let used =
            List.fold_left
              (fun used p ->
                let used = String_set.add p.p_txn_id used in
                Option.fold ~none:used
                  ~some:(fun txn -> String_set.add txn used)
                  p.p_cancel_txn_id
                |> fun used ->
                Option.fold ~none:used
                  ~some:(fun txn -> String_set.add txn used)
                  p.p_pending_edit_txn_id)
              String_set.empty l
          in
          let used = ref used in
          List.iter
            (fun (p, restored_kind) ->
              let pending_edit =
                match (p.p_kind, p.p_pending_edit_content) with
                | Persisted_kind (Attachment _), Some content
                  when valid_pending_attachment_content content ->
                    let txn_id =
                      match p.p_pending_edit_txn_id with
                      | Some txn_id -> txn_id
                      | None ->
                          let txn_id = fresh_txn_id_with_used t !used in
                          used := String_set.add txn_id !used;
                          txn_id
                    in
                    Some { content; txn_id }
                | _ -> None
              in
              let upload_result =
                Option.bind p.p_upload_result upload_result_of_persisted
              in
              let restored_cache_key =
                cache_key_of_uri ~txn_id:p.p_txn_id ~upload_result
                  p.p_upload_cache_uri
              in
              let malformed_cache_key =
                match p.p_upload_cache_uri with
                | None -> false
                | Some uri ->
                    not
                      (valid_persisted_cache_uri ~txn_id:p.p_txn_id uri
                         upload_result)
              in
              let r =
                {
                  r_id = p.p_id;
                  r_room_id = p.p_room_id;
                  r_kind = restored_kind;
                  r_txn_id = p.p_txn_id;
                  r_created_at = p.p_created_at;
                  r_status =
                    (if p.p_wedged || malformed_cache_key then Wedged
                     else Pending);
                  r_attempts = p.p_attempts;
                  r_last_error =
                    (if malformed_cache_key then
                       Some
                         (Error.Policy_denied
                            "persisted upload cache URI is malformed")
                     else None);
                  r_dependencies = p.p_dependencies;
                  r_resolved =
                    List.filter_map dependency_result_of_persisted p.p_resolved;
                  r_extra_content = p.p_extra_content;
                  r_attachment_filename = persisted_attachment_filename l p;
                  r_pending_edit = pending_edit;
                  r_cancel_requested = p.p_cancel_requested;
                  r_cancel_reason = p.p_cancel_reason;
                  r_cancel_txn_id =
                    (match (p.p_cancel_requested, p.p_cancel_txn_id) with
                    | true, None ->
                        let txn = fresh_txn_id_with_used t !used in
                        used := String_set.add txn !used;
                        Some txn
                    | _, txn_id -> txn_id);
                  r_upload_cache_key = restored_cache_key;
                  r_upload_result = upload_result;
                  r_detached = false;
                }
              in
              (match (t.q_media_store, r.r_kind, p.p_upload_cache_uri) with
              | Some media_store, Upload_request { data; _ }, None
                when Option.is_none upload_result -> (
                  let key =
                    Media_store.
                      {
                        uri = local_uri ~txn_id:r.r_txn_id;
                        format = Media_store.File;
                      }
                  in
                  match
                    Media_store.add ~ignore_retention:true ~protected:true
                      ?owner:t.q_media_owner media_store key ~data
                  with
                  | Ok () -> r.r_upload_cache_key <- Some key
                  | Error error ->
                      Log.warn (fun m ->
                          m "Cannot import legacy upload into media cache: %s"
                            (Error.to_string error)))
              | _ -> ());
              let key = Id.Room_id.to_string r.r_room_id in
              let cur =
                Option.value (String_map.find_opt key t.q_rooms) ~default:[]
              in
              t.q_rooms <- String_map.add key (cur @ [ r ]) t.q_rooms;
              if r.r_id >= t.q_next_id then t.q_next_id <- r.r_id + 1)
            (List.filter_map
               (fun p ->
                 if
                   (not
                      (Persisted_node_set.mem
                         (Id.Room_id.to_string p.p_room_id, p.p_id)
                         invalid_ids))
                   && valid_persisted_record l p
                 then
                   match p.p_kind with
                   | Persisted_kind kind -> Some (p, kind)
                   | Invalid_kind_state _ -> None
                 else None)
               l);
          (* Old stores have no dependency fields. Records from newer stores
             reached this point only after every dependency edge was checked;
             never strip a bad edge and accidentally make its child sendable. *)
          t.q_rooms <-
            String_map.map
              (List.map (fun r ->
                   r.r_dependencies <- List.sort Int.compare r.r_dependencies;
                   r.r_resolved <-
                     List.sort_uniq
                       (fun (left, _) (right, _) -> Int.compare left right)
                       r.r_resolved;
                   r))
              t.q_rooms)

(* A local cache entry is deliberately retained for a grace period after its
   last access. This covers a queue restore racing a process that is still
   finishing persistence, while allowing entries left by a crash before queue
   persistence to be reclaimed later. *)
let local_orphan_grace_seconds = 24 * 60 * 60

let derived_media_owner ~user_id ~store explicit_owner =
  match explicit_owner with
  | Some _ as owner -> owner
  | None -> (
      match store with
      | None -> None
      | Some store -> (
          match Option.bind (Store.dir store) Eio.Path.native with
          | None -> None
          | Some path ->
              let material = Id.User_id.to_string user_id ^ "\000" ^ path in
              Some
                ("send-queue:"
                ^ Digestif.SHA256.(digest_string material |> to_hex))))

(* Cleanup requires both a persistent queue and its stable identity. *)
let reconcile_local_cache t =
  match (t.q_store, t.q_media_store, t.q_media_owner) with
  | Some _, Some media_store, Some owner -> (
      let keep =
        all t
        |> List.filter_map (fun request -> request.r_upload_cache_key)
        |> List.filter (fun (key : Media_store.key) ->
            Media_store.is_local_uri key.uri)
      in
      let now = Ptime_clock.now () in
      let older_than =
        Option.value
          (Ptime.add_span now
             (Ptime.Span.of_int_s (-local_orphan_grace_seconds)))
          ~default:Ptime.epoch
      in
      match Media_store.prune_local ~owner ~keep ~older_than media_store with
      | Ok () -> ()
      | Error error ->
          (* Reconciliation is best effort. A backend failure must not wedge
             restored uploads; the next queue restore retries it. *)
          Log.warn (fun m ->
              m "Cannot reconcile local media cache: %s" (Error.to_string error))
      )
  | _ -> ()

let create ~random ~user_id ?store ?media_store ?media_owner ?(max_retries = 5)
    ?(base_delay_ms = 500) ?(max_delay_ms = 60_000) () =
  let t =
    {
      q_random = random;
      q_user_id = user_id;
      q_media_owner = derived_media_owner ~user_id ~store media_owner;
      q_store = store;
      q_media_store = media_store;
      q_max_retries = max_retries;
      q_base_delay_ms = base_delay_ms;
      q_max_delay_ms = max_delay_ms;
      q_next_id = 1;
      q_rooms = String_map.empty;
      q_enabled = true;
      q_disabled_rooms = String_set.empty;
      q_on_change = [];
      q_on_progress = [];
    }
  in
  restore t;
  reconcile_local_cache t;
  t

let user_id t = t.q_user_id
let store t = t.q_store
let on_change t f = t.q_on_change <- t.q_on_change @ [ f ]
let changed t r = List.iter (fun f -> f r) t.q_on_change
let on_progress t f = t.q_on_progress <- t.q_on_progress @ [ f ]

let to_persisted r =
  if r.r_detached then None
  else
    match r.r_status with
    | Sent _ | Uploaded _ | Cancelled -> None
    | Pending | Sending | Wedged ->
        let kind =
          match (r.r_upload_cache_key, r.r_kind) with
          | Some _, Upload_request upload ->
              Upload_request { upload with data = "" }
          | _ -> r.r_kind
        in
        Some
          {
            p_id = r.r_id;
            p_room_id = r.r_room_id;
            p_kind = Persisted_kind kind;
            p_txn_id = r.r_txn_id;
            p_created_at = r.r_created_at;
            p_attempts = r.r_attempts;
            p_wedged = (match r.r_status with Wedged -> true | _ -> false);
            p_dependencies = r.r_dependencies;
            p_resolved = List.map persisted_resolution_of_result r.r_resolved;
            p_extra_content = r.r_extra_content;
            p_attachment_filename = r.r_attachment_filename;
            p_pending_edit_content =
              Option.map (fun e -> e.content) r.r_pending_edit;
            p_pending_edit_txn_id =
              Option.map (fun e -> e.txn_id) r.r_pending_edit;
            p_cancel_requested = r.r_cancel_requested;
            p_cancel_reason = r.r_cancel_reason;
            p_cancel_txn_id = r.r_cancel_txn_id;
            p_upload_cache_uri =
              Option.map
                (fun key -> Media.Mxc.to_string key.Media_store.uri)
                r.r_upload_cache_key;
            p_upload_result =
              Option.map
                (fun result -> Persisted_upload_result result)
                r.r_upload_result;
          }

let save t =
  match t.q_store with
  | None -> ()
  | Some s -> (
      let l = List.filter_map to_persisted (all t) in
      match Store.Slot.set s store_slot l with
      | Ok () -> ()
      | Error e ->
          Log.err (fun m ->
              m "Cannot encode send queue: %s" (Error.to_string e)))

let now () = Event.Timestamp.of_ptime (Ptime_clock.now ())

let validate_dependencies t ~room_id depends_on =
  let all = all t in
  let parents =
    List.map (fun id -> List.find_opt (fun r -> r.r_id = id) all) depends_on
  in
  if
    List.length (List.sort_uniq Int.compare depends_on)
    <> List.length depends_on
  then invalid_arg "Matrix_client.Send_queue.enqueue: duplicate dependency";
  if
    List.exists Option.is_none parents
    || List.exists
         (fun r -> r.r_room_id <> room_id)
         (List.filter_map Fun.id parents)
  then invalid_arg "Matrix_client.Send_queue.enqueue: invalid dependency";
  if
    List.exists
      (fun r -> r.r_id >= t.q_next_id)
      (List.filter_map Fun.id parents)
  then invalid_arg "Matrix_client.Send_queue.enqueue: forward dependency";
  List.sort Int.compare depends_on

let make_request ~attachment_filename ~id ~room_id ~kind ~txn_id ~created_at
    ~depends_on ~extra_content =
  {
    r_id = id;
    r_room_id = room_id;
    r_kind = kind;
    r_txn_id = txn_id;
    r_created_at = created_at;
    r_status = Pending;
    r_attempts = 0;
    r_last_error = None;
    r_dependencies = depends_on;
    r_resolved = [];
    r_extra_content = extra_content;
    r_attachment_filename = attachment_filename;
    r_pending_edit = None;
    r_cancel_requested = false;
    r_cancel_reason = None;
    r_cancel_txn_id = None;
    r_detached = false;
    r_upload_cache_key = None;
    r_upload_result = None;
  }

let append_request t r =
  let key = Id.Room_id.to_string r.r_room_id in
  let cur = Option.value (String_map.find_opt key t.q_rooms) ~default:[] in
  t.q_rooms <- String_map.add key (cur @ [ r ]) t.q_rooms

let enqueue ?(depends_on = []) ?extra_content t ~room_id kind =
  let base_content =
    match kind with
    | Event { content; _ } | Attachment { content; _ } -> content
    | Reaction _ | Redaction _ | Upload_request _ -> jobject []
  in
  (* Event content and extension content are both Matrix JSON objects. Validate
     them together before allocating an id or changing the queue so [payload]
     and the send path remain total for every admitted request. *)
  ignore (Json_codec.merge_extra_content base_content ?extra_content ());
  (match kind with
  | Attachment { original_upload; thumbnail_upload; _ } ->
      let check_parent id expected_role =
        match List.find_opt (fun request -> request.r_id = id) (all t) with
        | Some
            { r_room_id = parent_room; r_kind = Upload_request { role; _ }; _ }
          when parent_room = room_id && role = expected_role ->
            ()
        | Some _ ->
            invalid_arg
              "Matrix_client.Send_queue.enqueue: attachment upload role"
        | None ->
            invalid_arg
              "Matrix_client.Send_queue.enqueue: attachment upload missing"
      in
      check_parent original_upload `Original;
      Option.iter (fun id -> check_parent id `Thumbnail) thumbnail_upload
  | _ -> ());
  let depends_on = validate_dependencies t ~room_id depends_on in
  let attachment_filename =
    match kind with
    | Attachment { content; original_upload; _ } ->
        let upload_filename =
          match
            List.find_opt
              (fun request -> request.r_id = original_upload)
              (all t)
          with
          | Some { r_kind = Upload_request { filename; _ }; _ } -> filename
          | _ -> None
        in
        attachment_logical_filename content upload_filename
    | _ -> None
  in
  let txn_id = fresh_txn_id t in
  let created_at = now () in
  let r =
    make_request ~attachment_filename ~id:t.q_next_id ~room_id ~kind ~txn_id
      ~created_at ~depends_on ~extra_content
  in
  cache_upload_data t r;
  t.q_next_id <- t.q_next_id + 1;
  append_request t r;
  save t;
  changed t r;
  r

let send_message ?depends_on ?extra_content t ~room_id ~event_type ~content =
  enqueue ?depends_on ?extra_content t ~room_id (Event { event_type; content })

let upload ?depends_on t ~room_id ~role ~content_type ~data ?filename () =
  enqueue ?depends_on t ~room_id
    (Upload_request
       { role; content_type; filename; data; encrypted_metadata = None })

let upload_encrypted ?depends_on t ~room_id ~role ~content_type ~encrypted
    ?filename () =
  enqueue ?depends_on t ~room_id
    (Upload_request
       {
         role;
         content_type;
         filename;
         data = encrypted.Encrypted_attachment.ciphertext;
         encrypted_metadata = Some encrypted.metadata;
       })

let attachment_upload ~content_type ~data ?filename () =
  { content_type; data; filename; encrypted_metadata = None }

let attachment_upload_encrypted ~content_type ~encrypted ?filename () =
  {
    content_type;
    data = encrypted.Encrypted_attachment.ciphertext;
    filename;
    encrypted_metadata = Some encrypted.metadata;
  }

let send_attachment ?depends_on ?extra_content t ~room_id ~base_content
    ~original ?thumbnail () =
  (* Validate everything before allocating IDs or changing the queue.  In
     particular, this keeps a malformed content/extra or dependency from
     leaving an orphan upload behind. *)
  ignore (Json_codec.merge_extra_content base_content ?extra_content ());
  let caller_dependencies = Option.value depends_on ~default:[] in
  let caller_dependencies =
    validate_dependencies t ~room_id caller_dependencies
  in
  let original_id = t.q_next_id in
  let thumbnail_id = Option.map (fun _ -> original_id + 1) thumbnail in
  let attachment_id =
    original_id
    + Option.value (Option.map (fun _ -> 1) thumbnail) ~default:0
    + 1
  in
  (* Allocate all transaction IDs while the graph is still private.  A
     random-source failure therefore cannot expose a partial graph. *)
  let used = ref (used_txn_ids t) in
  let allocate_txn_id () =
    let txn_id = fresh_txn_id_with_used t !used in
    used := String_set.add txn_id !used;
    txn_id
  in
  let original_txn_id = allocate_txn_id () in
  let thumbnail_txn_id = Option.map (fun _ -> allocate_txn_id ()) thumbnail in
  let attachment_txn_id = allocate_txn_id () in
  let created_at = now () in
  let original_request =
    make_request ~id:original_id ~room_id
      ~kind:
        (Upload_request
           {
             role = `Original;
             content_type = original.content_type;
             filename = original.filename;
             data = original.data;
             encrypted_metadata = original.encrypted_metadata;
           })
      ~txn_id:original_txn_id ~created_at ~depends_on:caller_dependencies
      ~extra_content:None ~attachment_filename:None
  in
  let thumbnail_request =
    Option.map
      (fun thumbnail ->
        make_request ~id:(Option.get thumbnail_id) ~room_id
          ~kind:
            (Upload_request
               {
                 role = `Thumbnail;
                 content_type = thumbnail.content_type;
                 filename = thumbnail.filename;
                 data = thumbnail.data;
                 encrypted_metadata = thumbnail.encrypted_metadata;
               })
          ~txn_id:(Option.get thumbnail_txn_id)
          ~created_at ~depends_on:caller_dependencies ~extra_content:None
          ~attachment_filename:None)
      thumbnail
  in
  let attachment_request =
    let upload_dependencies =
      original_id
      :: Option.value (Option.map List.singleton thumbnail_id) ~default:[]
    in
    make_request ~id:attachment_id ~room_id
      ~kind:
        (Attachment
           {
             content = base_content;
             original_upload = original_id;
             thumbnail_upload = thumbnail_id;
           })
      ~txn_id:attachment_txn_id ~created_at
      ~depends_on:
        (List.sort Int.compare (caller_dependencies @ upload_dependencies))
      ~extra_content
      ~attachment_filename:
        (attachment_logical_filename base_content original.filename)
  in
  let requests =
    original_request
    :: Option.value (Option.map List.singleton thumbnail_request) ~default:[]
    @ [ attachment_request ]
  in
  List.iter (cache_upload_data t) requests;
  t.q_next_id <- attachment_id + 1;
  List.iter (append_request t) requests;
  save t;
  List.iter (changed t) requests;
  attachment_request

let send_text ?extra_content t ~room_id ~body =
  let content =
    jobject [ ("msgtype", jstring "m.text"); ("body", jstring body) ]
  in
  send_message ?extra_content t ~room_id ~event_type:"m.room.message" ~content

let send_edit t ~room_id ~event_id ~new_body ?formatted_body ?format () =
  (* Keep edits as ordinary [Event] requests.  In particular, adding a
     separate queue kind would make the kind's persistence and encryption
     handling needlessly different from a regular room message. *)
  let formatted =
    match formatted_body with
    | None -> []
    | Some formatted_body ->
        let format = Option.value format ~default:"org.matrix.custom.html" in
        [
          ("format", jstring format); ("formatted_body", jstring formatted_body);
        ]
  in
  let outer_formatted =
    match formatted_body with
    | None -> []
    | Some formatted_body ->
        let format = Option.value format ~default:"org.matrix.custom.html" in
        [
          ("format", jstring format);
          ("formatted_body", jstring ("* " ^ formatted_body));
        ]
  in
  let new_content =
    jobject
      (("msgtype", jstring "m.text") :: ("body", jstring new_body) :: formatted)
  in
  let content =
    jobject
      (("msgtype", jstring "m.text")
       :: ("body", jstring ("* " ^ new_body))
       :: outer_formatted
      @ [
          ("m.new_content", new_content);
          ( "m.relates_to",
            jobject
              [
                ("rel_type", jstring "m.replace");
                ("event_id", jstring (Id.Event_id.to_string event_id));
              ] );
        ])
  in
  send_message t ~room_id ~event_type:"m.room.message" ~content

let send_reaction ?extra_content t ~room_id ~relates_to ~key =
  enqueue ?extra_content t ~room_id (Reaction { relates_to; key })

let send_redaction t ~room_id ~event_id ?reason () =
  enqueue t ~room_id (Redaction { event_id; reason })

let requests t = all t

let room_requests t room_id =
  Option.value
    (String_map.find_opt (Id.Room_id.to_string room_id) t.q_rooms)
    ~default:[]

(* A key is always [Id.Room_id.to_string] of a room this queue was given, so
   it parses back; a key that does not names no room and is dropped. *)
let rooms t =
  String_map.bindings t.q_rooms
  |> List.filter_map (fun (k, _) -> Result.to_option (Id.Room_id.of_string k))

let room_enabled t room_id =
  t.q_enabled
  && not (String_set.mem (Id.Room_id.to_string room_id) t.q_disabled_rooms)

let next t room_id =
  if not (room_enabled t room_id) then None
  else
    match room_requests t room_id with
    | r :: _ when r.r_status = Pending && r.r_dependencies = [] -> Some r
    | _ -> None

let pending_count t =
  List.length (List.filter (fun r -> r.r_status = Pending) (all t))

let is_empty t = String_map.is_empty t.q_rooms

(* An emptied room leaves the map, so [rooms] and [is_empty] need no further
   filtering and the map does not grow without bound. *)
let remove t r =
  let key = Id.Room_id.to_string r.r_room_id in
  let cur = Option.value (String_map.find_opt key t.q_rooms) ~default:[] in
  match List.filter (fun x -> x.r_id <> r.r_id) cur with
  | [] -> t.q_rooms <- String_map.remove key t.q_rooms
  | l -> t.q_rooms <- String_map.add key l t.q_rooms

(* [cancel_pending] is also used when a send that was marked for cancellation
   fails.  Keeping the removal and persistence in one operation means a
   restart cannot resurrect either the parent or one of its dependants. *)
let descendants t r =
  let all_requests = all t in
  let rec collect seen found = function
    | [] -> found
    | parent :: remaining ->
        let children =
          all_requests
          |> List.filter (fun child ->
              child.r_status <> Cancelled
              && List.mem parent.r_id child.r_dependencies
              && not (List.mem child.r_id seen))
          |> List.sort (fun a b -> Int.compare a.r_id b.r_id)
        in
        let seen =
          List.fold_left (fun ids child -> child.r_id :: ids) seen children
        in
        collect seen (List.rev_append children found) (remaining @ children)
  in
  let found = collect [ r.r_id ] [] [ r ] in
  List.sort (fun a b -> Int.compare a.r_id b.r_id) found

let attachment_upload_parents t r =
  match r.r_kind with
  | Attachment { original_upload; thumbnail_upload; _ } ->
      let ids =
        original_upload
        :: Option.fold ~none:[] ~some:(fun id -> [ id ]) thumbnail_upload
      in
      List.filter_map
        (fun id -> List.find_opt (fun candidate -> candidate.r_id = id) (all t))
        ids
      |> List.filter (fun candidate ->
          match candidate.r_kind with Upload_request _ -> true | _ -> false)
  | _ -> []

let cancel_pending t r =
  let descendants = descendants t r in
  (* Attachment uploads are private to their event in the convenience API.
     Drop still-local upload parents too, while leaving a shared upload alone
     for callers that deliberately built a wider graph with [enqueue]. *)
  let upload_parents =
    attachment_upload_parents t r
    |> List.filter (fun parent ->
        not
          (List.exists
             (fun child ->
               child.r_id <> r.r_id && List.mem parent.r_id child.r_dependencies)
             (all t)))
  in
  (* A parent upload may already be in [send_one]. Keep it in the queue with a
     durable cancellation intent until that attempt returns; removing it here
     would let the in-flight callback commit an orphaned [Uploaded] result on a
     detached request. The current upload callback has no cancellation token,
     so this is the strongest safe cancellation boundary available. *)
  let in_flight_uploads =
    List.filter (fun parent -> parent.r_status = Sending) upload_parents
  in
  List.iter
    (fun parent ->
      parent.r_cancel_requested <- true;
      parent.r_cancel_reason <- None;
      parent.r_cancel_txn_id <- None)
    in_flight_uploads;
  let cancelled =
    List.sort_uniq
      (fun left right -> Int.compare left.r_id right.r_id)
      ((r :: descendants)
      @ List.filter
          (fun parent ->
            not
              (List.exists
                 (fun active -> active.r_id = parent.r_id)
                 in_flight_uploads))
          upload_parents)
  in
  List.iter
    (fun request ->
      remove_local_upload_cache t request;
      request.r_status <- Cancelled;
      request.r_pending_edit <- None;
      request.r_cancel_requested <- false;
      request.r_cancel_reason <- None;
      request.r_cancel_txn_id <- None;
      remove t request)
    cancelled;
  save t;
  List.iter (changed t) cancelled

let cancel_with_reason ?reason t r =
  match r.r_status with
  | Sent _ | Uploaded _ -> `Already_sent
  | Sending ->
      (* The caller may race the synchronous attempt by yielding from its
         [send] function.  Persist exactly one compensating intent before
         returning; the successful attempt turns this request into a normal
         redaction below. *)
      if not r.r_cancel_requested then begin
        (* Allocate before changing the intent so a failing random source
           leaves the request in its original, retryable state. *)
        let cancel_txn_id = fresh_txn_id t in
        r.r_pending_edit <- None;
        r.r_cancel_requested <- true;
        r.r_cancel_reason <- reason;
        r.r_cancel_txn_id <- Some cancel_txn_id;
        save t
      end;
      `In_flight
  | Cancelled -> `Cancelled
  | Pending | Wedged ->
      cancel_pending t r;
      `Cancelled

let cancel t r = cancel_with_reason t r

(* Finish a transport callback that belongs to a room detached by
   [forget_room].  It is deliberately separate from [cancel_pending]: the
   latter can create a compensating redaction for a normal in-flight cancel,
   whereas forgetting a room must never send anything further for it. *)
let finish_detached t r error =
  remove_local_upload_cache t r;
  r.r_upload_result <- None;
  r.r_status <- Cancelled;
  r.r_last_error <- error;
  r.r_pending_edit <- None;
  r.r_cancel_requested <- false;
  r.r_cancel_reason <- None;
  r.r_cancel_txn_id <- None;
  r.r_detached <- false;
  save t;
  changed t r

let forget_room t room_id =
  let key = Id.Room_id.to_string room_id in
  let requests = room_requests t room_id in
  (* Remove the whole room before touching observers or persisting.  An
     observer is allowed to enqueue another request while it runs; that new
     request is therefore a deliberate post-forget send and is not included
     in this deletion. *)
  t.q_rooms <- String_map.remove key t.q_rooms;
  List.iter
    (fun r ->
      r.r_pending_edit <- None;
      r.r_cancel_requested <- false;
      r.r_cancel_reason <- None;
      r.r_cancel_txn_id <- None;
      r.r_detached <- true;
      match r.r_status with
      | Sending ->
          (* Keep the local upload bytes until the callback returns. *)
          ()
      | _ ->
          remove_local_upload_cache t r;
          r.r_upload_result <- None;
          r.r_status <- Cancelled;
          r.r_detached <- false)
    requests;
  save t;
  List.iter (fun r -> if r.r_status = Cancelled then changed t r) requests

let unwedge t r =
  match r.r_status with
  | Wedged ->
      r.r_status <- Pending;
      r.r_attempts <- 0;
      r.r_last_error <- None;
      save t;
      changed t r
  | _ -> ()

let enabled t = t.q_enabled
let set_enabled t ~enabled = t.q_enabled <- enabled

let set_room_enabled t room_id ~enabled =
  let key = Id.Room_id.to_string room_id in
  t.q_disabled_rooms <-
    (if enabled then String_set.remove key t.q_disabled_rooms
     else String_set.add key t.q_disabled_rooms)

type outcome =
  | Sent_ok of Id.Event_id.t
  | Uploaded_ok of upload_result
  | Retry_in of float
  | Failed of Error.t

let retry_delay t r =
  let n = max 1 r.r_attempts in
  (* [2 ^ 30] already exceeds any sane cap; stop there rather than overflow. *)
  let shift = min (n - 1) 30 in
  let exponential =
    float_of_int t.q_base_delay_ms *. (2. ** float_of_int shift)
  in
  (* Keep the jitter source on the queue so callers can inject a deterministic
     source in tests. Four bytes give a uniform fraction in [0, 1), and hence
     a multiplier in [0.5, 1.5). Apply the cap after jitter: the configured
     maximum is a hard upper bound on the value returned to the scheduler. *)
  let bytes = Random.generate t.q_random 4 in
  let fraction =
    let byte i = float_of_int (Char.code (String.unsafe_get bytes i)) in
    (byte 0 /. 256.)
    +. (byte 1 /. 65536.)
    +. (byte 2 /. 16_777_216.)
    +. (byte 3 /. 4_294_967_296.)
  in
  let jittered = exponential *. (0.5 +. fraction) in
  Float.min jittered (float_of_int t.q_max_delay_ms) /. 1000.

let classify t r (e : Error.t) =
  let retry_or_wedge ?after () =
    if r.r_attempts > t.q_max_retries then Failed e
    else
      match after with
      | Some ms -> Retry_in (float_of_int ms /. 1000.)
      | None -> Retry_in (retry_delay t r)
  in
  match e with
  | Error.Network_error _ -> retry_or_wedge ()
  | Error.Policy_denied _ | Error.Tls_error _ | Error.Json_error _ -> Failed e
  | Error.Http_error { status; _ } ->
      if status = 429 || status >= 500 then retry_or_wedge () else Failed e
  | Error.Matrix_error m -> (
      match m.errcode with
      | Error.M_LIMIT_EXCEEDED -> retry_or_wedge ?after:m.retry_after_ms ()
      (* [send_one] increments [r_attempts] before calling us. Keep the
         direct [classify] API equivalent by treating zero as the first
         attempt too, but never retry this ambiguous server failure twice. *)
      | Error.M_UNKNOWN ->
          if r.r_attempts <= 1 then retry_or_wedge () else Failed e
      | _ -> Failed e)
  | Error.No_session | Error.No_content -> Failed e

let content_of_kind r =
  match r.r_kind with
  | Event { content; _ } ->
      Json_codec.merge_extra_content content ?extra_content:r.r_extra_content ()
  | Reaction { relates_to; key } ->
      Json_codec.merge_extra_content
        (jobject
           [
             ( "m.relates_to",
               jobject
                 [
                   ("rel_type", jstring "m.annotation");
                   ("event_id", jstring (Id.Event_id.to_string relates_to));
                   ("key", jstring key);
                 ] );
           ])
        ?extra_content:r.r_extra_content ()
  | Redaction { reason; _ } -> (
      match reason with
      | Some reason -> jobject [ ("reason", jstring reason) ]
      | None -> jobject [])
  | Upload_request _ -> jobject []
  | Attachment { content; _ } ->
      Json_codec.merge_extra_content content ?extra_content:r.r_extra_content ()

let event_type_of_kind r =
  match r.r_kind with
  | Event { event_type; _ } -> event_type
  | Reaction _ -> "m.reaction"
  | Redaction _ -> "m.room.redaction"
  | Upload_request _ -> "m.upload"
  | Attachment _ -> "m.room.message"

type payload =
  | Send of { event_type : string; content : Jsont.json }
  | Redact of { event_id : Id.Event_id.t; reason : string option }
  | Upload_payload of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Encrypted_attachment.metadata option;
    }

let attachment_error message = Error (Error.Json_error message)

let attachment_owned_fields =
  [ "body"; "filename"; "format"; "formatted_body"; "m.mentions" ]

let object_members = function
  | Jsont.Object (members, _) -> Some members
  | _ -> None

let object_without names members =
  List.filter (fun (name, _) -> not (List.mem (fst name) names)) members

let sanitize_attachment_extra = function
  | None -> None
  | Some extra -> (
      match object_members extra with
      | None -> None
      | Some members ->
          Some
            (Jsont.Json.object'
               (object_without attachment_owned_fields members)))

let replace_object_members content ~remove ~add =
  match object_members content with
  | None -> attachment_error "attachment content must be a JSON object"
  | Some members ->
      let names = remove in
      Ok
        (Jsont.Json.object'
           (object_without names members
           @ List.map
               (fun (name, value) ->
                 Jsont.Json.mem (Jsont.Json.name name) value)
               add))

let encrypted_file_json ~url metadata =
  let file = Encrypted_attachment.Metadata.to_event_file ~url metadata in
  jobject
    [
      ("url", jstring file.url);
      ("key", file.key);
      ("iv", jstring file.iv);
      ( "hashes",
        jobject
          (List.map (fun (name, hash) -> (name, jstring hash)) file.hashes) );
      ("v", jstring file.v);
    ]

let attachment_result_json ~clear_key ~encrypted_key result =
  match result with
  | Clear_upload { mxc } -> (clear_key, jstring (Media.Mxc.to_string mxc))
  | Encrypted_upload { mxc; metadata } ->
      ( encrypted_key,
        encrypted_file_json ~url:(Media.Mxc.to_string mxc) metadata )

let attachment_upload_result r id =
  match List.assoc_opt id r.r_resolved with
  | Some (Upload result) -> Ok result
  | Some _ -> attachment_error "attachment dependency is not an upload"
  | None -> attachment_error "attachment upload result is missing"

let attachment_content r =
  match r.r_kind with
  | Attachment { content; original_upload; thumbnail_upload } -> (
      let base =
        try Ok (content_of_kind r)
        with Invalid_argument message -> attachment_error message
      in
      match base with
      | Error _ as error -> error
      | Ok base -> (
          match attachment_upload_result r original_upload with
          | Error _ as error -> error
          | Ok original -> (
              let original =
                attachment_result_json ~clear_key:"url" ~encrypted_key:"file"
                  original
              in
              match
                replace_object_members base ~remove:[ "url"; "file" ]
                  ~add:[ original ]
              with
              | Error _ as error -> error
              | Ok content -> (
                  match thumbnail_upload with
                  | None -> (
                      match Matrix_proto.Json.find_mem "info" content with
                      | None -> Ok content
                      | Some info ->
                          let info =
                            match object_members info with
                            | None -> jobject []
                            | Some members ->
                                Jsont.Json.object'
                                  (object_without
                                     [ "thumbnail_url"; "thumbnail_file" ]
                                     members)
                          in
                          replace_object_members content ~remove:[ "info" ]
                            ~add:[ ("info", info) ])
                  | Some thumbnail_upload -> (
                      match attachment_upload_result r thumbnail_upload with
                      | Error _ as error -> error
                      | Ok thumbnail ->
                          let info =
                            match Matrix_proto.Json.find_mem "info" content with
                            | Some info -> info
                            | None -> jobject []
                          in
                          let thumbnail =
                            attachment_result_json ~clear_key:"thumbnail_url"
                              ~encrypted_key:"thumbnail_file" thumbnail
                          in
                          let info =
                            match object_members info with
                            | None -> jobject []
                            | Some members ->
                                Jsont.Json.object'
                                  (object_without
                                     [ "thumbnail_url"; "thumbnail_file" ]
                                     members
                                  @ [
                                      Jsont.Json.mem
                                        (Jsont.Json.name (fst thumbnail))
                                        (snd thumbnail);
                                    ])
                          in
                          replace_object_members content ~remove:[ "info" ]
                            ~add:[ ("info", info) ])))))
  | _ -> attachment_error "request is not an attachment"

(* Local echoes can expose an upload before its attachment dependency has
   resolved.  This deliberately has a different contract from
   [attachment_content]: an echo is best-effort UI state, so malformed parents
   or a missing cache entry leave the original content visible rather than
   raising.  A durable remote result always wins over a still-present local
   cache entry. *)
let local_echo_upload_result t r upload_id =
  (* A queue without a media store retains the historical echo contract: the
     attachment remains its caller-supplied content until the event is sent.
     In particular, do not expose an already-resolved upload result merely
     because it happens to be present in the queue state. *)
  match t.q_media_store with
  | None -> None
  | Some _ -> (
      let local_cache_result parent =
        match (t.q_media_store, parent.r_upload_cache_key) with
        | Some media_store, Some key ->
            let now = Ptime_clock.now () in
            begin match Media_store.get ~now media_store key with
            | Ok (Some _) -> (
                match parent.r_kind with
                | Upload_request { encrypted_metadata; _ } ->
                    Some
                      (match encrypted_metadata with
                      | None -> Clear_upload { mxc = key.uri }
                      | Some metadata ->
                          Encrypted_upload { mxc = key.uri; metadata })
                | _ -> None)
            | Ok None | Error _ -> None
            end
        | _ -> None
      in
      match List.assoc_opt upload_id r.r_resolved with
      | Some (Upload result) -> Some result
      | _ -> (
          match
            List.find_opt (fun parent -> parent.r_id = upload_id) (all t)
          with
          | None -> None
          | Some { r_upload_result = Some result; _ } -> Some result
          | Some parent -> local_cache_result parent))

let local_echo_attachment_content t r base original_upload thumbnail_upload =
  let original = local_echo_upload_result t r original_upload in
  match original with
  | None -> base
  | Some original -> (
      match
        replace_object_members base ~remove:[ "url"; "file" ]
          ~add:
            [
              attachment_result_json ~clear_key:"url" ~encrypted_key:"file"
                original;
            ]
      with
      | Error _ -> base
      | Ok content -> (
          let info_without_thumbnail content =
            match Matrix_proto.Json.find_mem "info" content with
            | None -> content
            | Some info -> (
                let info =
                  match object_members info with
                  | None -> jobject []
                  | Some members ->
                      Jsont.Json.object'
                        (object_without
                           [ "thumbnail_url"; "thumbnail_file" ]
                           members)
                in
                match
                  replace_object_members content ~remove:[ "info" ]
                    ~add:[ ("info", info) ]
                with
                | Ok content -> content
                | Error _ -> content)
          in
          match thumbnail_upload with
          | None -> info_without_thumbnail content
          | Some thumbnail_upload -> (
              match local_echo_upload_result t r thumbnail_upload with
              | None -> info_without_thumbnail content
              | Some thumbnail -> (
                  let info =
                    Option.value
                      (Matrix_proto.Json.find_mem "info" content)
                      ~default:(jobject [])
                  in
                  let info_members =
                    match object_members info with
                    | None -> []
                    | Some members ->
                        object_without
                          [ "thumbnail_url"; "thumbnail_file" ]
                          members
                  in
                  let info =
                    Jsont.Json.object'
                      (info_members
                      @ [
                          (let name, value =
                             attachment_result_json ~clear_key:"thumbnail_url"
                               ~encrypted_key:"thumbnail_file" thumbnail
                           in
                           Jsont.Json.mem (Jsont.Json.name name) value);
                        ])
                  in
                  match
                    replace_object_members content ~remove:[ "info" ]
                      ~add:[ ("info", info) ]
                  with
                  | Ok content -> content
                  | Error _ -> content))))

let content_for_send r =
  match r.r_kind with
  | Attachment _ -> (
      match attachment_content r with
      | Ok content -> Ok ("m.room.message", content)
      | Error _ as error -> error)
  | _ -> Ok (event_type_of_kind r, content_of_kind r)

let attachment_content_for_base r base_content =
  match r.r_kind with
  | Attachment { original_upload; thumbnail_upload; _ } ->
      attachment_content
        {
          r with
          r_extra_content = sanitize_attachment_extra r.r_extra_content;
          r_kind =
            Attachment
              { content = base_content; original_upload; thumbnail_upload };
        }
  | _ -> attachment_error "request is not an attachment"

let attachment_caption_content r ~caption ~formatted_body ~format ~mentions =
  match r.r_kind with
  | Event _ | Reaction _ | Redaction _ | Upload_request _ ->
      attachment_error "request is not an attachment"
  | Attachment { content; _ } -> (
      if Option.is_none (object_members content) then
        attachment_error "attachment content must be a JSON object"
      else
        match Matrix_proto.Json.find_string "msgtype" content with
        | Some ("m.audio" | "m.file" | "m.image" | "m.video") ->
            let string_member name =
              match Matrix_proto.Json.find_mem name content with
              | None -> Ok None
              | Some (Jsont.String (value, _)) -> Ok (Some value)
              | Some _ ->
                  attachment_error
                    (Printf.sprintf "attachment %s must be a string" name)
            in
            let result =
              match (string_member "filename", string_member "body") with
              | (Error _ as error), _ | _, (Error _ as error) -> error
              | Ok content_filename, Ok content_body -> (
                  let filename =
                    match content_filename with
                    | Some _ as filename -> filename
                    | None -> (
                        match r.r_attachment_filename with
                        | Some _ as filename -> filename
                        | None -> content_body)
                  in
                  match (caption, filename) with
                  | None, None ->
                      attachment_error "attachment has no logical filename"
                  | Some body, _ | None, Some body ->
                      let add =
                        ("body", jstring body)
                        ::
                        (match caption with
                        | Some _ ->
                            Option.fold ~none:[]
                              ~some:(fun filename ->
                                [ ("filename", jstring filename) ])
                              filename
                        | None -> [])
                        @ (match formatted_body with
                          | None -> []
                          | Some body ->
                              [
                                ( "format",
                                  jstring
                                    (Option.value format
                                       ~default:"org.matrix.custom.html") );
                                ("formatted_body", jstring body);
                              ])
                        @
                        match mentions with
                        | None -> []
                        | Some mentions -> [ ("m.mentions", mentions) ]
                      in
                      replace_object_members content
                        ~remove:attachment_owned_fields ~add)
            in
            result
        | Some msgtype ->
            attachment_error
              (Printf.sprintf "cannot edit caption for msgtype %S" msgtype)
        | None -> attachment_error "attachment content has no msgtype")

let attachment_replacement_content r event_id base_content =
  match attachment_content_for_base r base_content with
  | Error _ as error -> error
  | Ok new_content -> (
      match object_members new_content with
      | None -> attachment_error "attachment content must be a JSON object"
      | Some _ ->
          let body =
            Option.value
              (Matrix_proto.Json.find_string "body" new_content)
              ~default:""
          in
          let formatted_body =
            Matrix_proto.Json.find_string "formatted_body" new_content
          in
          let outer_content =
            match
              replace_object_members new_content
                ~remove:
                  [ "body"; "formatted_body"; "m.relates_to"; "m.new_content" ]
                ~add:
                  (("body", jstring ("* " ^ body))
                  ::
                  (match formatted_body with
                  | None -> []
                  | Some formatted_body ->
                      [ ("formatted_body", jstring ("* " ^ formatted_body)) ]))
            with
            | Ok content -> content
            | Error _ -> new_content
          in
          replace_object_members outer_content
            ~remove:[ "m.new_content"; "m.relates_to" ]
            ~add:
              [
                ("m.new_content", new_content);
                ( "m.relates_to",
                  jobject
                    [
                      ("rel_type", jstring "m.replace");
                      ("event_id", jstring (Id.Event_id.to_string event_id));
                    ] );
              ])

let edit_attachment_caption ?formatted_body ?format ?mentions t r ~caption =
  let validate_mentions = function
    | None -> Ok None
    | Some (Jsont.Object _ as mentions) -> Ok (Some mentions)
    | Some _ -> attachment_error "m.mentions must be a JSON object"
  in
  match validate_mentions mentions with
  | Error _ as error -> error
  | Ok mentions -> (
      let is_attachment =
        match r.r_kind with Attachment _ -> true | _ -> false
      in
      let owned = List.exists (fun candidate -> candidate == r) (all t) in
      let active =
        match r.r_status with Pending | Sending | Wedged -> true | _ -> false
      in
      if not is_attachment then attachment_error "request is not an attachment"
      else if active && not owned then
        attachment_error "attachment request is not owned by this queue"
      else if r.r_cancel_requested then
        attachment_error "attachment request is being cancelled"
      else
        match r.r_status with
        | Sent _ | Uploaded _ | Cancelled -> Ok Already_sent
        | Pending | Wedged -> (
            match
              attachment_caption_content r ~caption ~formatted_body ~format
                ~mentions
            with
            | Error _ as error -> error
            | Ok content -> (
                match r.r_pending_edit with
                | Some edit ->
                    r.r_pending_edit <- Some { edit with content };
                    save t;
                    changed t r;
                    Ok Deferred
                | None -> (
                    match r.r_kind with
                    | Attachment ({ content = _; _ } as attachment) ->
                        r.r_kind <- Attachment { attachment with content };
                        r.r_extra_content <-
                          sanitize_attachment_extra r.r_extra_content;
                        save t;
                        changed t r;
                        Ok Updated
                    | _ -> attachment_error "request is not an attachment")))
        | Sending -> (
            match
              attachment_caption_content r ~caption ~formatted_body ~format
                ~mentions
            with
            | Error _ as error -> error
            | Ok content ->
                let txn_id =
                  match r.r_pending_edit with
                  | Some edit -> edit.txn_id
                  | None -> fresh_txn_id t
                in
                r.r_pending_edit <- Some { content; txn_id };
                save t;
                changed t r;
                Ok Deferred))

let payload r =
  match r.r_kind with
  | Redaction { event_id; reason } -> Redact { event_id; reason }
  | Upload_request upload ->
      Upload_payload
        {
          role = upload.role;
          content_type = upload.content_type;
          filename = upload.filename;
          data = upload.data;
          encrypted_metadata = upload.encrypted_metadata;
        }
  | Attachment _ ->
      Send { event_type = "m.room.message"; content = content_of_kind r }
  | Event _ | Reaction _ ->
      Send { event_type = event_type_of_kind r; content = content_of_kind r }

(* One PUT, one decode. The transaction id comes from the request, so a retry,
   and an encrypted resend of the same request, is idempotent. *)
let put_and_decode client ~path ~content =
  match Client.Http.encode_body Matrix_proto.Json.Codec.json content with
  | Error e -> Error e
  | Ok body -> (
      match Client.Http.put client ~path ~body () with
      | Error e -> Error e
      | Ok reply -> (
          match
            Client.Http.decode_response Messages.send_response_jsont reply
          with
          | Error e -> Error e
          | Ok resp -> Ok resp.Messages.event_id))

type progress_source = {
  source : Eio.Flow.source_ty Eio.Resource.t;
  total : int64;
  mutable current : int64;
  report : int64 -> unit;
}

module Progress_source = struct
  type t = progress_source

  let read_methods = []

  let single_read t buf =
    let n =
      Io_context.with_context "reading upload source for send-queue progress"
        (fun () -> Eio.Flow.single_read t.source buf)
    in
    let current = Int64.add t.current (Int64.of_int n) in
    t.current <- min current t.total;
    t.report t.current;
    n
end

let make_progress_source ~source ~total ~report =
  let state = { source; total; current = 0L; report } in
  Eio.Resource.T (state, Eio.Flow.Pi.source (module Progress_source))

let send_route = Route.v "/rooms/{room_id}/send/{event_type}/{transaction_id}"
let redact_route = Route.v "/rooms/{room_id}/redact/{event_id}/{transaction_id}"

let send_as ~event_type ~content client r =
  let path =
    Route.expand_exn send_route
      [
        ("room_id", Id.Room_id.to_string r.r_room_id);
        ("event_type", event_type);
        ("transaction_id", r.r_txn_id);
      ]
  in
  put_and_decode client ~path ~content

let http_send client r =
  match payload r with
  | Redact { event_id; _ } ->
      let path =
        Route.expand_exn redact_route
          [
            ("room_id", Id.Room_id.to_string r.r_room_id);
            ("event_id", Id.Event_id.to_string event_id);
            ("transaction_id", r.r_txn_id);
          ]
      in
      put_and_decode client ~path ~content:(content_of_kind r)
  | Send { event_type; content } -> (
      match r.r_kind with
      | Attachment _ -> (
          match content_for_send r with
          | Error _ as error -> error
          | Ok (_, content) -> send_as ~event_type ~content client r)
      | _ -> send_as ~event_type ~content client r)
  | Upload_payload _ ->
      Error (Error.Network_error "upload request requires the upload sender")

let upload_data_for_send t r =
  match (r.r_upload_cache_key, t.q_media_store) with
  | None, _ -> Ok r
  | Some _, None ->
      Error (Error.Policy_denied "cache-backed upload has no media store")
  | Some key, Some media_store -> (
      let now = Ptime_clock.now () in
      match Media_store.get ~now media_store key with
      | Ok (Some data) -> (
          match r.r_kind with
          | Upload_request upload ->
              Ok { r with r_kind = Upload_request { upload with data } }
          | _ -> Ok r)
      | Ok None ->
          Error
            (Error.Policy_denied
               "send queue media cache entry is missing (upload wedged)")
      | Error error -> Error error)

let http_upload ?(on_progress = fun _ -> ()) client r =
  match r.r_kind with
  | Upload_request { content_type; filename; data; encrypted_metadata; _ } -> (
      let total = Int64.of_int (String.length data) in
      let source = Eio.Flow.string_source data in
      let source = make_progress_source ~source ~total ~report:on_progress in
      match
        Media.upload_stream client ~content_type ~source ~length:total ?filename
          ()
      with
      | Error e -> Error e
      | Ok mxc ->
          Ok
            (match encrypted_metadata with
            | None -> Clear_upload { mxc }
            | Some metadata -> Encrypted_upload { mxc; metadata }))
  | _ -> Error (Error.Network_error "not an upload request")

let move_upload_cache t r mxc =
  match (t.q_media_store, r.r_upload_cache_key) with
  | Some media_store, Some from_key
    when Media_store.is_local_uri from_key.uri
         || Media.Mxc.equal from_key.uri mxc -> (
      let to_key = Media_store.{ uri = mxc; format = Media_store.File } in
      let now = Ptime.epoch in
      (* The result is already durable when this runs.  Treat an existing
         remote entry as a completed move, which is what makes recovery after
         a crash between replace_key and queue persistence idempotent. *)
      let remote_exists () =
        match Media_store.get ~now media_store to_key with
        | Ok (Some _) -> Ok true
        | Ok None -> Ok false
        | Error error -> Error error
      in
      let move () =
        if Media.Mxc.equal from_key.uri mxc then remote_exists ()
        else
          match Media_store.get ~now media_store from_key with
          | Error error -> Error error
          | Ok (Some _) -> (
              match
                Media_store.replace_key media_store ~from_:from_key ~to_:to_key
              with
              | Error error -> Error error
              | Ok () -> Ok true)
          | Ok None -> remote_exists ()
      in
      match move () with
      | Error error -> Error error
      | Ok false ->
          Error
            (Error.Policy_denied
               "send queue media cache entry missing after upload")
      | Ok true -> (
          r.r_upload_cache_key <- Some to_key;
          match Media_store.set_ignore_retention media_store to_key false with
          | Error error -> Error error
          | Ok () -> (
              match Media_store.unprotect media_store to_key with
              | Error error -> Error error
              | Ok () -> Ok ())))
  | Some _, Some _ ->
      Error
        (Error.Policy_denied
           "send queue media cache URI does not match its upload result")
  | _ -> Ok ()

let propagate_upload_result t r upload_result =
  match move_upload_cache t r (upload_result_mxc upload_result) with
  | Error error ->
      r.r_last_error <- Some error;
      r.r_status <- Wedged;
      save t;
      changed t r;
      Error error
  | Ok () ->
      r.r_status <- Uploaded upload_result;
      r.r_last_error <- None;
      let changed_children =
        List.filter_map
          (fun child ->
            if List.mem r.r_id child.r_dependencies then begin
              child.r_dependencies <-
                List.filter (( <> ) r.r_id) child.r_dependencies;
              child.r_resolved <-
                List.sort
                  (fun (a, _) (b, _) -> Int.compare a b)
                  ((r.r_id, Upload upload_result) :: child.r_resolved);
              Some child
            end
            else None)
          (all t)
      in
      remove t r;
      (* [r.r_upload_result] is intentionally left populated on the detached
         object.  The persisted representation is omitted by [Uploaded], while
         observers retaining the request still see the exact typed result. *)
      save t;
      Ok changed_children

let resume_upload_result t r upload_result =
  r.r_status <- Sending;
  r.r_attempts <- r.r_attempts + 1;
  changed t r;
  if r.r_cancel_requested then
    begin match move_upload_cache t r (upload_result_mxc upload_result) with
    | Error error ->
        r.r_last_error <- Some error;
        r.r_status <- Wedged;
        save t;
        changed t r;
        Failed error
    | Ok () ->
        cancel_pending t r;
        Failed (Error.Network_error "upload was cancelled")
    end
  else
    match propagate_upload_result t r upload_result with
    | Error error -> Failed error
    | Ok changed_children ->
        List.iter (changed t) changed_children;
        changed t r;
        Uploaded_ok upload_result

let send_one t ?(send = http_send) ?(upload = http_upload) ?on_progress client r
    =
  match r.r_status with
  | _ when r.r_detached ->
      Failed (Error.Network_error "request belongs to a forgotten room")
  | Sent e -> Sent_ok e
  | Uploaded result -> Uploaded_ok result
  | Cancelled -> Failed (Error.Network_error "request was cancelled")
  | Wedged ->
      Failed
        (Option.value r.r_last_error
           ~default:(Error.Network_error "request is wedged"))
  | (Pending | Sending) when Option.is_some r.r_upload_result ->
      resume_upload_result t r (Option.get r.r_upload_result)
  | Pending | Sending -> (
      r.r_status <- Sending;
      r.r_attempts <- r.r_attempts + 1;
      changed t r;
      let upload_total = ref None in
      let last_progress = ref (-1L) in
      let notify current_bytes total_bytes =
        if current_bytes <> !last_progress then begin
          last_progress := current_bytes;
          let progress = { current_bytes; total_bytes } in
          List.iter (fun f -> f r progress) t.q_on_progress;
          match on_progress with None -> () | Some f -> f r progress
        end
      in
      let report current_bytes total_bytes =
        (* Reserve [total] for the post-persistence terminal notification. A
           transport callback is allowed to report the complete body early,
           but observers must not mistake that for a durable upload result. *)
        if total_bytes > 0L && current_bytes < total_bytes then
          let current_bytes = max 0L (max !last_progress current_bytes) in
          notify current_bytes total_bytes
      in
      let report_terminal total_bytes = notify total_bytes total_bytes in
      let result =
        try
          match r.r_kind with
          | Upload_request _ ->
              Result.map
                (fun result -> `Upload result)
                (match upload_data_for_send t r with
                | Error error -> Error error
                | Ok upload_request ->
                    let total =
                      match upload_request.r_kind with
                      | Upload_request { data; _ } ->
                          Int64.of_int (String.length data)
                      | _ -> 0L
                    in
                    upload_total := Some total;
                    report 0L total;
                    let original_kind = r.r_kind in
                    (* Keep the original request identity visible to custom
                       upload callbacks (and cancellation races), while
                       temporarily substituting the cache bytes they should
                       consume. *)
                    r.r_kind <- upload_request.r_kind;
                    Fun.protect
                      ~finally:(fun () -> r.r_kind <- original_kind)
                      (fun () ->
                        upload
                          ~on_progress:(fun current -> report current total)
                          client r))
          | _ -> send client r |> Result.map (fun event_id -> `Event event_id)
        with exn ->
          (* An exception here, notably [Eio.Cancel.Cancelled] on the fiber
             this send runs in, must not leave [r] stuck at [Sending]
             forever with no way back: put it where the next attempt finds
             it, then propagate. *)
          let bt = Printexc.get_raw_backtrace () in
          if r.r_detached then finish_detached t r None
          else if r.r_cancel_requested then cancel_pending t r
          else begin
            r.r_status <- Pending;
            save t;
            changed t r
          end;
          Printexc.raise_with_backtrace exn bt
      in
      match result with
      | Ok (`Upload upload_result) ->
          if r.r_detached then begin
            finish_detached t r None;
            Failed (Error.Network_error "request belongs to a forgotten room")
          end
          else begin
            (* Persist the successful server result while the local cache key is
             still intact.  A crash after this save can therefore resume the
             cache transition and dependency propagation without another HTTP
             upload. *)
            r.r_upload_result <- Some upload_result;
            save t;
            if r.r_cancel_requested then
              begin match
                move_upload_cache t r (upload_result_mxc upload_result)
              with
              | Error error ->
                  r.r_last_error <- Some error;
                  r.r_status <- Wedged;
                  save t;
                  changed t r;
                  Failed error
              | Ok () ->
                  cancel_pending t r;
                  Failed (Error.Network_error "upload was cancelled")
              end
            else
              begin match propagate_upload_result t r upload_result with
              | Error error -> Failed error
              | Ok changed_children ->
                  Option.iter report_terminal !upload_total;
                  List.iter (changed t) changed_children;
                  changed t r;
                  Uploaded_ok upload_result
              end
          end
      | Ok (`Event event_id) ->
          if r.r_detached then begin
            finish_detached t r None;
            Failed (Error.Network_error "request belongs to a forgotten room")
          end
          else if r.r_cancel_requested then begin
            (* The original event landed while its caller was cancelling it.
               Reuse the request slot, but give the compensating redaction a
               transaction id allocated when the intent was recorded.  This
               single in-memory transition followed by [save] is the durable
               boundary between "parent unresolved" and "redaction pending". *)
            let redaction_txn_id =
              match r.r_cancel_txn_id with
              | Some txn_id -> txn_id
              | None -> fresh_txn_id t
            in
            let reason = r.r_cancel_reason in
            let children = descendants t r in
            List.iter
              (fun child ->
                child.r_status <- Cancelled;
                child.r_cancel_requested <- false;
                child.r_cancel_reason <- None;
                child.r_cancel_txn_id <- None;
                remove t child)
              children;
            r.r_kind <- Redaction { event_id; reason };
            r.r_txn_id <- redaction_txn_id;
            r.r_status <- Pending;
            r.r_attempts <- 0;
            r.r_last_error <- None;
            r.r_dependencies <- [];
            r.r_resolved <- [];
            r.r_pending_edit <- None;
            r.r_cancel_requested <- false;
            r.r_cancel_reason <- None;
            r.r_cancel_txn_id <- None;
            save t;
            changed t r;
            List.iter (changed t) children;
            Retry_in 0.
          end
          else begin
            let changed_children =
              List.filter_map
                (fun child ->
                  if List.mem r.r_id child.r_dependencies then begin
                    child.r_dependencies <-
                      List.filter (( <> ) r.r_id) child.r_dependencies;
                    child.r_resolved <-
                      List.sort
                        (fun (a, _) (b, _) -> Int.compare a b)
                        ((r.r_id, Event_id event_id) :: child.r_resolved);
                    Some child
                  end
                  else None)
                (all t)
            in
            match r.r_pending_edit with
            | Some edit -> (
                match
                  attachment_replacement_content r event_id edit.content
                with
                | Ok content ->
                    r.r_kind <- Event { event_type = "m.room.message"; content };
                    r.r_txn_id <- edit.txn_id;
                    r.r_status <- Pending;
                    r.r_attempts <- 0;
                    r.r_last_error <- None;
                    r.r_dependencies <- [];
                    r.r_resolved <- [];
                    r.r_extra_content <- None;
                    r.r_pending_edit <- None;
                    r.r_cancel_requested <- false;
                    r.r_cancel_reason <- None;
                    r.r_cancel_txn_id <- None;
                    save t;
                    List.iter (changed t) changed_children;
                    changed t r;
                    Retry_in 0.
                | Error error ->
                    Log.warn (fun m ->
                        m "Dropping invalid queued attachment edit: %s"
                          (Error.to_string error));
                    r.r_pending_edit <- None;
                    r.r_status <- Sent event_id;
                    r.r_last_error <- None;
                    remove t r;
                    save t;
                    List.iter (changed t) changed_children;
                    changed t r;
                    Sent_ok event_id)
            | None ->
                r.r_status <- Sent event_id;
                r.r_last_error <- None;
                remove t r;
                save t;
                List.iter (changed t) changed_children;
                changed t r;
                Sent_ok event_id
          end
      | Error e -> (
          r.r_last_error <- Some e;
          if r.r_detached then begin
            finish_detached t r (Some e);
            Failed e
          end
          else if r.r_cancel_requested then begin
            cancel_pending t r;
            Failed e
          end
          else
            let classified () =
              try classify t r e
              with exn ->
                (* Retry jitter consumes fallible secure randomness. Restore
                   [Pending] before propagating a source/cancellation failure,
                   otherwise the request is stranded in [Sending]. *)
                let bt = Printexc.get_raw_backtrace () in
                r.r_status <- Pending;
                save t;
                changed t r;
                Printexc.raise_with_backtrace exn bt
            in
            match classified () with
            | Sent_ok _ as o -> o
            | Uploaded_ok _ as o -> o
            | Retry_in d ->
                r.r_status <- Pending;
                save t;
                changed t r;
                Retry_in d
            | Failed e ->
                r.r_status <- Wedged;
                save t;
                changed t r;
                Failed e))

(* A redaction echo carries [redacts] in its content, as a room-version-11
   event does, even though [PUT /redact] puts the target in the path and only
   [reason] in the body. The content is the only place a reader of the echo
   can find what it redacts. *)
let echo_content t r =
  match r.r_kind with
  | Redaction { event_id; reason } ->
      jobject
        (("redacts", jstring (Id.Event_id.to_string event_id))
        ::
        (match reason with
        | Some reason -> [ ("reason", jstring reason) ]
        | None -> []))
  | Upload_request _ -> jobject []
  | Attachment { content; original_upload; thumbnail_upload } ->
      let base =
        match r.r_pending_edit with
        | None -> content_of_kind r
        | Some edit ->
            Json_codec.merge_extra_content edit.content
              ?extra_content:(sanitize_attachment_extra r.r_extra_content)
              ()
      in
      local_echo_attachment_content t r base original_upload thumbnail_upload
  | _ -> content_of_kind r

let local_echo t r =
  let unsigned =
    Event.Unsigned.make ~transaction_id:(Id.Transaction_id.v r.r_txn_id) ()
  in
  {
    Event.Raw_event.event_id = None;
    sender = t.q_user_id;
    origin_server_ts = r.r_created_at;
    type_ = Event.Event_type.of_string (event_type_of_kind r);
    state_key = None;
    redacts = None;
    content = echo_content t r;
    unsigned = Some unsigned;
    room_id = Some r.r_room_id;
  }
