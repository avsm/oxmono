(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type query
type get
type snippet_get
type changes
type set
type query_changes
type copy
type import
type parse

type mailbox_changes_response = Mail_mailbox.changes_response = {
  changes : Proto_method.changes_response;
  updated_properties : string list option;
}

type parse_error =
  | Method_error of Proto_error.Method_error.t
  | Json_error of Jsont.Error.t

let pp_parse_error ppf = function
  | Method_error e -> Proto_error.Method_error.pp ppf e
  | Json_error e -> Proto_error.pp_escaped ppf (Jsont.Error.to_string e)

let parse_error_to_string e = Format.asprintf "%a" pp_parse_error e

exception Parse_error of parse_error

let () =
  Printexc.register_printer (function
    | Parse_error e ->
        Some ("Jmap.Chain.Parse_error: " ^ parse_error_to_string e)
    | _ -> None)

(* Jsont.Error.msgf raises rather than returning, so an error made here has to
   be built rather than signalled: [parse] answers with a value. *)
let json_error fmt =
  Format.kasprintf
    (fun msg ->
      Json_error
        (Jsont.Error.make_msg Jsont.Error.Context.empty Jsont.Meta.none msg))
    fmt

type ('k, 'r) handle = {
  call_id : string;
  method_name : string;
  (* The properties the call asked for, by wire name, or [None] when it asked
     for every property or is not a /get. *)
  properties : string list option;
  read : Proto_invocation.t option -> ('r, parse_error) result;
}

let call_id h = h.call_id
let method_name h = h.method_name
let map_handle f h = { h with read = (fun inv -> Result.map f (h.read inv)) }

let attempt h =
  {
    h with
    read =
      (fun inv ->
        match h.read inv with
        | Ok v -> Ok (Ok v)
        | Error (Method_error e) -> Ok (Error e)
        | Error (Json_error _ as e) -> Error e);
  }

type id_source =
  | Ids of Proto_id.t list
  | Ref of Proto_invocation.result_reference

let ids l = Ids l
let id x = Ids [ x ]

let result_reference ~call_id ~method_name tokens =
  Proto_invocation.result_reference ~result_of:call_id ~name:method_name
    ~path:(Json_pointer.of_tokens tokens)

let make_ref ~call_id ~method_name tokens =
  Ref (result_reference ~call_id ~method_name tokens)

let from_query h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "ids" ]

let from_get_ids h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "list"; "*"; "id" ]

type _ id_property =
  | Thread_id : Mail_email.t id_property
  | Email_ids : Mail_thread.t id_property
  | Submission_email_id : Mail_submission.t id_property
  | Submission_identity_id : Mail_submission.t id_property
  | Submission_thread_id : Mail_submission.t id_property

let id_property_to_string : type r. r id_property -> string = function
  | Thread_id -> Mail_email.property_to_string `Thread_id
  | Email_ids -> Mail_thread.property_to_string `Email_ids
  | Submission_email_id -> Mail_submission.property_to_string `Email_id
  | Submission_identity_id -> Mail_submission.property_to_string `Identity_id
  | Submission_thread_id -> Mail_submission.property_to_string `Thread_id

(* RFC 8620 Section 5.1: a /get returns only the properties it was asked for,
   so a reference into a property left out of that list resolves to nothing
   and the server answers invalidResultReference.  The same section makes
   "id" the exception: it "is always returned, even if not explicitly
   requested". *)
let check_requested_property h name =
  match h.properties with
  | None -> ()
  | Some props ->
      if
        (not (String.equal name "id"))
        && not (List.exists (String.equal name) props)
      then
        invalid_arg
          (Printf.sprintf
             "Jmap.Chain: the %s did not ask for the property %S, so the \
              server would answer the result reference with \
              invalidResultReference"
             h.method_name name)

(* [name] reaches {!from_get_field_raw} from the caller, so a string that is
   no JSON Pointer token is bad input rather than a bug here. *)
let field_ref h name =
  check_requested_property h name;
  match
    make_ref ~call_id:h.call_id ~method_name:h.method_name [ "list"; "*"; name ]
  with
  | r -> r
  | exception Jsont.Error e -> invalid_arg (Jsont.Error.to_string e)

let from_get_field h p = field_ref h (id_property_to_string p)
let from_get_field_raw h field = field_ref h field

let from_changes_created h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "created" ]

let from_changes_updated h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "updated" ]

let from_changes_destroyed h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "destroyed" ]

let from_changes_updated_properties
    (h : (changes, mailbox_changes_response) handle) =
  result_reference ~call_id:h.call_id ~method_name:h.method_name
    [ "updatedProperties" ]

let from_query_changes_removed h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "removed" ]

let from_query_changes_added h =
  make_ref ~call_id:h.call_id ~method_name:h.method_name [ "added"; "*"; "id" ]

type state = {
  mutable next_id : int;
  mutable invocations : Proto_invocation.t list;
}

type 'a t = state -> 'a

let return x _state = x

let bind m f state =
  let a = m state in
  f a state

let map f m state = f (m state)

let both a b state =
  let x = a state in
  let y = b state in
  (x, y)

let ( let* ) = bind
let ( let+ ) m f = map f m
let attempt_call c = map attempt c
let ( and* ) = both
let ( and+ ) = both

let fresh_call_id state =
  let id = Printf.sprintf "c%d" state.next_id in
  state.next_id <- state.next_id + 1;
  id

let record_invocation inv state = state.invocations <- inv :: state.invocations

let build ~capabilities chain =
  let state = { next_id = 0; invocations = [] } in
  let result = chain state in
  let request =
    Proto_request.create ~using:capabilities
      ~method_calls:(List.rev state.invocations)
      ()
  in
  (request, result)

let build_request ~capabilities chain = fst (build ~capabilities chain)
let build_handles ~capabilities chain = snd (build ~capabilities chain)

let json_obj fields =
  let names = Hashtbl.create (List.length fields) in
  List.iter
    (fun (name, _) ->
      if Hashtbl.mem names name then
        invalid_arg (Printf.sprintf "Jmap.Chain: duplicate JSON member %S" name)
      else Hashtbl.add names name ())
    fields;
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

(* A value the client built that cannot be encoded is a programming error
   here, not a message from the server, so it is raised rather than turned
   into an empty object that would silently change the meaning of the
   call. *)
let encode_exn jsont value =
  match Jsont.Json.encode' jsont value with
  | Ok j -> j
  | Error e -> invalid_arg ("Jmap.Chain: " ^ Jsont.Error.to_string e)

let encode_list_exn jsont values = encode_exn (Jsont.list jsont) values
let json_of_int n = encode_exn Proto_int53.Signed.jsont n
let json_of_uint n = encode_exn Proto_int53.Unsigned.jsont n

let json_of_positive_uint name n =
  if n <= 0L then invalid_arg ("Jmap.Chain." ^ name ^ ": must be positive");
  json_of_uint n

let json_of_id i = Jsont.Json.string (Proto_id.to_string i)
let json_of_id_list l = Jsont.Json.list (List.map json_of_id l)
let json_of_string_list l = Jsont.Json.list (List.map Jsont.Json.string l)
let json_of_ref r = encode_exn Proto_invocation.result_reference_jsont r

let check_arguments json =
  match json with
  | Jsont.Object (members, _) -> (
      let names = Hashtbl.create (List.length members) in
      let duplicate =
        List.find_map
          (fun ((name, _), _) ->
            if Hashtbl.mem names name then Some name
            else begin
              Hashtbl.add names name ();
              None
            end)
          members
      in
      match duplicate with
      | Some name -> Error (Printf.sprintf "Duplicate argument %S" name)
      | None -> (
          let collision ((name, _), _) =
            if String.length name > 0 && Char.equal name.[0] '#' then
              let bare = String.sub name 1 (String.length name - 1) in
              if Hashtbl.mem names bare then Some bare else None
            else None
          in
          match List.find_map collision members with
          | None -> Ok ()
          | Some name ->
              Error
                (Printf.sprintf
                   "Argument %S is given in both normal and referenced form \
                    (%S and %S); RFC 8620 Section 3.7 requires the server to \
                    reject this with invalidArguments"
                   name name ("#" ^ name))))
  | _ -> Error "Method arguments must be a JSON object"

(* The [properties] argument of a /get, from the typed property variants, the
   raw wire names, or both concatenated (typed first). [None] when neither is
   given, which RFC 8620 Section 5.1 reads as "all properties". *)
let properties_arg ~to_string typed raw =
  match (typed, raw) with
  | None, None -> None
  | _ ->
      Some
        (List.map to_string (Option.value typed ~default:[])
        @ Option.value raw ~default:[])

let add_properties_arg ~name args = function
  | None -> args
  | Some props -> (name, json_of_string_list props) :: args

let add_id_source_arg ~name args = function
  | None -> args
  | Some (Ids l) -> (name, json_of_id_list l) :: args
  | Some (Ref r) -> ("#" ^ name, json_of_ref r) :: args

let add_opt args name f = function
  | None -> args
  | Some v -> (name, f v) :: args

let build_query_args ~account_id ?filter ~filter_jsont ?sort ?position ?anchor
    ?anchor_offset ?limit ?calculate_total () =
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_opt args "filter" (encode_exn filter_jsont) filter in
  let args =
    add_opt args "sort" (encode_list_exn Proto_filter.comparator_jsont) sort
  in
  let args = add_opt args "position" json_of_int position in
  let args = add_opt args "anchor" json_of_id anchor in
  let args = add_opt args "anchorOffset" json_of_int anchor_offset in
  let args = add_opt args "limit" json_of_uint limit in
  let args = add_opt args "calculateTotal" Jsont.Json.bool calculate_total in
  args

let build_changes_args ~account_id ~since_state ?max_changes () =
  let args =
    [
      ("accountId", json_of_id account_id);
      ("sinceState", Jsont.Json.string since_state);
    ]
  in
  add_opt args "maxChanges" (json_of_positive_uint "maxChanges") max_changes

let build_query_changes_args ~account_id ~since_query_state ?filter
    ~filter_jsont ?sort ?max_changes ?up_to_id ?calculate_total () =
  let args =
    [
      ("accountId", json_of_id account_id);
      ("sinceQueryState", Jsont.Json.string since_query_state);
    ]
  in
  let args = add_opt args "filter" (encode_exn filter_jsont) filter in
  let args =
    add_opt args "sort" (encode_list_exn Proto_filter.comparator_jsont) sort
  in
  let args = add_opt args "maxChanges" json_of_uint max_changes in
  let args = add_opt args "upToId" json_of_id up_to_id in
  let args = add_opt args "calculateTotal" Jsont.Json.bool calculate_total in
  args

let add_set_args ~create_jsont ?create ?update ?destroy args =
  let args =
    match create with
    | None | Some [] -> args
    | Some items ->
        ("create", encode_exn (Proto_json_map.of_creation create_jsont) items)
        :: args
  in
  let args =
    match update with
    | None | Some [] -> args
    | Some items ->
        ( "update",
          encode_exn (Proto_json_map.of_id_or_creation Proto_patch.jsont) items
        )
        :: args
  in
  add_id_source_arg ~name:"destroy" args destroy

let build_set_args ~account_id ?if_in_state ~create_jsont ?create ?update
    ?destroy () =
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_opt args "ifInState" Jsont.Json.string if_in_state in
  add_set_args ~create_jsont ?create ?update ?destroy args

(* RFC 8620 Section 3.2: "A method may also return more than one response
   [...] all of the responses have the same method call id".  Section 3.6.2
   makes a failure a response named "error" carrying that same id. *)
let read_response ~call_id ~method_name codec = function
  | None ->
      Error
        (json_error "No %s response found for call_id: %s" method_name call_id)
  | Some inv when Proto_response.is_error inv ->
      Error
        (match Proto_response.error inv with
        | Some (Ok e) -> Method_error e
        | Some (Error _) | None ->
            json_error "Undecodable error response for call_id: %s" call_id)
  | Some (inv : Proto_invocation.t) ->
      Result.map_error
        (fun e -> Json_error e)
        (Jsont.Json.decode' codec inv.arguments)

let invoke ~name ~arguments state =
  let call_id = fresh_call_id state in
  let inv = Proto_invocation.create ~name ~arguments ~method_call_id:call_id in
  record_invocation inv state;
  call_id

let make ~name ~args ?properties codec state =
  let call_id = invoke ~name ~arguments:(json_obj args) state in
  {
    call_id;
    method_name = name;
    properties;
    read = read_response ~call_id ~method_name:name codec;
  }

(* RFC 8620 Section 5.1: "state: The state string [...] This is the state of
   the data type in the account", returned whatever the ids asked for. *)
let state_read ~name ~account_id record_jsont state =
  let args =
    [ ("accountId", json_of_id account_id); ("ids", json_of_id_list []) ]
  in
  map_handle
    (fun (r : _ Proto_method.get_response) -> r.state)
    (make ~name ~args (Proto_method.get_response_jsont record_jsont) state)

let blob_copy ~from_account_id ~account_id ~blob_ids () state =
  if Proto_id.equal from_account_id account_id then
    invalid_arg
      "Jmap.Chain.blob_copy: source and destination accounts must differ";
  let args =
    [
      ("fromAccountId", json_of_id from_account_id);
      ("accountId", json_of_id account_id);
    ]
  in
  let args = add_id_source_arg ~name:"blobIds" args (Some blob_ids) in
  make ~name:"Blob/copy" ~args Proto_blob.copy_response_jsont state

let push_subscription_get ?ids ?properties () state =
  let args = add_id_source_arg ~name:"ids" [] ids in
  let args = add_properties_arg ~name:"properties" args properties in
  make ~name:"PushSubscription/get" ~args ?properties
    Proto_push.get_response_jsont state

let push_subscription_set ?create ?update ?destroy () state =
  let args =
    add_set_args ~create_jsont:Proto_push.create_args_jsont ?create ?update
      ?destroy []
  in
  make ~name:"PushSubscription/set" ~args Proto_push.set_response_jsont state

let email_query ~account_id ?filter ?sort ?position ?anchor ?anchor_offset
    ?limit ?calculate_total ?collapse_threads () state =
  let args =
    build_query_args ~account_id ?filter ~filter_jsont:Mail_email.filter_jsont
      ?sort ?position ?anchor ?anchor_offset ?limit ?calculate_total ()
  in
  let args = add_opt args "collapseThreads" Jsont.Json.bool collapse_threads in
  make ~name:"Email/query" ~args Proto_method.query_response_jsont state

let build_email_read_args ?body_properties ?body_properties_raw
    ?fetch_text_body_values ?fetch_html_body_values ?fetch_all_body_values
    ?max_body_value_bytes args =
  let args =
    add_properties_arg ~name:"bodyProperties" args
      (properties_arg ~to_string:Mail_email.body_part_property_to_string
         body_properties body_properties_raw)
  in
  let args =
    add_opt args "fetchTextBodyValues" Jsont.Json.bool fetch_text_body_values
  in
  let args =
    add_opt args "fetchHTMLBodyValues" Jsont.Json.bool fetch_html_body_values
  in
  let args =
    add_opt args "fetchAllBodyValues" Jsont.Json.bool fetch_all_body_values
  in
  add_opt args "maxBodyValueBytes" json_of_uint max_body_value_bytes

let email_get ~account_id ?ids ?properties ?properties_raw ?body_properties
    ?body_properties_raw ?fetch_text_body_values ?fetch_html_body_values
    ?fetch_all_body_values ?max_body_value_bytes () state =
  let props =
    properties_arg ~to_string:Mail_email.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  let args =
    build_email_read_args ?body_properties ?body_properties_raw
      ?fetch_text_body_values ?fetch_html_body_values ?fetch_all_body_values
      ?max_body_value_bytes args
  in
  make ~name:"Email/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_email.jsont)
    state

let email_state ~account_id state =
  state_read ~name:"Email/get" ~account_id Mail_email.jsont state

let email_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"Email/changes" ~args Proto_method.changes_response_jsont state

let email_query_changes ~account_id ~since_query_state ?filter ?sort
    ?max_changes ?up_to_id ?calculate_total ?collapse_threads () state =
  let args =
    build_query_changes_args ~account_id ~since_query_state ?filter
      ~filter_jsont:Mail_email.filter_jsont ?sort ?max_changes ?up_to_id
      ?calculate_total ()
  in
  let args = add_opt args "collapseThreads" Jsont.Json.bool collapse_threads in
  make ~name:"Email/queryChanges" ~args
    Proto_method.query_changes_response_jsont state

let email_set ~account_id ?if_in_state ?create ?update ?destroy () state =
  let args =
    build_set_args ~account_id ?if_in_state ~create_jsont:Mail_email.jsont
      ?create ?update ?destroy ()
  in
  make ~name:"Email/set" ~args
    (Proto_method.set_response_jsont Mail_email.jsont)
    state

(* RFC 8620 Section 5.4: the /copy create map is keyed by creation id and each
   object "MUST include the id property", naming the record to copy in the
   source account. Which other properties a copy may override is per data type:
   RFC 8621 Section 4.7 names three for an Email, and RFC 9610 Section 3
   requires a copied ContactCard to name a book of the destination account. *)
let copy_create_object ~caller ~overridable ~source_id overrides =
  List.iter
    (fun (n, _) ->
      if not (List.mem n overridable) then
        invalid_arg
          (Printf.sprintf "Jmap.Chain.%s: property %S cannot be overridden"
             caller n))
    overrides;
  json_obj (("id", json_of_id source_id) :: overrides)

let email_copy ~from_account_id ~account_id ?if_from_in_state ?if_in_state
    ~create ?on_success_destroy_original ?destroy_from_if_in_state () state =
  if Proto_id.equal from_account_id account_id then
    invalid_arg
      "Jmap.Chain.email_copy: source and destination accounts must differ";
  if List.is_empty create then
    invalid_arg "Jmap.Chain.email_copy: create must not be empty";
  let args =
    [
      ("fromAccountId", json_of_id from_account_id);
      ("accountId", json_of_id account_id);
    ]
  in
  let args = add_opt args "ifFromInState" Jsont.Json.string if_from_in_state in
  let args = add_opt args "ifInState" Jsont.Json.string if_in_state in
  let args =
    ( "create",
      encode_exn
        (Proto_json_map.of_creation Jsont.json)
        (List.map
           (fun (cid, source_id, overrides) ->
             ( cid,
               copy_create_object ~caller:"email_copy"
                 ~overridable:[ "mailboxIds"; "keywords"; "receivedAt" ]
                 ~source_id overrides ))
           create) )
    :: args
  in
  let args =
    add_opt args "onSuccessDestroyOriginal" Jsont.Json.bool
      on_success_destroy_original
  in
  let args =
    add_opt args "destroyFromIfInState" Jsont.Json.string
      destroy_from_if_in_state
  in
  make ~name:"Email/copy" ~args
    (Proto_method.copy_response_jsont Mail_email.jsont)
    state

let email_import ~account_id ?if_in_state ~emails () state =
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_opt args "ifInState" Jsont.Json.string if_in_state in
  let args =
    ( "emails",
      encode_exn
        (Proto_json_map.of_creation Mail_email.Import.email_jsont)
        emails )
    :: args
  in
  make ~name:"Email/import" ~args Mail_email.Import.response_jsont state

let email_parse ~account_id ~blob_ids ?properties ?properties_raw
    ?body_properties ?body_properties_raw ?fetch_text_body_values
    ?fetch_html_body_values ?fetch_all_body_values ?max_body_value_bytes ()
    state =
  let props =
    properties_arg ~to_string:Mail_email.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"blobIds" args (Some blob_ids) in
  let args = add_properties_arg ~name:"properties" args props in
  let args =
    build_email_read_args ?body_properties ?body_properties_raw
      ?fetch_text_body_values ?fetch_html_body_values ?fetch_all_body_values
      ?max_body_value_bytes args
  in
  make ~name:"Email/parse" ~args Mail_email.Parse.response_jsont state

let thread_get ~account_id ?ids ?properties ?properties_raw () state =
  let props =
    properties_arg ~to_string:Mail_thread.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"Thread/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_thread.jsont)
    state

let thread_state ~account_id state =
  state_read ~name:"Thread/get" ~account_id Mail_thread.jsont state

let thread_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"Thread/changes" ~args Proto_method.changes_response_jsont state

let mailbox_query ~account_id ?filter ?sort ?position ?anchor ?anchor_offset
    ?limit ?calculate_total ?sort_as_tree ?filter_as_tree () state =
  let args =
    build_query_args ~account_id ?filter ~filter_jsont:Mail_mailbox.filter_jsont
      ?sort ?position ?anchor ?anchor_offset ?limit ?calculate_total ()
  in
  let args = add_opt args "sortAsTree" Jsont.Json.bool sort_as_tree in
  let args = add_opt args "filterAsTree" Jsont.Json.bool filter_as_tree in
  make ~name:"Mailbox/query" ~args Proto_method.query_response_jsont state

let mailbox_get ~account_id ?ids ?properties ?properties_raw ?properties_ref ()
    state =
  (* RFC 8620 Section 3.7: "properties" and "#properties" in the same
     arguments object is an invalidArguments error. *)
  (match (properties, properties_raw, properties_ref) with
  | Some _, _, Some _ | _, Some _, Some _ ->
      invalid_arg
        "Chain.mailbox_get: ~properties/~properties_raw and ~properties_ref \
         are mutually exclusive (RFC 8620 Section 3.7)"
  | _ -> ());
  let props =
    properties_arg ~to_string:Mail_mailbox.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  let args = add_opt args "#properties" json_of_ref properties_ref in
  make ~name:"Mailbox/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_mailbox.jsont)
    state

let mailbox_state ~account_id state =
  state_read ~name:"Mailbox/get" ~account_id Mail_mailbox.jsont state

let mailbox_by_role ~account_id ?properties role state =
  let q =
    mailbox_query ~account_id
      ~filter:
        (Proto_filter.Condition
           { Mail_mailbox.Filter_condition.empty with role = Some (Some role) })
      () state
  in
  mailbox_get ~account_id ~ids:(from_query q) ?properties () state

let mailbox_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"Mailbox/changes" ~args Mail_mailbox.changes_response_jsont state

let mailbox_query_changes ~account_id ~since_query_state ?filter ?sort
    ?max_changes ?up_to_id ?calculate_total () state =
  let args =
    build_query_changes_args ~account_id ~since_query_state ?filter
      ~filter_jsont:Mail_mailbox.filter_jsont ?sort ?max_changes ?up_to_id
      ?calculate_total ()
  in
  make ~name:"Mailbox/queryChanges" ~args
    Proto_method.query_changes_response_jsont state

let mailbox_set ~account_id ?if_in_state ?create ?update ?destroy
    ?on_destroy_remove_emails () state =
  let args =
    build_set_args ~account_id ?if_in_state ~create_jsont:Mail_mailbox.jsont
      ?create ?update ?destroy ()
  in
  let args =
    add_opt args "onDestroyRemoveEmails" Jsont.Json.bool
      on_destroy_remove_emails
  in
  make ~name:"Mailbox/set" ~args
    (Proto_method.set_response_jsont Mail_mailbox.jsont)
    state

let identity_get ~account_id ?ids ?properties_raw () state =
  let props = properties_arg ~to_string:Fun.id None properties_raw in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"Identity/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_identity.jsont)
    state

let identity_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"Identity/changes" ~args Proto_method.changes_response_jsont state

let identity_set ~account_id ?if_in_state ?create ?update ?destroy () state =
  let args =
    build_set_args ~account_id ?if_in_state ~create_jsont:Mail_identity.jsont
      ?create ?update ?destroy ()
  in
  make ~name:"Identity/set" ~args
    (Proto_method.set_response_jsont Mail_identity.jsont)
    state

let email_submission_query ~account_id ?filter ?sort ?position ?anchor
    ?anchor_offset ?limit ?calculate_total () state =
  let args =
    build_query_args ~account_id ?filter
      ~filter_jsont:Mail_submission.filter_jsont ?sort ?position ?anchor
      ?anchor_offset ?limit ?calculate_total ()
  in
  make ~name:"EmailSubmission/query" ~args Proto_method.query_response_jsont
    state

let email_submission_get ~account_id ?ids ?properties ?properties_raw () state =
  let props =
    properties_arg ~to_string:Mail_submission.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"EmailSubmission/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_submission.jsont)
    state

let email_submission_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"EmailSubmission/changes" ~args Proto_method.changes_response_jsont
    state

let email_submission_query_changes ~account_id ~since_query_state ?filter ?sort
    ?max_changes ?up_to_id ?calculate_total () state =
  let args =
    build_query_changes_args ~account_id ~since_query_state ?filter
      ~filter_jsont:Mail_submission.filter_jsont ?sort ?max_changes ?up_to_id
      ?calculate_total ()
  in
  make ~name:"EmailSubmission/queryChanges" ~args
    Proto_method.query_changes_response_jsont state

let email_submission_set ~account_id ?if_in_state ?create ?update ?destroy
    ?on_success_update_email ?on_success_destroy_email () state =
  let args =
    build_set_args ~account_id ?if_in_state ~create_jsont:Mail_submission.jsont
      ?create ?update ?destroy ()
  in
  (* RFC 8621 Section 7.5: "onSuccessUpdateEmail: Id[PatchObject]|null", keyed
     by the EmailSubmission id or its creation reference. *)
  let args =
    match on_success_update_email with
    | None | Some [] -> args
    | Some items ->
        ( "onSuccessUpdateEmail",
          encode_exn (Proto_json_map.of_id_or_creation Proto_patch.jsont) items
        )
        :: args
  in
  let args =
    match on_success_destroy_email with
    | None | Some [] -> args
    | Some l -> ("onSuccessDestroyEmail", json_of_id_list l) :: args
  in
  make ~name:"EmailSubmission/set" ~args
    (Proto_method.set_response_jsont Mail_submission.jsont)
    state

let search_snippet_get ~account_id ~filter ~email_ids () state =
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = ("filter", encode_exn Mail_email.filter_jsont filter) :: args in
  let args = add_id_source_arg ~name:"emailIds" args (Some email_ids) in
  make ~name:"SearchSnippet/get" ~args Mail_snippet.get_response_jsont state

let vacation_response_get ~account_id ?properties_raw () state =
  let props = properties_arg ~to_string:Fun.id None properties_raw in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"VacationResponse/get" ~args ?properties:props
    (Proto_method.get_response_jsont Mail_vacation.jsont)
    state

let vacation_response_set ~account_id ?if_in_state ~update () state =
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_opt args "ifInState" Jsont.Json.string if_in_state in
  let args =
    ("update", json_obj [ ("singleton", Proto_patch.to_json update) ]) :: args
  in
  make ~name:"VacationResponse/set" ~args
    (Proto_method.set_response_jsont Mail_vacation.jsont)
    state

let address_book_get ~account_id ?ids ?properties ?properties_raw () state =
  let props =
    properties_arg ~to_string:Contacts_addressbook.property_to_string properties
      properties_raw
  in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"AddressBook/get" ~args ?properties:props
    (Proto_method.get_response_jsont Contacts_addressbook.jsont)
    state

let address_book_state ~account_id state =
  state_read ~name:"AddressBook/get" ~account_id Contacts_addressbook.jsont
    state

let address_book_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"AddressBook/changes" ~args Proto_method.changes_response_jsont
    state

let address_book_set ~account_id ?if_in_state ?create ?update ?destroy
    ?on_destroy_remove_contents ?on_success_set_is_default () state =
  let args =
    build_set_args ~account_id ?if_in_state
      ~create_jsont:Contacts_addressbook.jsont ?create ?update ?destroy ()
  in
  let args =
    add_opt args "onDestroyRemoveContents" Jsont.Json.bool
      on_destroy_remove_contents
  in
  let args =
    add_opt args "onSuccessSetIsDefault" json_of_id on_success_set_is_default
  in
  make ~name:"AddressBook/set" ~args
    (Proto_method.set_response_jsont Contacts_addressbook.jsont)
    state

let contact_card_get ~account_id ?ids ?properties () state =
  let props = properties_arg ~to_string:Fun.id None properties in
  let args = [ ("accountId", json_of_id account_id) ] in
  let args = add_id_source_arg ~name:"ids" args ids in
  let args = add_properties_arg ~name:"properties" args props in
  make ~name:"ContactCard/get" ~args ?properties:props
    (Proto_method.get_response_jsont Contacts_card.jsont)
    state

let contact_card_state ~account_id state =
  state_read ~name:"ContactCard/get" ~account_id Contacts_card.jsont state

let contact_card_changes ~account_id ~since_state ?max_changes () state =
  let args = build_changes_args ~account_id ~since_state ?max_changes () in
  make ~name:"ContactCard/changes" ~args Proto_method.changes_response_jsont
    state

let contact_card_query ~account_id ?filter ?sort ?position ?anchor
    ?anchor_offset ?limit ?calculate_total () state =
  let args =
    build_query_args ~account_id ?filter
      ~filter_jsont:Contacts_card.filter_jsont ?sort ?position ?anchor
      ?anchor_offset ?limit ?calculate_total ()
  in
  make ~name:"ContactCard/query" ~args Proto_method.query_response_jsont state

let contact_card_query_changes ~account_id ~since_query_state ?filter ?sort
    ?max_changes ?up_to_id ?calculate_total () state =
  let args =
    build_query_changes_args ~account_id ~since_query_state ?filter
      ~filter_jsont:Contacts_card.filter_jsont ?sort ?max_changes ?up_to_id
      ?calculate_total ()
  in
  make ~name:"ContactCard/queryChanges" ~args
    Proto_method.query_changes_response_jsont state

let contact_card_set ~account_id ?if_in_state ?create ?update ?destroy () state
    =
  let args =
    build_set_args ~account_id ?if_in_state ~create_jsont:Contacts_card.jsont
      ?create ?update ?destroy ()
  in
  make ~name:"ContactCard/set" ~args
    (Proto_method.set_response_jsont Contacts_card.jsont)
    state

let contact_card_copy ~from_account_id ~account_id ?if_from_in_state
    ?if_in_state ~create ?on_success_destroy_original ?destroy_from_if_in_state
    () state =
  if Proto_id.equal from_account_id account_id then
    invalid_arg
      "Jmap.Chain.contact_card_copy: source and destination accounts must \
       differ";
  if List.is_empty create then
    invalid_arg "Jmap.Chain.contact_card_copy: create must not be empty";
  let args =
    [
      ("fromAccountId", json_of_id from_account_id);
      ("accountId", json_of_id account_id);
    ]
  in
  let args = add_opt args "ifFromInState" Jsont.Json.string if_from_in_state in
  let args = add_opt args "ifInState" Jsont.Json.string if_in_state in
  let args =
    ( "create",
      encode_exn
        (Proto_json_map.of_creation Jsont.json)
        (List.map
           (fun (cid, source_id, overrides) ->
             ( cid,
               copy_create_object ~caller:"contact_card_copy"
                 ~overridable:[ "addressBookIds" ] ~source_id overrides ))
           create) )
    :: args
  in
  let args =
    add_opt args "onSuccessDestroyOriginal" Jsont.Json.bool
      on_success_destroy_original
  in
  let args =
    add_opt args "destroyFromIfInState" Jsont.Json.string
      destroy_from_if_in_state
  in
  make ~name:"ContactCard/copy" ~args
    (Proto_method.copy_response_jsont Contacts_card.jsont)
    state

let invocation ~name ~arguments codec state =
  (match check_arguments arguments with
  | Ok () -> ()
  | Error msg -> invalid_arg ("Chain.invocation: " ^ msg));
  let call_id = invoke ~name ~arguments state in
  {
    call_id;
    method_name = name;
    properties = None;
    read = read_response ~call_id ~method_name:name codec;
  }

let raw_invocation ~name ~arguments = invocation ~name ~arguments Jsont.json
let echo arguments = raw_invocation ~name:"Core/echo" ~arguments

(* RFC 8620 Section 3.2: the response of a call is the one among those sharing
   its method call id whose name is the name that was called, or "error". *)
let find_invocation h response =
  List.find_opt
    (fun (inv : Proto_invocation.t) ->
      String.equal inv.name h.method_name || Proto_response.is_error inv)
    (Proto_response.find_responses h.call_id response)

let method_error h response =
  match find_invocation h response with
  | Some inv when Proto_response.is_error inv -> (
      match Proto_response.error inv with
      | Some (Ok e) -> Some e
      | Some (Error _) | None -> None)
  | _ -> None

let parse h response = h.read (find_invocation h response)

let parse_exn h response =
  match parse h response with Ok r -> r | Error e -> raise (Parse_error e)

module Handles = struct
  type _ t = [] : unit t | ( :: ) : (_, 'r) handle * 'rs t -> ('r * 'rs) t
end

module Results = struct
  type _ t = [] : unit t | ( :: ) : 'r * 'rs t -> ('r * 'rs) t
end

let rec parse_all : type rs.
    rs Handles.t -> Proto_response.t -> (rs Results.t, parse_error) result =
 fun hs response ->
  match hs with
  | Handles.[] -> Ok Results.[]
  | Handles.(h :: hs) -> (
      match parse h response with
      | Error e -> Error e
      | Ok v -> (
          match parse_all hs response with
          | Error e -> Error e
          | Ok vs -> Ok Results.(v :: vs)))

let parse_all_exn hs response =
  match parse_all hs response with
  | Ok vs -> vs
  | Error e -> raise (Parse_error e)
