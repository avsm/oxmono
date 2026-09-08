(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let core = "urn:ietf:params:jmap:core"
let mail = "urn:ietf:params:jmap:mail"
let submission = "urn:ietf:params:jmap:submission"
let vacation_response = "urn:ietf:params:jmap:vacationresponse"
let contacts = "urn:ietf:params:jmap:contacts"

let empty_object_jsont kind =
  Jsont.Object.map ~kind () |> Jsont.Object.error_unknown |> Jsont.Object.finish

let unsigned_at_least ~(kind : string) minimum =
  let validate value =
    if Int64.compare value minimum < 0 then
      Jsont.Error.msgf Jsont.Meta.none "%s: expected an integer >= %Ld" kind
        minimum;
    value
  in
  Jsont.map ~kind ~dec:validate ~enc:validate Proto_int53.Unsigned.jsont

module Core = struct
  type t = {
    max_size_upload : int64;
    max_concurrent_upload : int64;
    max_size_request : int64;
    max_concurrent_requests : int64;
    max_calls_in_request : int64;
    max_objects_in_get : int64;
    max_objects_in_set : int64;
    collation_algorithms : string list;
  }

  let make max_size_upload max_concurrent_upload max_size_request
      max_concurrent_requests max_calls_in_request max_objects_in_get
      max_objects_in_set collation_algorithms =
    {
      max_size_upload;
      max_concurrent_upload;
      max_size_request;
      max_concurrent_requests;
      max_calls_in_request;
      max_objects_in_get;
      max_objects_in_set;
      collation_algorithms;
    }

  let create ~max_size_upload ~max_concurrent_upload ~max_size_request
      ~max_concurrent_requests ~max_calls_in_request ~max_objects_in_get
      ~max_objects_in_set ~collation_algorithms =
    make max_size_upload max_concurrent_upload max_size_request
      max_concurrent_requests max_calls_in_request max_objects_in_get
      max_objects_in_set collation_algorithms

  let jsont =
    let kind = "Core capability" in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "maxSizeUpload" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_size_upload)
    |> Jsont.Object.mem "maxConcurrentUpload" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_concurrent_upload)
    |> Jsont.Object.mem "maxSizeRequest" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_size_request)
    |> Jsont.Object.mem "maxConcurrentRequests" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_concurrent_requests)
    |> Jsont.Object.mem "maxCallsInRequest" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_calls_in_request)
    |> Jsont.Object.mem "maxObjectsInGet" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_objects_in_get)
    |> Jsont.Object.mem "maxObjectsInSet" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_objects_in_set)
    |> Jsont.Object.mem "collationAlgorithms" (Jsont.list Jsont.string)
         ~enc:(fun c -> c.collation_algorithms)
    |> Jsont.Object.finish
end

module Mail = struct
  type t = {
    max_mailboxes_per_email : int64 option;
    max_mailbox_depth : int64 option;
    max_size_mailbox_name : int64 option;
    max_size_attachments_per_email : int64 option;
    email_query_sort_options : string list option;
    may_create_top_level_mailbox : bool option;
  }

  let create ?max_mailboxes_per_email ?max_mailbox_depth ?max_size_mailbox_name
      ?max_size_attachments_per_email ?email_query_sort_options
      ?may_create_top_level_mailbox () =
    {
      max_mailboxes_per_email;
      max_mailbox_depth;
      max_size_mailbox_name;
      max_size_attachments_per_email;
      email_query_sort_options;
      may_create_top_level_mailbox;
    }

  let make max_mailboxes_per_email max_mailbox_depth max_size_mailbox_name
      max_size_attachments_per_email email_query_sort_options
      may_create_top_level_mailbox =
    {
      max_mailboxes_per_email;
      max_mailbox_depth;
      max_size_mailbox_name;
      max_size_attachments_per_email;
      email_query_sort_options;
      may_create_top_level_mailbox;
    }

  let is_empty c =
    Option.is_none c.max_mailboxes_per_email
    && Option.is_none c.max_mailbox_depth
    && Option.is_none c.max_size_mailbox_name
    && Option.is_none c.max_size_attachments_per_email
    && Option.is_none c.email_query_sort_options
    && Option.is_none c.may_create_top_level_mailbox

  (* RFC 8621 Section 1.3.1 types maxMailboxesPerEmail and maxMailboxDepth
     "UnsignedInt|null", where null means no limit, so at account scope "no
     limit" is an explicit null rather than an absent member. *)
  let encode_jsont =
    let kind = "Mail capability" in
    Jsont.Object.map ~kind make
    |> Proto_json_map.nullable_mem_null "maxMailboxesPerEmail"
         Proto_int53.Unsigned.jsont ~enc:(fun c -> c.max_mailboxes_per_email)
    |> Proto_json_map.nullable_mem_null "maxMailboxDepth"
         Proto_int53.Unsigned.jsont ~enc:(fun c -> c.max_mailbox_depth)
    |> Jsont.Object.opt_mem "maxSizeMailboxName" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_size_mailbox_name)
    |> Jsont.Object.opt_mem "maxSizeAttachmentsPerEmail"
         Proto_int53.Unsigned.jsont ~enc:(fun c ->
           c.max_size_attachments_per_email)
    |> Jsont.Object.opt_mem "emailQuerySortOptions" (Jsont.list Jsont.string)
         ~enc:(fun c -> c.email_query_sort_options)
    |> Jsont.Object.opt_mem "mayCreateTopLevelMailbox" Jsont.bool ~enc:(fun c ->
        c.may_create_top_level_mailbox)
    |> Jsont.Object.finish

  let session_jsont = empty_object_jsont "Mail session capability"

  let required name = function
    | Some value -> value
    | None ->
        Jsont.Error.msgf Jsont.Meta.none
          "Mail account capability: member %S is required" name

  let account_make max_mailboxes_per_email max_mailbox_depth
      max_size_mailbox_name max_size_attachments_per_email
      email_query_sort_options may_create_top_level_mailbox =
    make max_mailboxes_per_email max_mailbox_depth (Some max_size_mailbox_name)
      (Some max_size_attachments_per_email) (Some email_query_sort_options)
      (Some may_create_top_level_mailbox)

  let account_jsont =
    let kind = "Mail account capability" in
    Jsont.Object.map ~kind account_make
    |> Jsont.Object.mem "maxMailboxesPerEmail"
         (Jsont.option (unsigned_at_least ~kind:"maxMailboxesPerEmail" 1L))
         ~enc:(fun c -> c.max_mailboxes_per_email)
    |> Jsont.Object.mem "maxMailboxDepth"
         (Jsont.option Proto_int53.Unsigned.jsont) ~enc:(fun c ->
           c.max_mailbox_depth)
    |> Jsont.Object.mem "maxSizeMailboxName"
         (unsigned_at_least ~kind:"maxSizeMailboxName" 100L) ~enc:(fun c ->
           required "maxSizeMailboxName" c.max_size_mailbox_name)
    |> Jsont.Object.mem "maxSizeAttachmentsPerEmail" Proto_int53.Unsigned.jsont
         ~enc:(fun c ->
           required "maxSizeAttachmentsPerEmail"
             c.max_size_attachments_per_email)
    |> Jsont.Object.mem "emailQuerySortOptions" (Jsont.list Jsont.string)
         ~enc:(fun c ->
           required "emailQuerySortOptions" c.email_query_sort_options)
    |> Jsont.Object.mem "mayCreateTopLevelMailbox" Jsont.bool ~enc:(fun c ->
        required "mayCreateTopLevelMailbox" c.may_create_top_level_mailbox)
    |> Jsont.Object.finish
end

module Submission = struct
  type t = {
    max_delayed_send : int64 option;
    submission_extensions : (string * string list) list option;
  }

  let create ?max_delayed_send ?submission_extensions () =
    { max_delayed_send; submission_extensions }

  let make max_delayed_send submission_extensions =
    { max_delayed_send; submission_extensions }

  let submission_extensions_jsont =
    Proto_json_map.of_string (Jsont.list Jsont.string)

  let encode_jsont =
    let kind = "Submission capability" in
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "maxDelayedSend" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> c.max_delayed_send)
    |> Jsont.Object.opt_mem "submissionExtensions" submission_extensions_jsont
         ~enc:(fun c -> c.submission_extensions)
    |> Jsont.Object.finish

  let session_jsont = empty_object_jsont "Submission session capability"

  let required name = function
    | Some value -> value
    | None ->
        Jsont.Error.msgf Jsont.Meta.none
          "Submission account capability: member %S is required" name

  let account_make max_delayed_send submission_extensions =
    make (Some max_delayed_send) (Some submission_extensions)

  let account_jsont =
    let kind = "Submission account capability" in
    Jsont.Object.map ~kind account_make
    |> Jsont.Object.mem "maxDelayedSend" Proto_int53.Unsigned.jsont
         ~enc:(fun c -> required "maxDelayedSend" c.max_delayed_send)
    |> Jsont.Object.mem "submissionExtensions" submission_extensions_jsont
         ~enc:(fun c -> required "submissionExtensions" c.submission_extensions)
    |> Jsont.Object.finish
end

module Contacts = struct
  type t = {
    max_address_books_per_card : int64 option;
    may_create_address_book : bool option;
  }

  let create ?max_address_books_per_card ?may_create_address_book () =
    { max_address_books_per_card; may_create_address_book }

  let make max_address_books_per_card may_create_address_book =
    { max_address_books_per_card; may_create_address_book }

  let is_empty c =
    Option.is_none c.max_address_books_per_card
    && Option.is_none c.may_create_address_book

  (* RFC 9610 Section 1.4.1 types maxAddressBooksPerCard "UnsignedInt|null",
     where null means the limit is the number of AddressBooks in the account,
     so "no limit" is an explicit null rather than an absent member. *)
  let encode_jsont =
    let kind = "Contacts capability" in
    Jsont.Object.map ~kind make
    |> Proto_json_map.nullable_mem_null "maxAddressBooksPerCard"
         Proto_int53.Unsigned.jsont ~enc:(fun c -> c.max_address_books_per_card)
    |> Jsont.Object.opt_mem "mayCreateAddressBook" Jsont.bool ~enc:(fun c ->
        c.may_create_address_book)
    |> Jsont.Object.finish

  let session_jsont = empty_object_jsont "Contacts session capability"

  let required name = function
    | Some value -> value
    | None ->
        Jsont.Error.msgf Jsont.Meta.none
          "Contacts account capability: member %S is required" name

  let account_make max_address_books_per_card may_create_address_book =
    make max_address_books_per_card (Some may_create_address_book)

  let account_jsont =
    let kind = "Contacts account capability" in
    Jsont.Object.map ~kind account_make
    |> Jsont.Object.mem "maxAddressBooksPerCard"
         (Jsont.option (unsigned_at_least ~kind:"maxAddressBooksPerCard" 1L))
         ~enc:(fun c -> c.max_address_books_per_card)
    |> Jsont.Object.mem "mayCreateAddressBook" Jsont.bool ~enc:(fun c ->
        required "mayCreateAddressBook" c.may_create_address_book)
    |> Jsont.Object.finish
end

type capability =
  | Core of Core.t
  | Mail of Mail.t
  | Submission of Submission.t
  | Vacation_response
  | Contacts of Contacts.t
  | Unknown of Jsont.json

let decode jsont wrap json =
  match Jsont.Json.decode jsont json with
  | Ok v -> Ok (wrap v)
  | Error msg -> Error msg

let empty_capability_jsont = empty_object_jsont "Empty capability"

let session_capability_of_json uri json =
  match uri with
  | u when String.equal u core -> decode Core.jsont (fun c -> Core c) json
  | u when String.equal u mail ->
      decode Mail.session_jsont (fun () -> Mail (Mail.create ())) json
  | u when String.equal u submission ->
      decode Submission.session_jsont
        (fun () -> Submission (Submission.create ()))
        json
  | u when String.equal u vacation_response ->
      decode empty_capability_jsont (fun () -> Vacation_response) json
  | u when String.equal u contacts ->
      decode Contacts.session_jsont
        (fun () -> Contacts (Contacts.create ()))
        json
  | _ -> Ok (Unknown json)

let account_capability_of_json uri json =
  match uri with
  | u when String.equal u core -> Ok (Unknown json)
  | u when String.equal u mail ->
      decode Mail.account_jsont (fun m -> Mail m) json
  | u when String.equal u submission ->
      decode Submission.account_jsont (fun s -> Submission s) json
  | u when String.equal u vacation_response ->
      decode empty_capability_jsont (fun () -> Vacation_response) json
  | u when String.equal u contacts ->
      decode Contacts.account_jsont (fun c -> Contacts c) json
  | _ -> Ok (Unknown json)

let empty_object = Jsont.Object ([], Jsont.Meta.none)

let capability_to_json (uri, cap) =
  let encode jsont v =
    match Jsont.Json.encode' jsont v with
    | Ok json -> json
    | Error e -> invalid_arg ("Proto_capability: " ^ Jsont.Error.to_string e)
  in
  match cap with
  | Core c -> (uri, encode Core.jsont c)
  | Mail m when Mail.is_empty m -> (uri, empty_object)
  | Mail m -> (uri, encode Mail.encode_jsont m)
  | Submission s -> (uri, encode Submission.encode_jsont s)
  | Vacation_response -> (uri, empty_object)
  | Contacts c when Contacts.is_empty c -> (uri, empty_object)
  | Contacts c -> (uri, encode Contacts.encode_jsont c)
  | Unknown json -> (uri, json)
