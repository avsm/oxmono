(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let type_jsont ~(kind : string) ~of_string ~to_string =
  Jsont.map ~kind ~dec:of_string ~enc:to_string Jsont.string

(* Httpz owns the diagnostic-boundary policy shared with its media codecs,
   including C0, DEL, and C1 controls. *)
let pp_escaped ppf s =
  Format.pp_print_string ppf (Httpz_media.sanitize_diagnostic s)

let normalise ~of_string ~to_string type_ = of_string (to_string type_)

let pp_error ~name ~note ppf =
  pp_escaped ppf name;
  match note with
  | None -> ()
  | Some note -> Format.fprintf ppf " (%a)" pp_escaped note

let to_string pp e = Format.asprintf "%a" pp e

module Request_error = struct
  type type_ =
    [ `Unknown_capability
    | `Not_json
    | `Not_request
    | `Limit
    | `Other of string ]

  let type_to_string = function
    | `Unknown_capability -> "urn:ietf:params:jmap:error:unknownCapability"
    | `Not_json -> "urn:ietf:params:jmap:error:notJSON"
    | `Not_request -> "urn:ietf:params:jmap:error:notRequest"
    | `Limit -> "urn:ietf:params:jmap:error:limit"
    | `Other s -> s

  let type_of_string = function
    | "urn:ietf:params:jmap:error:unknownCapability" -> `Unknown_capability
    | "urn:ietf:params:jmap:error:notJSON" -> `Not_json
    | "urn:ietf:params:jmap:error:notRequest" -> `Not_request
    | "urn:ietf:params:jmap:error:limit" -> `Limit
    | s -> `Other s

  let type_jsont =
    type_jsont ~kind:"Request error URN" ~of_string:type_of_string
      ~to_string:type_to_string

  let min_status = 0
  let max_status = 999
  let is_status i = i >= min_status && i <= max_status

  (* RFC 7807 problem details: "status", when present, is a JSON number and
     never a string. *)
  let status_jsont =
    let kind = "HTTP status" in
    let dec meta f =
      if
        Float.is_integer f
        && f >= float_of_int min_status
        && f <= float_of_int max_status
      then int_of_float f
      else Jsont.Error.msgf meta "expected an HTTP status code but found %g" f
    in
    let enc i =
      if is_status i then float_of_int i
      else
        Jsont.Error.msgf Jsont.Meta.none
          "expected an HTTP status code but found %d" i
    in
    Jsont.Base.number (Jsont.Base.map ~kind ~dec ~enc ())

  type t = {
    type_ : type_;
    status : int option;
    title : string option;
    detail : string option;
    limit : string option;
    unknown : Proto_unknown.t;
  }

  let make type_ status title detail limit unknown =
    { type_; status; title; detail; limit; unknown }

  let unknown_member t name = Proto_unknown.find t.unknown name

  (* RFC 8620 Section 3.6.1: for "urn:ietf:params:jmap:error:limit", a
     "limit" property MUST also be present, "containing the name of the limit
     being applied". *)
  let validate t =
    match (t.type_, t.limit) with
    | `Limit, None ->
        Error
          "a urn:ietf:params:jmap:error:limit problem details object must \
           carry a \"limit\" member naming the limit being applied"
    | _ -> Ok t

  let v ?status ?title ?detail ?limit ?(unknown = Proto_unknown.empty) type_ =
    match status with
    | Some s when not (is_status s) ->
        Error (Printf.sprintf "status is not an HTTP status code: %d" s)
    | _ ->
        let type_ =
          normalise ~of_string:type_of_string ~to_string:type_to_string type_
        in
        validate { type_; status; title; detail; limit; unknown }

  let jsont =
    let kind = "Request error" in
    Jsont.Object.map ~kind make
    (* RFC 7807 Section 4.2: an absent "type" member means "about:blank". *)
    |> Jsont.Object.mem "type" type_jsont ~dec_absent:(fun () -> `Other "about:blank")
         ~enc:(fun e -> e.type_)
    |> Proto_json_map.nullable_mem "status" status_jsont ~enc:(fun e ->
        e.status)
    |> Proto_json_map.nullable_mem "title" Jsont.string ~enc:(fun e -> e.title)
    |> Proto_json_map.nullable_mem "detail" Jsont.string ~enc:(fun e ->
        e.detail)
    |> Proto_json_map.nullable_mem "limit" Jsont.string ~enc:(fun e -> e.limit)
    (* RFC 7807 Section 3.1: "problem type definitions MAY extend the problem
       details object with additional members". *)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun e -> e.unknown)
    |> Jsont.Object.finish

  let pp ppf e =
    let note =
      match (e.detail, e.title) with
      | Some s, _ | None, Some s -> Some s
      | None, None -> None
    in
    pp_error ~name:(type_to_string e.type_) ~note ppf

  let to_string e = to_string pp e
end

module Method_error = struct
  type type_ =
    [ `Server_unavailable
    | `Server_fail
    | `Server_partial_fail
    | `Unknown_method
    | `Invalid_arguments
    | `Invalid_result_reference
    | `Forbidden
    | `Account_not_found
    | `Account_not_supported_by_method
    | `Account_read_only
    | `Cannot_calculate_changes
    | `Request_too_large
    | `State_mismatch
    | `Anchor_not_found
    | `Unsupported_sort
    | `Unsupported_filter
    | `Too_many_changes
    | `From_account_not_found
    | `From_account_not_supported_by_method
    | `Other of string ]

  let type_to_string = function
    | `Server_unavailable -> "serverUnavailable"
    | `Server_fail -> "serverFail"
    | `Server_partial_fail -> "serverPartialFail"
    | `Unknown_method -> "unknownMethod"
    | `Invalid_arguments -> "invalidArguments"
    | `Invalid_result_reference -> "invalidResultReference"
    | `Forbidden -> "forbidden"
    | `Account_not_found -> "accountNotFound"
    | `Account_not_supported_by_method -> "accountNotSupportedByMethod"
    | `Account_read_only -> "accountReadOnly"
    | `Cannot_calculate_changes -> "cannotCalculateChanges"
    | `Request_too_large -> "requestTooLarge"
    | `State_mismatch -> "stateMismatch"
    | `Anchor_not_found -> "anchorNotFound"
    | `Unsupported_sort -> "unsupportedSort"
    | `Unsupported_filter -> "unsupportedFilter"
    | `Too_many_changes -> "tooManyChanges"
    | `From_account_not_found -> "fromAccountNotFound"
    | `From_account_not_supported_by_method -> "fromAccountNotSupportedByMethod"
    | `Other s -> s

  let type_of_string = function
    | "serverUnavailable" -> `Server_unavailable
    | "serverFail" -> `Server_fail
    | "serverPartialFail" -> `Server_partial_fail
    | "unknownMethod" -> `Unknown_method
    | "invalidArguments" -> `Invalid_arguments
    | "invalidResultReference" -> `Invalid_result_reference
    | "forbidden" -> `Forbidden
    | "accountNotFound" -> `Account_not_found
    | "accountNotSupportedByMethod" -> `Account_not_supported_by_method
    | "accountReadOnly" -> `Account_read_only
    | "cannotCalculateChanges" -> `Cannot_calculate_changes
    | "requestTooLarge" -> `Request_too_large
    | "stateMismatch" -> `State_mismatch
    | "anchorNotFound" -> `Anchor_not_found
    | "unsupportedSort" -> `Unsupported_sort
    | "unsupportedFilter" -> `Unsupported_filter
    | "tooManyChanges" -> `Too_many_changes
    | "fromAccountNotFound" -> `From_account_not_found
    | "fromAccountNotSupportedByMethod" -> `From_account_not_supported_by_method
    | s -> `Other s

  let type_jsont =
    type_jsont ~kind:"Method error type" ~of_string:type_of_string
      ~to_string:type_to_string

  type t = {
    type_ : type_;
    description : string option;
    unknown : Proto_unknown.t;
  }

  let make type_ description unknown = { type_; description; unknown }
  let unknown_member t name = Proto_unknown.find t.unknown name
  let validate t = Ok t

  let v ?description ?(unknown = Proto_unknown.empty) type_ =
    let type_ =
      normalise ~of_string:type_of_string ~to_string:type_to_string type_
    in
    validate { type_; description; unknown }

  let jsont =
    let kind = "Method error" in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "type" type_jsont ~enc:(fun e -> e.type_)
    |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun e ->
        e.description)
    (* RFC 8620 Section 3.6.2: an error type "MAY define further
       properties". *)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun e -> e.unknown)
    |> Jsont.Object.finish

  let pp ppf e = pp_error ~name:(type_to_string e.type_) ~note:e.description ppf
  let to_string e = to_string pp e
end

module Set_error = struct
  type type_ =
    [ `Forbidden
    | `Over_quota
    | `Too_large
    | `Rate_limit
    | `Not_found
    | `Invalid_patch
    | `Will_destroy
    | `Invalid_properties
    | `Singleton
    | `Already_exists
    | `Mailbox_has_child
    | `Mailbox_has_email
    | `Address_book_has_contents
    | `Blob_not_found
    | `Too_many_keywords
    | `Too_many_mailboxes
    | `Invalid_email
    | `Too_many_recipients
    | `No_recipients
    | `Invalid_recipients
    | `Cannot_unsend
    | `Forbidden_mail_from
    | `Forbidden_from
    | `Forbidden_to_send
    | `Other of string ]

  let type_to_string = function
    | `Forbidden -> "forbidden"
    | `Over_quota -> "overQuota"
    | `Too_large -> "tooLarge"
    | `Rate_limit -> "rateLimit"
    | `Not_found -> "notFound"
    | `Invalid_patch -> "invalidPatch"
    | `Will_destroy -> "willDestroy"
    | `Invalid_properties -> "invalidProperties"
    | `Singleton -> "singleton"
    | `Already_exists -> "alreadyExists"
    | `Mailbox_has_child -> "mailboxHasChild"
    | `Mailbox_has_email -> "mailboxHasEmail"
    | `Address_book_has_contents -> "addressBookHasContents"
    | `Blob_not_found -> "blobNotFound"
    | `Too_many_keywords -> "tooManyKeywords"
    | `Too_many_mailboxes -> "tooManyMailboxes"
    | `Invalid_email -> "invalidEmail"
    | `Too_many_recipients -> "tooManyRecipients"
    | `No_recipients -> "noRecipients"
    | `Invalid_recipients -> "invalidRecipients"
    | `Cannot_unsend -> "cannotUnsend"
    | `Forbidden_mail_from -> "forbiddenMailFrom"
    | `Forbidden_from -> "forbiddenFrom"
    | `Forbidden_to_send -> "forbiddenToSend"
    | `Other s -> s

  let type_of_string = function
    | "forbidden" -> `Forbidden
    | "overQuota" -> `Over_quota
    | "tooLarge" -> `Too_large
    | "rateLimit" -> `Rate_limit
    | "notFound" -> `Not_found
    | "invalidPatch" -> `Invalid_patch
    | "willDestroy" -> `Will_destroy
    | "invalidProperties" -> `Invalid_properties
    | "singleton" -> `Singleton
    | "alreadyExists" -> `Already_exists
    | "mailboxHasChild" -> `Mailbox_has_child
    | "mailboxHasEmail" -> `Mailbox_has_email
    | "addressBookHasContents" -> `Address_book_has_contents
    | "blobNotFound" -> `Blob_not_found
    | "tooManyKeywords" -> `Too_many_keywords
    | "tooManyMailboxes" -> `Too_many_mailboxes
    | "invalidEmail" -> `Invalid_email
    | "tooManyRecipients" -> `Too_many_recipients
    | "noRecipients" -> `No_recipients
    | "invalidRecipients" -> `Invalid_recipients
    | "cannotUnsend" -> `Cannot_unsend
    | "forbiddenMailFrom" -> `Forbidden_mail_from
    | "forbiddenFrom" -> `Forbidden_from
    | "forbiddenToSend" -> `Forbidden_to_send
    | s -> `Other s

  let type_jsont =
    type_jsont ~kind:"SetError type" ~of_string:type_of_string
      ~to_string:type_to_string

  type t = {
    type_ : type_;
    description : string option;
    properties : string list option;
    existing_id : Proto_id.t option;
    not_found : Proto_id.t list option;
    max_size : int64 option;
    max_recipients : int64 option;
    invalid_recipients : string list option;
    unknown : Proto_unknown.t;
  }

  let make type_ description properties existing_id not_found max_size
      max_recipients invalid_recipients unknown =
    {
      type_;
      description;
      properties;
      existing_id;
      not_found;
      max_size;
      max_recipients;
      invalid_recipients;
      unknown;
    }

  let unknown_member t name = Proto_unknown.find t.unknown name

  (* RFC 8620 Section 5.4 and RFC 8621 Sections 4.6 and 7.5 make one further
     member mandatory for each of these four types. *)
  let validate t =
    let missing member =
      Error
        (Printf.sprintf "a %s SetError must carry a %S member"
           (type_to_string t.type_) member)
    in
    match t.type_ with
    | `Already_exists when Option.is_none t.existing_id -> missing "existingId"
    | `Blob_not_found when Option.is_none t.not_found -> missing "notFound"
    | `Too_many_recipients when Option.is_none t.max_recipients ->
        missing "maxRecipients"
    | `Invalid_recipients when Option.is_none t.invalid_recipients ->
        missing "invalidRecipients"
    | _ -> Ok t

  let v ?description ?properties ?existing_id ?not_found ?max_size
      ?max_recipients ?invalid_recipients ?(unknown = Proto_unknown.empty) type_
      =
    let out_of_range name = function
      | Some n -> (
          match Proto_int53.Unsigned.of_int64 n with
          | Ok _ -> None
          | Error msg -> Some (Printf.sprintf "%s: %s" name msg))
      | None -> None
    in
    match
      ( out_of_range "maxSize" max_size,
        out_of_range "maxRecipients" max_recipients )
    with
    | Some msg, _ | None, Some msg -> Error msg
    | None, None ->
        let type_ =
          normalise ~of_string:type_of_string ~to_string:type_to_string type_
        in
        validate
          {
            type_;
            description;
            properties;
            existing_id;
            not_found;
            max_size;
            max_recipients;
            invalid_recipients;
            unknown;
          }

  let jsont =
    let kind = "SetError" in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "type" type_jsont ~enc:(fun e -> e.type_)
    |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun e ->
        e.description)
    |> Jsont.Object.opt_mem "properties" (Jsont.list Jsont.string)
         ~enc:(fun e -> e.properties)
    |> Jsont.Object.opt_mem "existingId" Proto_id.jsont ~enc:(fun e ->
        e.existing_id)
    |> Jsont.Object.opt_mem "notFound" (Jsont.list Proto_id.jsont)
         ~enc:(fun e -> e.not_found)
    |> Jsont.Object.opt_mem "maxSize" Proto_int53.Unsigned.jsont ~enc:(fun e ->
        e.max_size)
    |> Jsont.Object.opt_mem "maxRecipients" Proto_int53.Unsigned.jsont
         ~enc:(fun e -> e.max_recipients)
    |> Jsont.Object.opt_mem "invalidRecipients" (Jsont.list Jsont.string)
         ~enc:(fun e -> e.invalid_recipients)
    (* RFC 8620 Section 5.3: a SetError type "may define further
       properties", and extensions add their own. *)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun e -> e.unknown)
    |> Jsont.Object.finish

  let pp ppf e = pp_error ~name:(type_to_string e.type_) ~note:e.description ppf
  let to_string e = to_string pp e
end
