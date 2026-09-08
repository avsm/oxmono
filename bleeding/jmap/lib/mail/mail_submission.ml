(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type property =
  [ `Id
  | `Identity_id
  | `Email_id
  | `Thread_id
  | `Envelope
  | `Send_at
  | `Undo_status
  | `Delivery_status
  | `Dsn_blob_ids
  | `Mdn_blob_ids ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Identity_id -> "identityId"
  | `Email_id -> "emailId"
  | `Thread_id -> "threadId"
  | `Envelope -> "envelope"
  | `Send_at -> "sendAt"
  | `Undo_status -> "undoStatus"
  | `Delivery_status -> "deliveryStatus"
  | `Dsn_blob_ids -> "dsnBlobIds"
  | `Mdn_blob_ids -> "mdnBlobIds"

let property_of_string s : property option =
  match s with
  | "id" -> Some `Id
  | "identityId" -> Some `Identity_id
  | "emailId" -> Some `Email_id
  | "threadId" -> Some `Thread_id
  | "envelope" -> Some `Envelope
  | "sendAt" -> Some `Send_at
  | "undoStatus" -> Some `Undo_status
  | "deliveryStatus" -> Some `Delivery_status
  | "dsnBlobIds" -> Some `Dsn_blob_ids
  | "mdnBlobIds" -> Some `Mdn_blob_ids
  | _ -> None

module Address = struct
  type t = { email : string; parameters : (string * string option) list option }

  let v ?parameters email = { email; parameters }

  let jsont =
    let kind = "EmailSubmission Address" in
    Jsont.Object.map ~kind (fun email parameters -> { email; parameters })
    |> Jsont.Object.mem "email" Jsont.string ~enc:(fun t -> t.email)
    (* parameters is Object|null, and each value is String|null when the
       SMTP parameter takes no value (RFC 8621 Section 7). *)
    |> Proto_json_map.nullable_mem "parameters"
         (Proto_json_map.of_string Jsont.(option string))
         ~enc:(fun t -> t.parameters)
    |> Jsont.Object.finish
end

module Envelope = struct
  type t = { mail_from : Address.t; rcpt_to : Address.t list }

  let v ~mail_from ~rcpt_to = { mail_from; rcpt_to }

  let jsont =
    let kind = "Envelope" in
    Jsont.Object.map ~kind (fun mail_from rcpt_to -> { mail_from; rcpt_to })
    |> Jsont.Object.mem "mailFrom" Address.jsont ~enc:(fun t -> t.mail_from)
    |> Jsont.Object.mem "rcptTo" (Jsont.list Address.jsont) ~enc:(fun t ->
        t.rcpt_to)
    |> Jsont.Object.finish
end

module Delivery_status = struct
  type delivered = [ `Queued | `Yes | `No | `Unknown | `Other of string ]

  let delivered_to_string = function
    | `Queued -> "queued"
    | `Yes -> "yes"
    | `No -> "no"
    | `Unknown -> "unknown"
    | `Other value -> value

  let delivered_of_string = function
    | "queued" -> Some `Queued
    | "yes" -> Some `Yes
    | "no" -> Some `No
    | "unknown" -> Some `Unknown
    | _ -> None

  let delivered_of_string_exn s =
    match delivered_of_string s with
    | Some d -> d
    | None -> invalid_arg (Printf.sprintf "Unknown delivered value: %S" s)

  (* Keep a nonconformant extension spelling distinct from the RFC's own
     "unknown" status. Losing the whole /get would be brittle, while mapping
     the spelling to [`Unknown] would silently change it on a round trip. *)
  let delivered_jsont =
    let dec s =
      match delivered_of_string s with Some d -> d | None -> `Other s
    in
    Jsont.map ~kind:"DeliveryStatus.delivered" ~dec ~enc:delivered_to_string
      Jsont.string

  type displayed = [ `Unknown | `Yes | `Other of string ]

  let displayed_to_string = function
    | `Unknown -> "unknown"
    | `Yes -> "yes"
    | `Other value -> value

  let displayed_of_string = function
    | "yes" -> Some `Yes
    | "unknown" -> Some `Unknown
    | _ -> None

  let displayed_of_string_exn s =
    match displayed_of_string s with
    | Some d -> d
    | None -> invalid_arg (Printf.sprintf "Unknown displayed value: %S" s)

  let displayed_jsont =
    let dec s =
      match displayed_of_string s with Some d -> d | None -> `Other s
    in
    Jsont.map ~kind:"DeliveryStatus.displayed" ~dec ~enc:displayed_to_string
      Jsont.string

  type t = { smtp_reply : string; delivered : delivered; displayed : displayed }

  let v ~smtp_reply ~delivered ~displayed = { smtp_reply; delivered; displayed }

  let jsont =
    let kind = "DeliveryStatus" in
    let make smtp_reply delivered displayed =
      { smtp_reply; delivered; displayed }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "smtpReply" Jsont.string ~enc:(fun t -> t.smtp_reply)
    |> Jsont.Object.mem "delivered" delivered_jsont ~enc:(fun t -> t.delivered)
    |> Jsont.Object.mem "displayed" displayed_jsont ~enc:(fun t -> t.displayed)
    |> Jsont.Object.finish
end

type undo_status = [ `Pending | `Final | `Canceled ]

let undo_status_assoc : (string * undo_status) list =
  [ ("pending", `Pending); ("final", `Final); ("canceled", `Canceled) ]

let undo_status_to_string : undo_status -> string = function
  | `Pending -> "pending"
  | `Final -> "final"
  | `Canceled -> "canceled"

let undo_status_of_string s = List.assoc_opt s undo_status_assoc
let undo_status_jsont = Jsont.enum ~kind:"UndoStatus" undo_status_assoc

type t = {
  id : Proto_id.t option;
  identity_id : Proto_id.t option;
  email_id : Proto_id.t option;
  thread_id : Proto_id.t option;
  envelope : Envelope.t option;
  send_at : Ptime.t option;
  undo_status : undo_status option;
  delivery_status : (string * Delivery_status.t) list option;
  dsn_blob_ids : Proto_id.t list option;
  mdn_blob_ids : Proto_id.t list option;
}

let v ?id ?identity_id ?email_id ?thread_id ?envelope ?send_at ?undo_status
    ?delivery_status ?dsn_blob_ids ?mdn_blob_ids () =
  {
    id;
    identity_id;
    email_id;
    thread_id;
    envelope;
    send_at;
    undo_status;
    delivery_status;
    dsn_blob_ids;
    mdn_blob_ids;
  }

let id t = t.id

let create ?envelope ~identity_id ~email_id () =
  v ~identity_id ~email_id ?envelope ()

let jsont =
  let kind = "EmailSubmission" in
  let make id identity_id email_id thread_id envelope send_at undo_status
      delivery_status dsn_blob_ids mdn_blob_ids =
    {
      id;
      identity_id;
      email_id;
      thread_id;
      envelope;
      send_at;
      undo_status;
      delivery_status;
      dsn_blob_ids;
      mdn_blob_ids;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "identityId" Proto_id.jsont ~enc:(fun t ->
      t.identity_id)
  |> Jsont.Object.opt_mem "emailId" Proto_id.jsont ~enc:(fun t -> t.email_id)
  |> Jsont.Object.opt_mem "threadId" Proto_id.jsont ~enc:(fun t -> t.thread_id)
  (* envelope is Envelope|null (RFC 8621 Section 7). *)
  |> Proto_json_map.nullable_mem "envelope" Envelope.jsont ~enc:(fun t ->
      t.envelope)
  |> Jsont.Object.opt_mem "sendAt" Proto_date.utc_jsont ~enc:(fun t ->
      t.send_at)
  |> Jsont.Object.opt_mem "undoStatus" undo_status_jsont ~enc:(fun t ->
      t.undo_status)
  (* deliveryStatus is String[DeliveryStatus]|null (RFC 8621 Section 7):
     null while the server has no delivery information. *)
  |> Proto_json_map.nullable_mem "deliveryStatus"
       (Proto_json_map.of_string Delivery_status.jsont) ~enc:(fun t ->
         t.delivery_status)
  |> Jsont.Object.opt_mem "dsnBlobIds" (Jsont.list Proto_id.jsont)
       ~enc:(fun t -> t.dsn_blob_ids)
  |> Jsont.Object.opt_mem "mdnBlobIds" (Jsont.list Proto_id.jsont)
       ~enc:(fun t -> t.mdn_blob_ids)
  |> Jsont.Object.finish

module Filter_condition = struct
  type t = {
    identity_ids : Proto_id.t list option;
    email_ids : Proto_id.t list option;
    thread_ids : Proto_id.t list option;
    undo_status : undo_status option;
    before : Ptime.t option;
    after : Ptime.t option;
  }

  let empty =
    {
      identity_ids = None;
      email_ids = None;
      thread_ids = None;
      undo_status = None;
      before = None;
      after = None;
    }

  let jsont =
    let kind = "EmailSubmissionFilterCondition" in
    let make identity_ids email_ids thread_ids undo_status before after =
      { identity_ids; email_ids; thread_ids; undo_status; before; after }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "identityIds" (Jsont.list Proto_id.jsont)
         ~enc:(fun f -> f.identity_ids)
    |> Jsont.Object.opt_mem "emailIds" (Jsont.list Proto_id.jsont)
         ~enc:(fun f -> f.email_ids)
    |> Jsont.Object.opt_mem "threadIds" (Jsont.list Proto_id.jsont)
         ~enc:(fun f -> f.thread_ids)
    |> Jsont.Object.opt_mem "undoStatus" undo_status_jsont ~enc:(fun f ->
        f.undo_status)
    |> Jsont.Object.opt_mem "before" Proto_date.utc_jsont ~enc:(fun f ->
        f.before)
    |> Jsont.Object.opt_mem "after" Proto_date.utc_jsont ~enc:(fun f -> f.after)
    |> Jsont.Object.finish
end

type filter = Filter_condition.t Proto_filter.filter

let filter_jsont = Proto_filter.filter_jsont Filter_condition.jsont

let filter ?identity_ids ?email_ids ?thread_ids ?undo_status ?before ?after () =
  Proto_filter.Condition
    {
      Filter_condition.identity_ids;
      email_ids;
      thread_ids;
      undo_status;
      before;
      after;
    }

type sort_property = [ `Email_id | `Thread_id | `Sent_at ]

let sort ?ascending ?collation p =
  (* RFC 8621 Section 7.3 names the third sort property "sentAt", though the
     property of the object it orders on is "sendAt". *)
  let property =
    match p with
    | `Email_id -> "emailId"
    | `Thread_id -> "threadId"
    | `Sent_at -> "sentAt"
  in
  Proto_filter.comparator ?is_ascending:ascending ?collation property

let creation = Proto_id.creation
