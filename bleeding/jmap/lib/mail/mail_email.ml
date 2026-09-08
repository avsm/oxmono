(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type metadata_property =
  [ `Id
  | `Blob_id
  | `Thread_id
  | `Mailbox_ids
  | `Keywords
  | `Size
  | `Received_at ]

type header_convenience_property =
  [ `Message_id
  | `In_reply_to
  | `References
  | `Sender
  | `From
  | `To
  | `Cc
  | `Bcc
  | `Reply_to
  | `Subject
  | `Sent_at
  | `Headers ]

type body_property =
  [ `Body_structure
  | `Body_values
  | `Text_body
  | `Html_body
  | `Attachments
  | `Has_attachment
  | `Preview ]

type standard_property =
  [ metadata_property | header_convenience_property | body_property ]

type header_property = [ `Header of Mail_header.header_property ]
type property = [ standard_property | header_property ]

let standard_property_to_string : [< standard_property ] -> string = function
  | `Id -> "id"
  | `Blob_id -> "blobId"
  | `Thread_id -> "threadId"
  | `Mailbox_ids -> "mailboxIds"
  | `Keywords -> "keywords"
  | `Size -> "size"
  | `Received_at -> "receivedAt"
  | `Message_id -> "messageId"
  | `In_reply_to -> "inReplyTo"
  | `References -> "references"
  | `Sender -> "sender"
  | `From -> "from"
  | `To -> "to"
  | `Cc -> "cc"
  | `Bcc -> "bcc"
  | `Reply_to -> "replyTo"
  | `Subject -> "subject"
  | `Sent_at -> "sentAt"
  | `Headers -> "headers"
  | `Body_structure -> "bodyStructure"
  | `Body_values -> "bodyValues"
  | `Text_body -> "textBody"
  | `Html_body -> "htmlBody"
  | `Attachments -> "attachments"
  | `Has_attachment -> "hasAttachment"
  | `Preview -> "preview"

let property_to_string : [< property ] -> string = function
  | `Header hp -> Mail_header.header_property_to_string hp
  | #standard_property as p -> standard_property_to_string p

let standard_property_of_string s : standard_property option =
  match s with
  | "id" -> Some `Id
  | "blobId" -> Some `Blob_id
  | "threadId" -> Some `Thread_id
  | "mailboxIds" -> Some `Mailbox_ids
  | "keywords" -> Some `Keywords
  | "size" -> Some `Size
  | "receivedAt" -> Some `Received_at
  | "messageId" -> Some `Message_id
  | "inReplyTo" -> Some `In_reply_to
  | "references" -> Some `References
  | "sender" -> Some `Sender
  | "from" -> Some `From
  | "to" -> Some `To
  | "cc" -> Some `Cc
  | "bcc" -> Some `Bcc
  | "replyTo" -> Some `Reply_to
  | "subject" -> Some `Subject
  | "sentAt" -> Some `Sent_at
  | "headers" -> Some `Headers
  | "bodyStructure" -> Some `Body_structure
  | "bodyValues" -> Some `Body_values
  | "textBody" -> Some `Text_body
  | "htmlBody" -> Some `Html_body
  | "attachments" -> Some `Attachments
  | "hasAttachment" -> Some `Has_attachment
  | "preview" -> Some `Preview
  | _ -> None

let property_of_string s : property option =
  match standard_property_of_string s with
  | Some p -> Some (p :> property)
  | None -> (
      match Mail_header.header_property_of_string s with
      | Some hp -> Some (`Header hp)
      | None -> None)

type standard_body_part_property =
  [ `Part_id
  | `Blob_id
  | `Size
  | `Part_headers
  | `Name
  | `Type
  | `Charset
  | `Disposition
  | `Cid
  | `Language
  | `Location
  | `Sub_parts ]

type body_part_property = [ standard_body_part_property | header_property ]

let standard_body_part_property_to_string :
    [< standard_body_part_property ] -> string = function
  | `Part_id -> "partId"
  | `Blob_id -> "blobId"
  | `Size -> "size"
  | `Part_headers -> "headers"
  | `Name -> "name"
  | `Type -> "type"
  | `Charset -> "charset"
  | `Disposition -> "disposition"
  | `Cid -> "cid"
  | `Language -> "language"
  | `Location -> "location"
  | `Sub_parts -> "subParts"

let body_part_property_to_string : [< body_part_property ] -> string = function
  | `Header hp -> Mail_header.header_property_to_string hp
  | #standard_body_part_property as p -> standard_body_part_property_to_string p

let standard_body_part_property_of_string s : standard_body_part_property option
    =
  match s with
  | "partId" -> Some `Part_id
  | "blobId" -> Some `Blob_id
  | "size" -> Some `Size
  | "headers" -> Some `Part_headers
  | "name" -> Some `Name
  | "type" -> Some `Type
  | "charset" -> Some `Charset
  | "disposition" -> Some `Disposition
  | "cid" -> Some `Cid
  | "language" -> Some `Language
  | "location" -> Some `Location
  | "subParts" -> Some `Sub_parts
  | _ -> None

(* RFC 8621 Section 4.1.4: a client may also request EmailBodyPart
   properties representing individual header fields, e.g. [header:Content-Type],
   following the same syntax and semantics as for the Email object. *)
let body_part_property_of_string s : body_part_property option =
  match standard_body_part_property_of_string s with
  | Some p -> Some (p :> body_part_property)
  | None -> (
      match Mail_header.header_property_of_string s with
      | Some hp -> Some (`Header hp)
      | None -> None)

type t = {
  id : Proto_id.t option;
  blob_id : Proto_id.t option;
  thread_id : Proto_id.t option;
  size : int64 option;
  received_at : Ptime.t option;
  mailbox_ids : (Proto_id.t * bool) list option;
  keywords : (Mail_keyword.t * bool) list option;
  message_id : string list option;
  in_reply_to : string list option;
  references : string list option;
  sender : Mail_address.t list option;
  from : Mail_address.t list option;
  to_ : Mail_address.t list option;
  cc : Mail_address.t list option;
  bcc : Mail_address.t list option;
  reply_to : Mail_address.t list option;
  subject : string option;
  sent_at : Ptime.t option;
  headers : Mail_header.t list option;
  body_structure : Mail_body.Part.t option;
  body_values : (string * Mail_body.Value.t) list option;
  text_body : Mail_body.Part.t list option;
  html_body : Mail_body.Part.t list option;
  attachments : Mail_body.Part.t list option;
  has_attachment : bool option;
  preview : string option;
  dynamic_headers : (string * Jsont.json) list;
  unknown : Proto_unknown.t;
}

let header_prefix = "header:"
let is_header_name n = String.starts_with ~prefix:header_prefix n

let v ?id ?blob_id ?thread_id ?size ?received_at ?mailbox_ids ?keywords
    ?message_id ?in_reply_to ?references ?sender ?from ?to_ ?cc ?bcc ?reply_to
    ?subject ?sent_at ?headers ?body_structure ?body_values ?text_body
    ?html_body ?attachments ?has_attachment ?preview ?(dynamic_headers = [])
    ?(unknown = Proto_unknown.empty) () =
  List.iter
    (fun (name, _) ->
      if not (is_header_name name) then
        invalid_arg
          (Printf.sprintf "Mail_email: %S is not a %s property" name
             header_prefix))
    dynamic_headers;
  {
    id;
    blob_id;
    thread_id;
    size;
    received_at;
    mailbox_ids;
    keywords;
    message_id;
    in_reply_to;
    references;
    sender;
    from;
    to_;
    cc;
    bcc;
    reply_to;
    subject;
    sent_at;
    headers;
    body_structure;
    body_values;
    text_body;
    html_body;
    attachments;
    has_attachment;
    preview;
    dynamic_headers;
    unknown;
  }

let empty = v ()
let id t = t.id
let of_mailboxes ids = List.map (fun i -> (i, true)) ids

let check_distinct_mailboxes fn ids =
  let rec check seen = function
    | [] -> ()
    | id :: rest ->
        if List.exists (Proto_id.equal id) seen then
          invalid_arg
            (Printf.sprintf "%s: duplicate mailbox id %S" fn
               (Proto_id.to_string id));
        check (id :: seen) rest
  in
  check [] ids

(* RFC 8621 Section 4.6. *)
let create ~mailbox_ids ?keywords ?sender ?from ?to_ ?cc ?bcc ?reply_to ?subject
    ?sent_at ?in_reply_to ?references ?text_body ?html_body () =
  if List.is_empty mailbox_ids then
    invalid_arg "Mail_email.create: mailbox_ids must not be empty";
  let body_value part_id body =
    Option.map (fun b -> (part_id, Mail_body.Value.v b)) body
  in
  let body_part part_id type_ body =
    Option.map (fun _ -> [ Mail_body.Part.v ~part_id ~type_ () ]) body
  in
  let body_values =
    List.filter_map Fun.id
      [ body_value "text" text_body; body_value "html" html_body ]
  in
  v ~mailbox_ids:(of_mailboxes mailbox_ids)
    ?keywords:(Option.map Mail_keyword.of_list keywords)
    ?sender ?from ?to_ ?cc ?bcc ?reply_to ?subject ?sent_at ?in_reply_to
    ?references
    ?body_values:(if List.is_empty body_values then None else Some body_values)
    ?text_body:(body_part "text" "text/plain" text_body)
    ?html_body:(body_part "html" "text/html" html_body)
    ()

let mailbox_ids_jsont =
  let kind = "Email mailboxIds" in
  let enc mailbox_ids =
    List.iter
      (fun (id, member) ->
        if not member then
          Jsont.Error.msgf Jsont.Meta.none
            "Email: mailboxId %S is mapped to false" (Proto_id.to_string id))
      mailbox_ids;
    mailbox_ids
  in
  Jsont.map ~kind ~dec:Fun.id ~enc Proto_json_map.id_to_bool

let unknown_member t name = Proto_unknown.find t.unknown name

let keyword_list t =
  match t.keywords with
  | None -> []
  | Some m -> List.filter_map (fun (k, b) -> if b then Some k else None) m

let has_keyword k t =
  match t.keywords with
  | None -> false
  | Some m -> List.exists (fun (k', b) -> b && Mail_keyword.equal k k') m

let mailbox_list t =
  match t.mailbox_ids with
  | None -> []
  | Some m -> List.filter_map (fun (id, b) -> if b then Some id else None) m

let in_mailbox id t =
  match t.mailbox_ids with
  | None -> false
  | Some m -> List.exists (fun (id', b) -> b && Proto_id.equal id id') m

let body_value t (part : Mail_body.Part.t) =
  match (t.body_values, part.part_id) with
  | Some values, Some part_id -> List.assoc_opt part_id values
  | _ -> None

let decode_header_value prop_name json =
  match Mail_header.property_form prop_name with
  | None -> None
  | Some (form, all) -> (
      match
        Jsont.Json.decode' (Mail_header.header_value_jsont ~form ~all) json
      with
      | Ok v -> Some v
      | Error _ -> None)

let find_header t key =
  match List.assoc_opt key t.dynamic_headers with
  | None -> None
  | Some json -> decode_header_value key json

let find_header_string t key =
  match find_header t key with
  | Some (Mail_header.String_single s) -> s
  | _ -> None

let find_header_addresses t key =
  match find_header t key with
  | Some (Mail_header.Addresses_single addrs) -> addrs
  | _ -> None

let find_header_grouped_addresses t key =
  match find_header t key with
  | Some (Mail_header.Grouped_single groups) -> groups
  | _ -> None

let find_header_strings t key =
  match find_header t key with
  | Some (Mail_header.Strings_single l) -> l
  | _ -> None

let find_header_date t key =
  match find_header t key with
  | Some (Mail_header.Date_single d) -> d
  | _ -> None

let find_header_all t key =
  match find_header t key with
  | Some (Mail_header.String_all l) -> Some l
  | _ -> None

let find_header_text t key =
  Option.map Mail_header.value_to_string (find_header t key)

let split_unknown (u : Proto_unknown.t) =
  match u with
  | Jsont.Object (mems, meta) ->
      let headers, rest =
        List.partition (fun ((name, _), _) -> is_header_name name) mems
      in
      ( List.map (fun ((name, _), json) -> (name, json)) headers,
        Jsont.Object (rest, meta) )
  | u -> ([], u)

let merge_unknown t =
  match t.unknown with
  | Jsont.Object (mems, meta) ->
      let headers =
        List.map
          (fun (name, json) -> (Jsont.Json.name name, json))
          t.dynamic_headers
      in
      Jsont.Object (mems @ headers, meta)
  | u -> u

let jsont =
  let kind = "Email" in
  let body_values_jsont = Proto_json_map.of_string Mail_body.Value.jsont in
  (* The address and message-id header properties are [T[]|null]
     (RFC 8621 Section 4.1.3): null means the header field is not present,
     which [nullable_mem] maps to [None] and re-encodes by omitting the
     member rather than by emitting an empty array. *)
  let addr_list = Jsont.list Mail_address.jsont in
  let str_list = Jsont.list Jsont.string in
  let part_list = Jsont.list Mail_body.Part.jsont in
  let hdr_list = Jsont.list Mail_header.jsont in
  let make id blob_id thread_id size received_at mailbox_ids keywords message_id
      in_reply_to references sender from to_ cc bcc reply_to subject sent_at
      headers body_structure body_values text_body html_body attachments
      has_attachment preview unknown =
    let dynamic_headers, unknown = split_unknown unknown in
    {
      id;
      blob_id;
      thread_id;
      size;
      received_at;
      mailbox_ids;
      keywords;
      message_id;
      in_reply_to;
      references;
      sender;
      from;
      to_;
      cc;
      bcc;
      reply_to;
      subject;
      sent_at;
      headers;
      body_structure;
      body_values;
      text_body;
      html_body;
      attachments;
      has_attachment;
      preview;
      dynamic_headers;
      unknown;
    }
  in
  Jsont.Object.map ~kind make
  (* The metadata properties are nullable rather than merely optional:
     RFC 8621 Section 4.9 has [Email/parse] return [id], [mailboxIds],
     [keywords] and [receivedAt] as an explicit [null], and [threadId] too
     unless "the server can calculate which Thread the Email would be
     assigned to were it to be imported". *)
  |> Proto_json_map.nullable_mem "id" Proto_id.jsont ~enc:(fun e -> e.id)
  |> Proto_json_map.nullable_mem "blobId" Proto_id.jsont ~enc:(fun e ->
      e.blob_id)
  |> Proto_json_map.nullable_mem "threadId" Proto_id.jsont ~enc:(fun e ->
      e.thread_id)
  |> Proto_json_map.nullable_mem "size" Proto_int53.Unsigned.jsont
       ~enc:(fun e -> e.size)
  |> Proto_json_map.nullable_mem "receivedAt" Proto_date.utc_jsont
       ~enc:(fun e -> e.received_at)
  |> Proto_json_map.nullable_mem "mailboxIds" mailbox_ids_jsont ~enc:(fun e ->
      e.mailbox_ids)
  |> Proto_json_map.nullable_mem "keywords" Mail_keyword.map_jsont
       ~enc:(fun e -> e.keywords)
  |> Proto_json_map.nullable_mem "messageId" str_list ~enc:(fun e ->
      e.message_id)
  |> Proto_json_map.nullable_mem "inReplyTo" str_list ~enc:(fun e ->
      e.in_reply_to)
  |> Proto_json_map.nullable_mem "references" str_list ~enc:(fun e ->
      e.references)
  |> Proto_json_map.nullable_mem "sender" addr_list ~enc:(fun e -> e.sender)
  |> Proto_json_map.nullable_mem "from" addr_list ~enc:(fun e -> e.from)
  |> Proto_json_map.nullable_mem "to" addr_list ~enc:(fun e -> e.to_)
  |> Proto_json_map.nullable_mem "cc" addr_list ~enc:(fun e -> e.cc)
  |> Proto_json_map.nullable_mem "bcc" addr_list ~enc:(fun e -> e.bcc)
  |> Proto_json_map.nullable_mem "replyTo" addr_list ~enc:(fun e -> e.reply_to)
  (* subject is String|null and sentAt is Date|null (RFC 8621
     Section 4.1.3). *)
  |> Proto_json_map.nullable_mem "subject" Jsont.string ~enc:(fun e ->
      e.subject)
  |> Proto_json_map.nullable_mem "sentAt" Proto_date.jsont ~enc:(fun e ->
      e.sent_at)
  |> Proto_json_map.nullable_mem "headers" hdr_list ~enc:(fun e -> e.headers)
  |> Proto_json_map.nullable_mem "bodyStructure" Mail_body.Part.jsont
       ~enc:(fun e -> e.body_structure)
  |> Proto_json_map.nullable_mem "bodyValues" body_values_jsont ~enc:(fun e ->
      e.body_values)
  |> Proto_json_map.nullable_mem "textBody" part_list ~enc:(fun e ->
      e.text_body)
  |> Proto_json_map.nullable_mem "htmlBody" part_list ~enc:(fun e ->
      e.html_body)
  |> Proto_json_map.nullable_mem "attachments" part_list ~enc:(fun e ->
      e.attachments)
  |> Proto_json_map.nullable_mem "hasAttachment" Jsont.bool ~enc:(fun e ->
      e.has_attachment)
  |> Proto_json_map.nullable_mem "preview" Jsont.string ~enc:(fun e ->
      e.preview)
  (* RFC 8621 Section 4.1.3 lets a client ask for "header:*" properties,
     which are not members of the type above; keeping the unknown members is
     what makes them reachable and re-encodable. *)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:merge_unknown
  |> Jsont.Object.finish

module Filter_condition = struct
  type t = {
    in_mailbox : Proto_id.t option;
    in_mailbox_other_than : Proto_id.t list option;
    before : Ptime.t option;
    after : Ptime.t option;
    min_size : int64 option;
    max_size : int64 option;
    all_in_thread_have_keyword : Mail_keyword.t option;
    some_in_thread_have_keyword : Mail_keyword.t option;
    none_in_thread_have_keyword : Mail_keyword.t option;
    has_keyword : Mail_keyword.t option;
    not_keyword : Mail_keyword.t option;
    has_attachment : bool option;
    text : string option;
    from : string option;
    to_ : string option;
    cc : string option;
    bcc : string option;
    subject : string option;
    body : string option;
    header : (string * string option) option;
  }

  let empty =
    {
      in_mailbox = None;
      in_mailbox_other_than = None;
      before = None;
      after = None;
      min_size = None;
      max_size = None;
      all_in_thread_have_keyword = None;
      some_in_thread_have_keyword = None;
      none_in_thread_have_keyword = None;
      has_keyword = None;
      not_keyword = None;
      has_attachment = None;
      text = None;
      from = None;
      to_ = None;
      cc = None;
      bcc = None;
      subject = None;
      body = None;
      header = None;
    }

  let header_jsont =
    let kind = "HeaderFilter" in
    let dec json =
      match json with
      | Jsont.Array ([ Jsont.String (name, _) ], _) -> (name, None)
      | Jsont.Array ([ Jsont.String (name, _); Jsont.String (value, _) ], _) ->
          (name, Some value)
      | j ->
          Jsont.Error.msgf (Jsont.Json.meta j)
            "%s: expected [name] or [name, value]" kind
    in
    let enc (name, value) =
      let str s = Jsont.String (s, Jsont.Meta.none) in
      match value with
      | None -> Jsont.Array ([ str name ], Jsont.Meta.none)
      | Some v -> Jsont.Array ([ str name; str v ], Jsont.Meta.none)
    in
    Jsont.map ~kind ~dec ~enc Jsont.json

  let jsont =
    let kind = "EmailFilterCondition" in
    let make in_mailbox in_mailbox_other_than before after min_size max_size
        all_in_thread_have_keyword some_in_thread_have_keyword
        none_in_thread_have_keyword has_keyword not_keyword has_attachment text
        from to_ cc bcc subject body header =
      {
        in_mailbox;
        in_mailbox_other_than;
        before;
        after;
        min_size;
        max_size;
        all_in_thread_have_keyword;
        some_in_thread_have_keyword;
        none_in_thread_have_keyword;
        has_keyword;
        not_keyword;
        has_attachment;
        text;
        from;
        to_;
        cc;
        bcc;
        subject;
        body;
        header;
      }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "inMailbox" Proto_id.jsont ~enc:(fun f ->
        f.in_mailbox)
    |> Jsont.Object.opt_mem "inMailboxOtherThan" (Jsont.list Proto_id.jsont)
         ~enc:(fun f -> f.in_mailbox_other_than)
    |> Jsont.Object.opt_mem "before" Proto_date.utc_jsont ~enc:(fun f ->
        f.before)
    |> Jsont.Object.opt_mem "after" Proto_date.utc_jsont ~enc:(fun f -> f.after)
    |> Jsont.Object.opt_mem "minSize" Proto_int53.Unsigned.jsont ~enc:(fun f ->
        f.min_size)
    |> Jsont.Object.opt_mem "maxSize" Proto_int53.Unsigned.jsont ~enc:(fun f ->
        f.max_size)
    |> Jsont.Object.opt_mem "allInThreadHaveKeyword" Mail_keyword.jsont
         ~enc:(fun f -> f.all_in_thread_have_keyword)
    |> Jsont.Object.opt_mem "someInThreadHaveKeyword" Mail_keyword.jsont
         ~enc:(fun f -> f.some_in_thread_have_keyword)
    |> Jsont.Object.opt_mem "noneInThreadHaveKeyword" Mail_keyword.jsont
         ~enc:(fun f -> f.none_in_thread_have_keyword)
    |> Jsont.Object.opt_mem "hasKeyword" Mail_keyword.jsont ~enc:(fun f ->
        f.has_keyword)
    |> Jsont.Object.opt_mem "notKeyword" Mail_keyword.jsont ~enc:(fun f ->
        f.not_keyword)
    |> Jsont.Object.opt_mem "hasAttachment" Jsont.bool ~enc:(fun f ->
        f.has_attachment)
    |> Jsont.Object.opt_mem "text" Jsont.string ~enc:(fun f -> f.text)
    |> Jsont.Object.opt_mem "from" Jsont.string ~enc:(fun f -> f.from)
    |> Jsont.Object.opt_mem "to" Jsont.string ~enc:(fun f -> f.to_)
    |> Jsont.Object.opt_mem "cc" Jsont.string ~enc:(fun f -> f.cc)
    |> Jsont.Object.opt_mem "bcc" Jsont.string ~enc:(fun f -> f.bcc)
    |> Jsont.Object.opt_mem "subject" Jsont.string ~enc:(fun f -> f.subject)
    |> Jsont.Object.opt_mem "body" Jsont.string ~enc:(fun f -> f.body)
    |> Jsont.Object.opt_mem "header" header_jsont ~enc:(fun f -> f.header)
    |> Jsont.Object.finish
end

type filter = Filter_condition.t Proto_filter.filter

let filter_jsont = Proto_filter.filter_jsont Filter_condition.jsont

let filter ?in_mailbox ?in_mailbox_other_than ?before ?after ?min_size ?max_size
    ?all_in_thread_have_keyword ?some_in_thread_have_keyword
    ?none_in_thread_have_keyword ?has_keyword ?not_keyword ?has_attachment ?text
    ?from ?to_ ?cc ?bcc ?subject ?body ?header () =
  Proto_filter.Condition
    {
      Filter_condition.in_mailbox;
      in_mailbox_other_than;
      before;
      after;
      min_size;
      max_size;
      all_in_thread_have_keyword;
      some_in_thread_have_keyword;
      none_in_thread_have_keyword;
      has_keyword;
      not_keyword;
      has_attachment;
      text;
      from;
      to_;
      cc;
      bcc;
      subject;
      body;
      header;
    }

type sort_property =
  [ `Received_at
  | `Size
  | `From
  | `To
  | `Subject
  | `Sent_at
  | `Has_keyword of Mail_keyword.t
  | `All_in_thread_have_keyword of Mail_keyword.t
  | `Some_in_thread_have_keyword of Mail_keyword.t ]

let sort ?ascending ?collation p =
  let property, keyword =
    match p with
    | `Received_at -> ("receivedAt", None)
    | `Size -> ("size", None)
    | `From -> ("from", None)
    | `To -> ("to", None)
    | `Subject -> ("subject", None)
    | `Sent_at -> ("sentAt", None)
    | `Has_keyword k -> ("hasKeyword", Some (Mail_keyword.to_string k))
    | `All_in_thread_have_keyword k ->
        ("allInThreadHaveKeyword", Some (Mail_keyword.to_string k))
    | `Some_in_thread_have_keyword k ->
        ("someInThreadHaveKeyword", Some (Mail_keyword.to_string k))
  in
  Proto_filter.comparator ?is_ascending:ascending ?collation ?keyword property

type get_args_extra = {
  body_properties : body_part_property list option;
  fetch_text_body_values : bool;
  fetch_html_body_values : bool;
  fetch_all_body_values : bool;
  max_body_value_bytes : int64 option;
}

let get_args_extra ?body_properties ?(fetch_text_body_values = false)
    ?(fetch_html_body_values = false) ?(fetch_all_body_values = false)
    ?max_body_value_bytes () =
  {
    body_properties;
    fetch_text_body_values;
    fetch_html_body_values;
    fetch_all_body_values;
    max_body_value_bytes;
  }

let body_part_property_list_jsont =
  let dec s =
    match body_part_property_of_string s with
    | Some p -> p
    | None -> Jsont.Error.msgf Jsont.Meta.none "Unknown body property: %s" s
  in
  Jsont.list
    (Jsont.map ~kind:"body_part_property" ~dec ~enc:body_part_property_to_string
       Jsont.string)

let get_args_extra_mems ~extra map =
  map
  |> Proto_json_map.nullable_mem "bodyProperties" body_part_property_list_jsont
       ~enc:(fun a -> (extra a).body_properties)
  |> Jsont.Object.mem "fetchTextBodyValues" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> (extra a).fetch_text_body_values)
       ~enc_omit:(fun b -> not b)
  |> Jsont.Object.mem "fetchHTMLBodyValues" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> (extra a).fetch_html_body_values)
       ~enc_omit:(fun b -> not b)
  |> Jsont.Object.mem "fetchAllBodyValues" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> (extra a).fetch_all_body_values)
       ~enc_omit:(fun b -> not b)
  |> Proto_json_map.nullable_mem "maxBodyValueBytes" Proto_int53.Unsigned.jsont
       ~enc:(fun a -> (extra a).max_body_value_bytes)

let get_args_extra_jsont =
  let kind = "Email/get arguments" in
  let make body_properties fetch_text_body_values fetch_html_body_values
      fetch_all_body_values max_body_value_bytes =
    {
      body_properties;
      fetch_text_body_values;
      fetch_html_body_values;
      fetch_all_body_values;
      max_body_value_bytes;
    }
  in
  Jsont.Object.map ~kind make
  |> get_args_extra_mems ~extra:Fun.id
  |> Jsont.Object.finish

let find_by_id map id =
  Option.bind map
    (List.find_map (fun (k, v) -> if Proto_id.equal k id then Some v else None))

module Import = struct
  type email = {
    blob_id : Proto_id.t;
    mailbox_ids : (Proto_id.t * bool) list;
    keywords : (Mail_keyword.t * bool) list;
    received_at : Ptime.t option;
  }

  let email ~blob_id ~mailbox_ids ?(keywords = []) ?received_at () =
    if List.is_empty mailbox_ids then
      invalid_arg "Jmap.Proto.Email.Import.email: mailbox_ids must not be empty";
    check_distinct_mailboxes "Jmap.Proto.Email.Import.email" mailbox_ids;
    {
      blob_id;
      mailbox_ids = of_mailboxes mailbox_ids;
      keywords = Mail_keyword.of_list keywords;
      received_at;
    }

  let email_jsont =
    let kind = "EmailImport" in
    let make blob_id mailbox_ids keywords received_at =
      { blob_id; mailbox_ids; keywords; received_at }
    in
    let validate email =
      if List.is_empty email.mailbox_ids then
        Jsont.Error.msg Jsont.Meta.none
          "EmailImport: mailboxIds must not be empty";
      email
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "blobId" Proto_id.jsont ~enc:(fun e -> e.blob_id)
    |> Jsont.Object.mem "mailboxIds" mailbox_ids_jsont ~enc:(fun e ->
        e.mailbox_ids)
    (* RFC 8621 Section 4.8: keywords "String[Boolean] (default: {})". *)
    |> Jsont.Object.mem "keywords" Mail_keyword.map_jsont ~dec_absent:(fun () -> [])
         ~enc:(fun e -> e.keywords)
         ~enc_omit:List.is_empty
    |> Proto_json_map.nullable_mem "receivedAt" Proto_date.utc_jsont
         ~enc:(fun e -> e.received_at)
    |> Jsont.Object.finish
    |> Jsont.map ~kind ~dec:validate ~enc:validate

  type args = {
    account_id : Proto_id.t;
    if_in_state : string option;
    emails : (t Proto_id.creation * email) list;
  }

  let args ~account_id ?if_in_state ~emails () =
    { account_id; if_in_state; emails }

  let args_jsont =
    let kind = "Email/import arguments" in
    let make account_id if_in_state emails =
      { account_id; if_in_state; emails }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
    |> Proto_json_map.nullable_mem "ifInState" Jsont.string ~enc:(fun a ->
        a.if_in_state)
    |> Jsont.Object.mem "emails" (Proto_json_map.of_creation email_jsont)
         ~enc:(fun a -> a.emails)
    |> Jsont.Object.finish

  type response = {
    account_id : Proto_id.t;
    old_state : string option;
    new_state : string;
    created : (Proto_id.t * t) list option;
    not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
  }

  let response_jsont =
    let kind = "Email/import response" in
    let make account_id old_state new_state created not_created =
      { account_id; old_state; new_state; created; not_created }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
    |> Proto_json_map.nullable_mem "oldState" Jsont.string ~enc:(fun r ->
        r.old_state)
    |> Jsont.Object.mem "newState" Jsont.string ~enc:(fun r -> r.new_state)
    |> Proto_json_map.nullable_mem "created" (Proto_json_map.of_id jsont)
         ~enc:(fun r -> r.created)
    |> Proto_json_map.nullable_mem "notCreated"
         (Proto_json_map.of_id Proto_error.Set_error.jsont) ~enc:(fun r ->
           r.not_created)
    |> Jsont.Object.finish

  let find map c = find_by_id map (Proto_id.creation_id c)
  let created r c = find r.created c
  let not_created r c = find r.not_created c
end

module Parse = struct
  type args = {
    account_id : Proto_id.t;
    blob_ids : Proto_id.t list;
    properties : string list option;
    extra : get_args_extra;
  }

  let args ~account_id ~blob_ids ?properties ?body_properties
      ?fetch_text_body_values ?fetch_html_body_values ?fetch_all_body_values
      ?max_body_value_bytes () =
    {
      account_id;
      blob_ids;
      properties;
      extra =
        get_args_extra ?body_properties ?fetch_text_body_values
          ?fetch_html_body_values ?fetch_all_body_values ?max_body_value_bytes
          ();
    }

  let args_jsont =
    let kind = "Email/parse arguments" in
    let make account_id blob_ids properties body_properties
        fetch_text_body_values fetch_html_body_values fetch_all_body_values
        max_body_value_bytes =
      {
        account_id;
        blob_ids;
        properties;
        extra =
          {
            body_properties;
            fetch_text_body_values;
            fetch_html_body_values;
            fetch_all_body_values;
            max_body_value_bytes;
          };
      }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
    |> Jsont.Object.mem "blobIds" (Jsont.list Proto_id.jsont) ~enc:(fun a ->
        a.blob_ids)
    |> Proto_json_map.nullable_mem "properties" (Jsont.list Jsont.string)
         ~enc:(fun a -> a.properties)
    |> get_args_extra_mems ~extra:(fun a -> a.extra)
    |> Jsont.Object.finish

  type response = {
    account_id : Proto_id.t;
    parsed : (Proto_id.t * t) list option;
    not_parsable : Proto_id.t list option;
    not_found : Proto_id.t list option;
  }

  let response_jsont =
    let kind = "Email/parse response" in
    let make account_id parsed not_parsable not_found =
      { account_id; parsed; not_parsable; not_found }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
    |> Proto_json_map.nullable_mem "parsed" (Proto_json_map.of_id jsont)
         ~enc:(fun r -> r.parsed)
    |> Proto_json_map.nullable_mem "notParsable" (Jsont.list Proto_id.jsont)
         ~enc:(fun r -> r.not_parsable)
    |> Proto_json_map.nullable_mem "notFound" (Jsont.list Proto_id.jsont)
         ~enc:(fun r -> r.not_found)
    |> Jsont.Object.finish

  let parsed r blob_id = find_by_id r.parsed blob_id
end

module Patch = struct
  let json_true = Jsont.Json.bool true

  let bool_map to_key entries =
    Jsont.Json.object'
      (List.filter_map
         (fun (k, b) ->
           if b then
             Some (Jsont.Json.mem (Jsont.Json.name (to_key k)) json_true)
           else None)
         entries)

  (* A patch is built by the client, so the keyword it names must satisfy the
     RFC 8621 Section 4.1.1 syntax; a malformed one would be rejected by the
     server as an invalidPatch, later and less clearly. *)
  let keyword kw =
    match Mail_keyword.validate kw with
    | Ok kw -> Mail_keyword.to_string kw
    | Error msg -> invalid_arg ("Mail_email.Patch: " ^ msg)

  let set_keyword kw =
    Proto_patch.path [ "keywords"; keyword kw ] (Some json_true)

  let remove_keyword kw = Proto_patch.path [ "keywords"; keyword kw ] None

  let set_keywords kws =
    Proto_patch.path [ "keywords" ]
      (Some (bool_map Mail_keyword.to_string (Mail_keyword.of_list kws)))

  let add_to_mailbox id =
    Proto_patch.path [ "mailboxIds"; Proto_id.to_string id ] (Some json_true)

  let remove_from_mailbox id =
    Proto_patch.path [ "mailboxIds"; Proto_id.to_string id ] None

  let set_mailboxes ids =
    check_distinct_mailboxes "Mail_email.Patch.set_mailboxes" ids;
    Proto_patch.path [ "mailboxIds" ]
      (Some (bool_map Proto_id.to_string (of_mailboxes ids)))
end

let creation = Proto_id.creation
