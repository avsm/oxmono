(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** RFC 8621 mail codec tests, focused on the nullable properties that servers
    routinely send as an explicit JSON [null]. *)

open Jmap.Proto

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let decode jsont json_str = Jsont_bytesrw.decode_string' jsont json_str
let encode jsont value = Jsont_bytesrw.encode_string' jsont value

let decode_file name jsont path =
  match decode jsont (read_file path) with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

let test_decode_failure name jsont path () =
  match decode jsont (read_file path) with
  | Ok _ -> Alcotest.failf "%s: expected decode failure but got success" name
  | Error _ -> ()

(* Round trip through a decode / encode / decode cycle and check that the
   re-encoded JSON is byte-identical to the first encoding. *)
let roundtrip name jsont path =
  let v = decode_file name jsont path in
  match encode jsont v with
  | Error e ->
      Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)
  | Ok s1 -> (
      match decode jsont s1 with
      | Error e ->
          Alcotest.failf "%s: re-decode failed: %s" name
            (Jsont.Error.to_string e)
      | Ok v' -> (
          match encode jsont v' with
          | Error e ->
              Alcotest.failf "%s: re-encode failed: %s" name
                (Jsont.Error.to_string e)
          | Ok s2 ->
              Alcotest.(check string) (name ^ ": stable re-encode") s1 s2;
              (s1, v')))

let test_roundtrip name jsont path () = ignore (roundtrip name jsont path)

let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n = 0 || go 0

let check_omitted name member json =
  Alcotest.(check bool)
    (Printf.sprintf "%s: %S omitted" name member)
    false
    (contains ~needle:(Printf.sprintf "%S" member) json)

let some name = function
  | Some v -> v
  | None -> Alcotest.failf "%s: expected Some" name

(* Mailbox (RFC 8621 Section 2) *)
module Mailbox_tests = struct
  let path = "mailbox/top_level_null.json"

  let test_null_parent_and_role () =
    let mb = decode_file "mailbox" Mailbox.jsont path in
    Alcotest.(check bool) "parentId is None" true (mb.Mailbox.parent_id = None);
    Alcotest.(check bool) "role is None" true (mb.Mailbox.role = None);
    Alcotest.(check string) "name" "Inbox" (some "name" mb.Mailbox.name)

  (* RFC 8621 Section 2 types parentId Id|null and role String|null, so a
     re-encode must state the null rather than drop the member and turn "no
     parent" into "not asked for". *)
  let test_nulls_survive_a_reencode () =
    let json, _ = roundtrip "mailbox" Mailbox.jsont path in
    Alcotest.(check bool)
      "parentId is null" true
      (contains ~needle:{|"parentId":null|} json);
    Alcotest.(check bool)
      "role is null" true
      (contains ~needle:{|"role":null|} json)

  let test_subscribed_is_not_a_role () =
    Alcotest.(check bool)
      "subscribed has no special use" true
      (Mailbox.special_use_of_role (`Other "subscribed") = None);
    Alcotest.(check bool)
      "inbox has one" true
      (Mailbox.special_use_of_role `Inbox = Some `Inbox);
    (* Decoding a role folds case, so the wire value is rewritten. *)
    Alcotest.(check bool) "INBOX" true (Mailbox.role_of_string "INBOX" = `Inbox);
    Alcotest.(check string)
      "role_to_string" "inbox"
      (Mailbox.role_to_string (Mailbox.role_of_string "INBOX"));
    Alcotest.(check bool)
      "SUBSCRIBED stays other" true
      (Mailbox.role_of_string "SUBSCRIBED" = `Other "SUBSCRIBED")

  let tests =
    [
      ("null parentId and role decode", `Quick, test_null_parent_and_role);
      ("null parentId and role re-encode", `Quick, test_nulls_survive_a_reencode);
      ("subscribed is not a role", `Quick, test_subscribed_is_not_a_role);
      ("roundtrip", `Quick, test_roundtrip "mailbox" Mailbox.jsont path);
    ]
end

(* Email (RFC 8621 Section 4.1.3) *)
module Email_tests = struct
  let path = "email/no_subject_no_date.json"

  let test_null_subject_and_sent_at () =
    let e = decode_file "email" Email.jsont path in
    Alcotest.(check bool) "subject is None" true (e.Email.subject = None);
    Alcotest.(check bool) "sentAt is None" true (e.Email.sent_at = None);
    Alcotest.(check bool) "to is None" true (e.Email.to_ = None);
    Alcotest.(check bool) "messageId is None" true (e.Email.message_id = None);
    Alcotest.(check int)
      "from has one address" 1
      (List.length (some "from" e.Email.from))

  let test_address_pp_escapes_control_bytes () =
    let address =
      Email_address.create ~name:"display \"café\"\\name\n"
        "mail\027@example.invalid"
    in
    let text = Format.asprintf "%a" Email_address.pp address in
    Alcotest.(check bool) "no newline" false (String.contains text '\n');
    Alcotest.(check bool) "no escape byte" false (String.contains text '\027');
    Alcotest.(check bool)
      "printable UTF-8, quotes and backslashes are preserved" true
      (contains ~needle:"display \"café\"\\name\\x0A" text)

  (* A [null] address list means "no such header field"; re-encoding it as
     [[]] would create an empty header field instead (RFC 8621 4.1.3). *)
  let test_null_list_not_reencoded_as_empty () =
    let json, _ = roundtrip "email" Email.jsont path in
    check_omitted "email" "to" json;
    check_omitted "email" "cc" json;
    check_omitted "email" "messageId" json;
    check_omitted "email" "subject" json;
    Alcotest.(check bool) "from is kept" true (contains ~needle:"\"from\"" json)

  (* RFC 8621 Section 4.6: the object of an Email/set create is the Email
     object with the server-set properties omitted, its body a bodyValues
     entry named by a textBody part. *)
  let test_create_object () =
    let e =
      Email.v
        ~mailbox_ids:(Email.of_mailboxes [ Id.of_string_exn "Mdrafts" ])
        ~keywords:(Keyword.of_list [ `Draft ])
        ~from:[ Email_address.create ~name:"Me" "me@example.com" ]
        ~to_:[ Email_address.create "you@example.com" ]
        ~subject:"Hello"
        ~body_values:[ ("1", Email_body.Value.v "Hello, world.") ]
        ~text_body:[ Email_body.Part.v ~part_id:"1" ~type_:"text/plain" () ]
        ()
    in
    let json =
      match encode Email.jsont e with
      | Ok s -> s
      | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
    in
    Alcotest.(check string)
      "create object"
      {|{"mailboxIds":{"Mdrafts":true},"keywords":{"$draft":true},"from":[{"name":"Me","email":"me@example.com"}],"to":[{"email":"you@example.com"}],"subject":"Hello","bodyValues":{"1":{"value":"Hello, world."}},"textBody":[{"partId":"1","type":"text/plain"}]}|}
      json;
    (* Every unset property is omitted, not encoded as null. *)
    Alcotest.(check string)
      "empty object" "{}"
      (match encode Email.jsont Email.empty with
      | Ok s -> s
      | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err))

  (* RFC 8620 Section 2 has a client ignore properties it does not
     understand, and every other object keeps them so that a value survives
     a re-encode.  A "header:*" member is a property the client asked for
     and lands in [dynamic_headers] instead. *)
  let test_unknown_members_kept () =
    let json = {|{"id":"M1","x-vendor":{"a":1},"header:X-Foo:asText":"bar"}|} in
    match decode Email.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok e -> (
        Alcotest.(check bool)
          "an extension member is kept" true
          (Email.unknown_member e "x-vendor" <> None);
        Alcotest.(check bool)
          "a header property is not an unknown member" true
          (Email.unknown_member e "header:X-Foo:asText" = None);
        Alcotest.(check (option string))
          "the header value decodes" (Some "bar")
          (Email.find_header_string e "header:X-Foo:asText");
        Alcotest.(check (option string))
          "and renders" (Some "bar")
          (Email.find_header_text e "header:X-Foo:asText");
        match encode Email.jsont e with
        | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
        | Ok out ->
            Alcotest.(check bool)
              "the extension member survives" true
              (contains ~needle:{|"x-vendor"|} out);
            Alcotest.(check bool)
              "the header property survives" true
              (contains ~needle:{|"header:X-Foo:asText"|} out))

  (* A dynamic header is keyed by the whole property name, so a key without
     the prefix would encode as a second member of a name the object already
     defines. *)
  let test_v_rejects_a_bare_dynamic_header () =
    match
      Email.v ~dynamic_headers:[ ("subject", Jsont.Json.string "x") ] ()
    with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "expected Invalid_argument"

  (* RFC 8621 Section 4.1.1 maps a keyword to true; a false value is not a
     shape a server sends, and the accessors read the map as a set. *)
  let test_keyword_accessors () =
    let e =
      Email.v ~keywords:(Keyword.of_list [ `Seen; `Flagged ]) ~subject:"x" ()
    in
    Alcotest.(check (list string))
      "the keywords set" [ "$seen"; "$flagged" ]
      (List.map Keyword.to_string (Email.keyword_list e));
    Alcotest.(check bool) "a keyword it has" true (Email.has_keyword `Seen e);
    Alcotest.(check bool)
      "case is folded" true
      (Email.has_keyword (Keyword.of_string "$SEEN") e);
    Alcotest.(check bool)
      "a keyword it lacks" false
      (Email.has_keyword `Draft e);
    Alcotest.(check (list string))
      "an unrequested property" []
      (List.map Keyword.to_string (Email.keyword_list Email.empty));
    Alcotest.(check bool)
      "and has no keyword" false
      (Email.has_keyword `Seen Email.empty);
    match decode Email.jsont {|{"keywords":{"$seen":true,"$draft":false}}|} with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "a false keyword value must be rejected"

  (* RFC 8621 Section 4.1.4 keys bodyValues by the partId of the part. *)
  let test_body_value () =
    let part = Email_body.Part.v ~part_id:"1" ~type_:"text/plain" () in
    let other = Email_body.Part.v ~part_id:"2" ~type_:"text/html" () in
    let untyped = Email_body.Part.v ~type_:"text/plain" () in
    let e =
      Email.v
        ~body_values:[ ("1", Email_body.Value.v "Hello, world.") ]
        ~text_body:[ part ] ()
    in
    Alcotest.(check (option string))
      "the value of the part" (Some "Hello, world.")
      (Option.map
         (fun (v : Email_body.Value.t) -> v.value)
         (Email.body_value e part));
    Alcotest.(check bool)
      "a part with no value" true
      (Option.is_none (Email.body_value e other));
    Alcotest.(check bool)
      "a part with no partId" true
      (Option.is_none (Email.body_value e untyped));
    Alcotest.(check bool)
      "values that were not fetched" true
      (Option.is_none (Email.body_value Email.empty part))

  let tests =
    [
      ("Email.v builds a create object", `Quick, test_create_object);
      ("keyword_list and has_keyword", `Quick, test_keyword_accessors);
      ("body_value joins a part to its value", `Quick, test_body_value);
      ("unknown members are kept", `Quick, test_unknown_members_kept);
      ( "v rejects a dynamic header without the prefix",
        `Quick,
        test_v_rejects_a_bare_dynamic_header );
      ( "address printers escape control bytes",
        `Quick,
        test_address_pp_escapes_control_bytes );
      ("null subject and sentAt decode", `Quick, test_null_subject_and_sent_at);
      ( "null address list is not re-encoded as []",
        `Quick,
        test_null_list_not_reencoded_as_empty );
      ("roundtrip", `Quick, test_roundtrip "email" Email.jsont path);
    ]
end

(* EmailBodyPart (RFC 8621 Section 4.1.4) *)
module Body_tests = struct
  let leaf = "email_body/leaf_null.json"
  let no_type = "email_body/no_type.json"

  let test_null_language_and_sub_parts () =
    let p = decode_file "body" Email_body.Part.jsont leaf in
    Alcotest.(check bool)
      "language is None" true
      (p.Email_body.Part.language = None);
    Alcotest.(check bool)
      "subParts is None" true
      (p.Email_body.Part.sub_parts = None);
    Alcotest.(check string)
      "type" "text/plain"
      (some "type" p.Email_body.Part.type_)

  (* RFC 8621 Section 4.2: the client chooses [bodyProperties], so [type]
     may legitimately be absent from the response. *)
  let test_absent_type () =
    let p = decode_file "body" Email_body.Part.jsont no_type in
    Alcotest.(check bool) "type is None" true (p.Email_body.Part.type_ = None)

  (* RFC 8621 Section 4.1.4: a client may name "header:*" properties in the
     [bodyProperties] argument of an Email/get, and the server returns them
     on each body part.  They are not EmailBodyPart properties, so they are
     kept as unknown members and read back through [header_property]. *)
  let headers = "email_body/header_properties.json"

  let test_header_properties () =
    let p = decode_file "body" Email_body.Part.jsont headers in
    Alcotest.(check (option string))
      "header:Content-Type" (Some "image/png; name=logo.png")
      (Email_body.Part.header_property p "header:Content-Type");
    Alcotest.(check (option string))
      "header:Content-Id:asText" (Some "logo@example.com")
      (Email_body.Part.header_property p "header:Content-Id:asText");
    (* A non-string form is not a [header_property], but is still kept. *)
    Alcotest.(check bool)
      "header:Content-Language:all is not a string" true
      (Email_body.Part.header_property p "header:Content-Language:all" = None);
    Alcotest.(check bool)
      "header:Content-Language:all is kept" true
      (Email_body.Part.unknown_member p "header:Content-Language:all" <> None);
    Alcotest.(check bool)
      "a property not asked for" true
      (Email_body.Part.header_property p "header:Subject" = None);
    (* A known property is not an unknown member. *)
    Alcotest.(check bool)
      "type is a real property" true
      (Email_body.Part.unknown_member p "type" = None)

  let test_header_properties_re_encoded () =
    let json, _ = roundtrip "body" Email_body.Part.jsont headers in
    Alcotest.(check bool)
      "header:Content-Type re-encoded" true
      (contains ~needle:{|"header:Content-Type"|} json)

  let tests =
    [
      ( "null language and subParts decode",
        `Quick,
        test_null_language_and_sub_parts );
      ("absent type decodes", `Quick, test_absent_type);
      ("header:* properties are kept", `Quick, test_header_properties);
      ( "header:* properties survive a re-encode",
        `Quick,
        test_header_properties_re_encoded );
      ( "roundtrip header properties",
        `Quick,
        test_roundtrip "body" Email_body.Part.jsont headers );
      ( "roundtrip leaf",
        `Quick,
        test_roundtrip "body" Email_body.Part.jsont leaf );
      ( "roundtrip without type",
        `Quick,
        test_roundtrip "body" Email_body.Part.jsont no_type );
    ]
end

(* Identity (RFC 8621 Section 6) *)
module Identity_tests = struct
  let path = "identity/rfc8621_6_4.json"

  let test_null_reply_to_and_bcc () =
    let i = decode_file "identity" Identity.jsont path in
    Alcotest.(check bool) "replyTo is None" true (i.Identity.reply_to = None);
    Alcotest.(check bool) "bcc is None" true (i.Identity.bcc = None);
    Alcotest.(check string)
      "email" "*@example.com"
      (some "email" i.Identity.email)

  (* Cyrus omits replyTo, bcc and both signatures entirely; RFC 8621
     Section 6 gives them defaults, so absent must decode as [None] rather
     than fail. *)
  let cyrus = "identity/cyrus_get.json"

  let test_absent_optional_properties () =
    let i = decode_file "identity" Identity.jsont cyrus in
    Alcotest.(check bool) "replyTo is None" true (i.Identity.reply_to = None);
    Alcotest.(check bool) "bcc is None" true (i.Identity.bcc = None);
    Alcotest.(check bool)
      "textSignature is None" true
      (i.Identity.text_signature = None);
    Alcotest.(check bool)
      "mayDelete is false" true
      (i.Identity.may_delete = Some false)

  (* Identity/set create objects are built from the same type: only the
     properties given are sent (RFC 8621 Section 6.3). *)
  let test_create_object_encoding () =
    let i = Identity.v ~name:"Joe B" ~email:"joe@example.com" () in
    match encode Identity.jsont i with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        check_omitted "identity create" "id" json;
        check_omitted "identity create" "replyTo" json;
        check_omitted "identity create" "mayDelete" json;
        Alcotest.(check bool)
          "name is sent" true
          (contains ~needle:"\"name\":\"Joe B\"" json)

  (* RFC 8621 Section 6 lets [email] stand for any local part at a domain, and
     RFC 8620 Section 2 lets the session username be a full address. *)
  let test_sending_address () =
    let addr local_part email =
      Identity.sending_address ~local_part (Identity.v ~email ())
    in
    Alcotest.(check (option string))
      "wildcard domain" (Some "joe@example.com")
      (addr "joe" "*@example.com");
    Alcotest.(check (option string))
      "a plain address" (Some "jb@example.com")
      (addr "joe" "jb@example.com");
    Alcotest.(check (option string)) "wildcard alone" None (addr "joe" "*");
    Alcotest.(check (option string)) "no domain" None (addr "joe" "*@");
    Alcotest.(check (option string)) "empty address" None (addr "joe" "");
    Alcotest.(check (option string))
      "a local part that is an address" None
      (addr "joe@example.com" "*@example.org");
    Alcotest.(check (option string)) "empty local part" None (addr "" "*@x.org");
    Alcotest.(check (option string))
      "no address" None
      (Identity.sending_address ~local_part:"joe" (Identity.v ()))

  let tests =
    [
      ("sending address", `Quick, test_sending_address);
      ("null replyTo and bcc decode", `Quick, test_null_reply_to_and_bcc);
      ( "absent optional properties decode",
        `Quick,
        test_absent_optional_properties );
      ( "create object omits unset properties",
        `Quick,
        test_create_object_encoding );
      ("roundtrip", `Quick, test_roundtrip "identity" Identity.jsont path);
      ( "roundtrip Cyrus shape",
        `Quick,
        test_roundtrip "identity" Identity.jsont cyrus );
    ]
end

(* EmailSubmission (RFC 8621 Section 7) *)
module Submission_tests = struct
  let envelope = "submission/rfc8621_7_5_1_envelope.json"
  let address = "submission/valueless_parameter.json"
  let null_envelope = "submission/null_envelope.json"
  let status = "submission/delivery_status.json"

  (* The Section 7.5.1 example sends "parameters": null on every address. *)
  let test_rfc_example_envelope () =
    let s = decode_file "submission" Submission.jsont envelope in
    Alcotest.(check bool)
      "deliveryStatus is None" true
      (s.Submission.delivery_status = None);
    let env = some "envelope" s.Submission.envelope in
    let from = env.Submission.Envelope.mail_from in
    Alcotest.(check string)
      "mailFrom" "john@example.com" from.Submission.Address.email;
    Alcotest.(check bool)
      "mailFrom parameters is None" true
      (from.Submission.Address.parameters = None);
    Alcotest.(check int)
      "one rcptTo" 1
      (List.length env.Submission.Envelope.rcpt_to)

  (* A valueless ESMTP parameter is sent as a null value. *)
  let test_valueless_parameter () =
    let a = decode_file "submission" Submission.Address.jsont address in
    let params = some "parameters" a.Submission.Address.parameters in
    Alcotest.(check bool)
      "SMTPUTF8 has no value" true
      (List.assoc "SMTPUTF8" params = None);
    Alcotest.(check string)
      "BODY value" "8BITMIME"
      (some "BODY" (List.assoc "BODY" params))

  let test_null_envelope () =
    let s = decode_file "submission" Submission.jsont null_envelope in
    Alcotest.(check bool) "envelope is None" true (s.Submission.envelope = None)

  let test_delivery_status () =
    let d = decode_file "submission" Submission.Delivery_status.jsont status in
    Alcotest.(check bool)
      "delivered is queued" true
      (d.Submission.Delivery_status.delivered = `Queued);
    Alcotest.(check bool)
      "displayed is unknown" true
      (d.Submission.Delivery_status.displayed = `Unknown)

  (* The object a server returns in the "created" map of an
     EmailSubmission/set response holds only the server-set properties it
     assigned; Cyrus sends id, undoStatus and sendAt. *)
  let created = "submission/cyrus_created.json"

  let test_created_object () =
    let s = decode_file "submission" Submission.jsont created in
    Alcotest.(check bool)
      "undoStatus is final" true
      (s.Submission.undo_status = Some `Final);
    Alcotest.(check bool) "envelope is None" true (s.Submission.envelope = None);
    Alcotest.(check bool) "emailId is None" true (s.Submission.email_id = None);
    Alcotest.(check string)
      "sendAt" "2026-09-02T08:12:14Z"
      (match s.Submission.send_at with
      | Some t -> Ptime.to_rfc3339 ~tz_offset_s:0 t
      | None -> Alcotest.fail "sendAt missing")

  (* A whole /set response with an explicit null for every unused map. *)
  let test_set_response () =
    let r =
      decode_file "submission"
        (Method.set_response_jsont Submission.jsont)
        "submission/cyrus_set_response.json"
    in
    Alcotest.(check bool) "updated is None" true (r.Method.updated = None);
    match r.Method.created with
    | Some [ (cid, sub) ] ->
        Alcotest.(check string) "creation id" "s1" (Id.to_string cid);
        Alcotest.(check bool)
          "undoStatus" true
          (sub.Submission.undo_status = Some `Final)
    | _ -> Alcotest.fail "expected one created submission"

  (* RFC 8621 Section 7.5 says an unknown emailId is an invalidProperties
     SetError; Cyrus answers "emailNotFound", which must survive as
     [`Other]. *)
  let test_not_created_error () =
    let r =
      decode_file "submission"
        (Method.set_response_jsont Submission.jsont)
        "submission/cyrus_not_created.json"
    in
    match r.Method.not_created with
    | Some [ (_, e) ] ->
        Alcotest.(check string)
          "error type round trips" "emailNotFound"
          (Error.Set_error.type_to_string e.Error.Set_error.type_)
    | _ -> Alcotest.fail "expected one notCreated entry"

  (* A create object carries only the client-settable properties. *)
  let test_create_object_encoding () =
    let sub =
      Submission.create
        ~identity_id:(Id.of_string_exn "I64588216")
        ~email_id:(Id.of_creation_id_exn "draft1")
        ~envelope:
          (Submission.Envelope.v
             ~mail_from:(Submission.Address.v "john@example.com")
             ~rcpt_to:[ Submission.Address.v "jane@example.com" ])
        ()
    in
    match encode Submission.jsont sub with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        check_omitted "submission create" "id" json;
        check_omitted "submission create" "undoStatus" json;
        check_omitted "submission create" "deliveryStatus" json;
        (* An absent "parameters" and an explicit null mean the same thing. *)
        check_omitted "submission create" "parameters" json;
        Alcotest.(check bool)
          "emailId is the creation reference" true
          (contains ~needle:"\"emailId\":\"#draft1\"" json)

  let test_undo_status_strings () =
    List.iter
      (fun (s, v) ->
        Alcotest.(check string)
          (s ^ " round trips") s
          (Submission.undo_status_to_string v);
        Alcotest.(check bool)
          (s ^ " parses") true
          (Submission.undo_status_of_string s = Some v))
      [ ("pending", `Pending); ("final", `Final); ("canceled", `Canceled) ];
    Alcotest.(check bool)
      "unknown undoStatus is rejected" true
      (Submission.undo_status_of_string "sent" = None);
    Alcotest.(check bool)
      "unknown delivered is rejected" true
      (Submission.Delivery_status.delivered_of_string "bounced" = None);
    Alcotest.(check bool)
      "the strict constructor raises" true
      (match Submission.Delivery_status.delivered_of_string_exn "bounced" with
      | exception Invalid_argument _ -> true
      | _ -> false);
    Alcotest.(check bool)
      "the strict displayed constructor raises" true
      (match Submission.Delivery_status.displayed_of_string_exn "maybe" with
      | exception Invalid_argument _ -> true
      | _ -> false);
    Alcotest.(check string)
      "displayed" "unknown"
      (Submission.Delivery_status.displayed_to_string `Unknown)

  (* RFC 8621 Section 7.3: "If zero properties are specified, it is
     automatically true for all objects." *)
  let test_empty_filter () =
    match
      encode Submission.Filter_condition.jsont Submission.Filter_condition.empty
    with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json -> Alcotest.(check string) "empty filter" "{}" json

  let test_filter_encoding () =
    let f =
      {
        Submission.Filter_condition.empty with
        email_ids = Some [ Id.of_string_exn "M1" ];
        undo_status = Some `Final;
      }
    in
    match encode Submission.Filter_condition.jsont f with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        check_omitted "submission filter" "identityIds" json;
        Alcotest.(check bool)
          "undoStatus is the wire spelling" true
          (contains ~needle:"\"undoStatus\":\"final\"" json)

  (* Preserve a nonconformant or future spelling without conflating it with
     the RFC's semantic "unknown" value or losing the whole /get response. *)
  let test_unknown_delivered_is_preserved () =
    let d =
      decode_file "submission" Submission.Delivery_status.jsont
        "submission/bad_delivered.json"
    in
    Alcotest.(check bool)
      "delivered is Other" true
      (d.Submission.Delivery_status.delivered = `Other "bounced");
    Alcotest.(check string)
      "smtpReply survives" "250 2.0.0 Message accepted"
      d.Submission.Delivery_status.smtp_reply;
    match encode Submission.Delivery_status.jsont d with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        Alcotest.(check bool)
          "spelling round trips" true
          (contains ~needle:{|"delivered":"bounced"|} json)

  let test_unknown_displayed_is_preserved () =
    let d =
      decode_file "submission" Submission.Delivery_status.jsont
        "submission/bad_displayed.json"
    in
    Alcotest.(check bool)
      "displayed is Other" true
      (d.Submission.Delivery_status.displayed = `Other "maybe");
    Alcotest.(check bool)
      "delivered still decodes" true
      (d.Submission.Delivery_status.delivered = `Yes)

  (* One nonconformant status must not cost the client the rest of the
     response. *)
  let test_unknown_status_does_not_fail_the_get () =
    let json =
      {|{"accountId":"a1","state":"s","list":[{"id":"S1","undoStatus":"final","deliveryStatus":{"jane@example.com":{"smtpReply":"250 ok","delivered":"bounced","displayed":"unknown"}}}],"notFound":[]}|}
    in
    match decode (Method.get_response_jsont Submission.jsont) json with
    | Error e -> Alcotest.failf "decode: %s" (Jsont.Error.to_string e)
    | Ok r -> (
        match r.Method.list with
        | [ sub ] -> (
            match sub.Submission.delivery_status with
            | Some [ (rcpt, d) ] ->
                Alcotest.(check string) "recipient" "jane@example.com" rcpt;
                Alcotest.(check bool)
                  "delivered is preserved" true
                  (d.Submission.Delivery_status.delivered = `Other "bounced")
            | _ -> Alcotest.fail "expected one deliveryStatus entry")
        | _ -> Alcotest.fail "expected one submission")

  let tests =
    [
      ("RFC 8621 7.5.1 envelope decodes", `Quick, test_rfc_example_envelope);
      ("valueless SMTP parameter decodes", `Quick, test_valueless_parameter);
      ("null envelope decodes", `Quick, test_null_envelope);
      ("delivery status decodes", `Quick, test_delivery_status);
      ( "unknown delivered value is preserved",
        `Quick,
        test_unknown_delivered_is_preserved );
      ( "unknown displayed value is preserved",
        `Quick,
        test_unknown_displayed_is_preserved );
      ( "an unknown status does not fail the /get",
        `Quick,
        test_unknown_status_does_not_fail_the_get );
      ( "roundtrip envelope",
        `Quick,
        test_roundtrip "submission" Submission.jsont envelope );
      ( "roundtrip address",
        `Quick,
        test_roundtrip "address" Submission.Address.jsont address );
      ("created object decodes", `Quick, test_created_object);
      ("/set response with explicit nulls decodes", `Quick, test_set_response);
      ("unknown SetError type survives", `Quick, test_not_created_error);
      ( "create object omits server-set properties",
        `Quick,
        test_create_object_encoding );
      ("undoStatus and DeliveryStatus strings", `Quick, test_undo_status_strings);
      ("empty filter condition encodes to {}", `Quick, test_empty_filter);
      ("filter condition encoding", `Quick, test_filter_encoding);
      ( "roundtrip created object",
        `Quick,
        test_roundtrip "submission" Submission.jsont created );
    ]
end

(* VacationResponse (RFC 8621 Section 8) *)
module Vacation_tests = struct
  let path = "vacation/null_dates.json"
  let restricted = "vacation/properties_restricted.json"

  let test_null_dates () =
    let v = decode_file "vacation" Vacation.jsont path in
    Alcotest.(check bool) "fromDate is None" true (v.Vacation.from_date = None);
    Alcotest.(check bool) "toDate is None" true (v.Vacation.to_date = None);
    Alcotest.(check bool)
      "isEnabled is true" true
      (v.Vacation.is_enabled = Some true)

  (* Section 8.1: VacationResponse/get is a standard /get, so a client may
     restrict "properties" and the response then omits isEnabled. *)
  let test_properties_restricted () =
    let v = decode_file "vacation" Vacation.jsont restricted in
    Alcotest.(check bool) "isEnabled is None" true (v.Vacation.is_enabled = None);
    Alcotest.(check string)
      "id" "singleton"
      (Id.to_string (some "id" v.Vacation.id))

  let tests =
    [
      ("null fromDate and toDate decode", `Quick, test_null_dates);
      ("properties-restricted get decodes", `Quick, test_properties_restricted);
      ("roundtrip", `Quick, test_roundtrip "vacation" Vacation.jsont path);
    ]
end

(* Capability objects (RFC 8620 Section 2, RFC 8621 Sections 1.3.1/1.3.2) *)
module Capability_tests = struct
  let mail_nulls = "capability/mail_nulls.json"
  let mail_session = "capability/mail_session.json"
  let submission_session = "capability/submission_session.json"
  let core = "capability/core_unsigned.json"

  let test_mail_null_limits () =
    let c =
      decode_file "mail capability" Capability.Mail.account_jsont mail_nulls
    in
    Alcotest.(check bool)
      "maxMailboxesPerEmail is None" true
      (c.Capability.Mail.max_mailboxes_per_email = None);
    Alcotest.(check bool)
      "maxMailboxDepth is None" true
      (c.Capability.Mail.max_mailbox_depth = None);
    Alcotest.(check int64)
      "maxSizeMailboxName" 490L
      (some "maxSizeMailboxName" c.Capability.Mail.max_size_mailbox_name)

  (* The session-scoped value of both capabilities is the empty object, so
     session_capability_of_json must not fall through to Unknown for it. *)
  let test_mail_session_scope () =
    let json = read_file mail_session in
    let v =
      match decode Jsont.json json with
      | Ok j -> j
      | Error e -> Alcotest.failf "json: %s" (Jsont.Error.to_string e)
    in
    match Capability.session_capability_of_json Capability.mail v with
    | Ok (Capability.Mail m) ->
        Alcotest.(check bool)
          "no members set" true
          (m.Capability.Mail.max_size_mailbox_name = None
          && m.Capability.Mail.may_create_top_level_mailbox = None)
    | _ -> Alcotest.fail "session-level mail capability decoded as Unknown"

  let test_submission_session_scope () =
    let json = read_file submission_session in
    let v =
      match decode Jsont.json json with
      | Ok j -> j
      | Error e -> Alcotest.failf "json: %s" (Jsont.Error.to_string e)
    in
    match Capability.session_capability_of_json Capability.submission v with
    | Ok (Capability.Submission s) ->
        Alcotest.(check bool)
          "no members set" true
          (s.Capability.Submission.max_delayed_send = None
          && s.Capability.Submission.submission_extensions = None)
    | _ ->
        Alcotest.fail "session-level submission capability decoded as Unknown"

  let test_core_unsigned () =
    let c = decode_file "core capability" Capability.Core.jsont core in
    Alcotest.(check int64)
      "maxCallsInRequest" 32L c.Capability.Core.max_calls_in_request;
    Alcotest.(check int64)
      "maxObjectsInGet" 256L c.Capability.Core.max_objects_in_get

  let tests =
    [
      ("mail null limits decode", `Quick, test_mail_null_limits);
      ("session-level mail capability", `Quick, test_mail_session_scope);
      ( "session-level submission capability",
        `Quick,
        test_submission_session_scope );
      ("core UnsignedInt limits decode", `Quick, test_core_unsigned);
      ( "core limit as string is rejected",
        `Quick,
        test_decode_failure "core" Capability.Core.jsont
          "capability/core_string_limit.json" );
      ( "negative core limit is rejected",
        `Quick,
        test_decode_failure "core" Capability.Core.jsont
          "capability/core_negative_limit.json" );
      ( "roundtrip mail with nulls",
        `Quick,
        test_roundtrip "mail capability" Capability.Mail.account_jsont
          mail_nulls );
      ( "roundtrip empty mail",
        `Quick,
        test_roundtrip "mail session capability" Capability.Mail.session_jsont
          mail_session );
      ( "roundtrip core",
        `Quick,
        test_roundtrip "core capability" Capability.Core.jsont core );
    ]
end

(* SearchSnippet/get response (RFC 8621 Section 5.1) *)
module Snippet_tests = struct
  let found = "snippet/get_response.json"
  let not_found = "snippet/get_response_not_found.json"

  (* The response has no "state" member and notFound is Id[]|null, so the
     generic /get response codec cannot decode it. *)
  let test_null_not_found () =
    let r = decode_file "snippet" Search_snippet.get_response_jsont found in
    Alcotest.(check string)
      "accountId" "ue411d190"
      (Id.to_string r.Search_snippet.account_id);
    Alcotest.(check bool)
      "notFound is None" true
      (r.Search_snippet.not_found = None);
    match r.Search_snippet.list with
    | [ s ] ->
        Alcotest.(check bool)
          "preview is None" true
          (s.Search_snippet.preview = None);
        Alcotest.(check string)
          "subject" "I <mark>lost</mark> my keys"
          (some "subject" s.Search_snippet.subject)
    | l -> Alcotest.failf "expected one snippet, got %d" (List.length l)

  let test_not_found () =
    let r = decode_file "snippet" Search_snippet.get_response_jsont not_found in
    Alcotest.(check int)
      "one notFound id" 1
      (List.length (some "notFound" r.Search_snippet.not_found))

  (* A conformant SearchSnippet/get response is not a generic /get response. *)
  let test_not_a_generic_get_response () =
    test_decode_failure "snippet"
      (Method.get_response_jsont Search_snippet.jsont)
      found ()

  let tests =
    [
      ("response with null notFound decodes", `Quick, test_null_not_found);
      ("response with notFound ids decodes", `Quick, test_not_found);
      ( "generic /get response codec rejects it",
        `Quick,
        test_not_a_generic_get_response );
      ( "roundtrip",
        `Quick,
        test_roundtrip "snippet" Search_snippet.get_response_jsont found );
    ]
end

(* Keywords (RFC 8621 Section 4.1.1) *)
module Keyword_tests = struct
  let ok s =
    match Keyword.validate (`Custom s) with
    | Ok k ->
        Alcotest.(check string)
          (Printf.sprintf "%S accepted" s)
          s (Keyword.to_string k)
    | Error msg -> Alcotest.failf "%S rejected: %s" s msg

  let bad s =
    match Keyword.validate (`Custom s) with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%S should have been rejected" s

  (* "a keyword is a case-insensitive string of 1-255 characters in the ASCII
     subset %x21-%x7e (excludes control chars and space)". *)
  let test_valid () =
    ok "$seen";
    ok "$draft";
    ok "important";
    ok "$MailFlagBit0";
    ok "a";
    ok (String.make 255 'k');
    (* %x21 and %x7e are both in range. *)
    ok "!";
    ok "~";
    (* A registered keyword is well formed by construction. *)
    Alcotest.(check bool) "$seen" true (Keyword.validate `Seen = Ok `Seen)

  let test_invalid () =
    bad "";
    bad (String.make 256 'k');
    bad "has space";
    bad "tab\there";
    bad "\xc3\xa9t\xc3\xa9";
    (* "and it MUST NOT include any of these characters", the eight below *)
    List.iter
      (fun c -> bad (Printf.sprintf "a%cb" c))
      [ '('; ')'; '{'; ']'; '%'; '*'; '"'; '\\' ]

  (* The check applies where a client builds a keyword, never on decode. *)
  let test_builders_reject () =
    let raises name f =
      match f () with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    in
    raises "of_list" (fun () -> Keyword.of_list [ `Custom "bad keyword" ]);
    raises "of_list duplicate" (fun () ->
        Keyword.of_list [ `Custom "Label"; `Custom "label" ]);
    raises "Import.email" (fun () ->
        Email.Import.email ~blob_id:(Id.of_string_exn "G1")
          ~mailbox_ids:[ Id.of_string_exn "M1" ]
          ~keywords:[ `Custom "bad\"keyword" ]
          ());
    raises "Import.email duplicate mailbox" (fun () ->
        Email.Import.email ~blob_id:(Id.of_string_exn "G1")
          ~mailbox_ids:[ Id.of_string_exn "M1"; Id.of_string_exn "M1" ]
          ());
    raises "Email.Patch.set_keyword" (fun () ->
        Email.Patch.set_keyword (`Custom "bad keyword"));
    raises "Email.Patch.remove_keyword" (fun () ->
        Email.Patch.remove_keyword (`Custom "bad*keyword"));
    raises "Email.Patch.set_keywords" (fun () ->
        Email.Patch.set_keywords [ `Custom "bad keyword" ]);
    (* A well-formed keyword still goes through. *)
    Alcotest.(check bool)
      "well-formed keywords" true
      (Keyword.of_list [ `Seen ] = [ (`Seen, true) ]);
    match decode Email.jsont {|{"keywords":{"Label":true,"label":true}}|} with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "case-insensitive duplicate keywords accepted"

  (* A server that sends a malformed keyword must not lose the client the
     Email: decoding is not validated. *)
  let test_decode_is_not_validated () =
    match
      decode Email.jsont {|{"id":"M1","keywords":{"bad keyword":true}}|}
    with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok e ->
        Alcotest.(check bool)
          "keyword kept verbatim" true
          (e.Email.keywords = Some [ (`Custom "bad keyword", true) ]);
        let printed =
          Format.asprintf "%a" Keyword.pp (`Custom "bad\n\027[2J")
        in
        Alcotest.(check bool)
          "keyword printer has no newline" false
          (String.contains printed '\n');
        Alcotest.(check bool)
          "keyword printer has no escape byte" false
          (String.contains printed '\027')

  (* An unregistered keyword keeps its spelling and only the JMAP [$] form of
     a registered keyword is recognised. *)
  let test_of_string () =
    let same s = Keyword.to_string (Keyword.of_string s) in
    Alcotest.(check string) "$important" "$important" (same "$important");
    Alcotest.(check bool)
      "$important is a custom keyword" true
      (Keyword.equal (Keyword.of_string "$important") (`Custom "$important"));
    Alcotest.(check string) "my-label" "my-label" (same "my-label");
    Alcotest.(check string) "seen" "seen" (same "seen");
    Alcotest.(check string) "$SEEN" "$seen" (same "$SEEN");
    Alcotest.(check string) "\\Seen" "\\Seen" (same "\\Seen");
    Alcotest.(check string) "\\Deleted" "\\Deleted" (same "\\Deleted");
    (* [$deleted] has no JMAP form, so it is a custom keyword holding the
       caller's spelling. *)
    Alcotest.(check string) "$DELETED" "$DELETED" (same "$DELETED");
    Alcotest.(check bool)
      "$DELETED is a custom keyword" true
      (Keyword.of_string "$DELETED" = `Custom "$DELETED");
    Alcotest.(check bool)
      "\\Deleted has no JMAP form" true
      (Keyword.of_mail_flag `Deleted = None);
    Alcotest.(check int)
      "dropped from a list" 1
      (List.length (Keyword.of_mail_flag_list [ `Deleted; `Seen ]));
    Alcotest.(check bool)
      "$seen survives" true
      (Keyword.of_mail_flag `Seen = Some `Seen)

  (* Section 4.1.1 makes a keyword case insensitive. *)
  let test_equal () =
    Alcotest.(check bool)
      "custom keywords fold case" true
      (Keyword.equal (`Custom "Label") (`Custom "label"));
    Alcotest.(check bool)
      "different keywords" false
      (Keyword.equal (`Custom "label") `Seen);
    Alcotest.(check bool)
      "compare is a total order" true
      (Keyword.compare `Seen `Seen = 0
      && Keyword.compare `Draft `Seen < 0
      && Keyword.compare `Seen `Draft > 0)

  (* draft-ietf-mailmaint-messageflag-mailboxattribute Section 3 reads the
     three $MailFlagBit keywords as a 3-bit number, bit 0 first. *)
  let test_flag_colors () =
    let colors =
      [
        (`Orange, [ `MailFlagBit0 ]);
        (`Yellow, [ `MailFlagBit1 ]);
        (`Green, [ `MailFlagBit0; `MailFlagBit1 ]);
        (`Blue, [ `MailFlagBit2 ]);
        (`Purple, [ `MailFlagBit0; `MailFlagBit2 ]);
        (`Gray, [ `MailFlagBit1; `MailFlagBit2 ]);
      ]
    in
    List.iter
      (fun (color, bits) ->
        Alcotest.(check bool)
          "bits of a colour" true
          (Keyword.flag_color_to_keywords color = (bits :> Keyword.t list));
        Alcotest.(check bool)
          "colour of the bits" true
          (Keyword.flag_color_of_keywords
             ((bits :> Keyword.t list) @ [ `Flagged ])
          = Some color))
      colors;
    Alcotest.(check bool)
      "red sets no bit" true
      (Keyword.flag_color_to_keywords `Red = []);
    (* No bit set is indistinguishable from an unflagged message, and all
       three set is undefined. *)
    Alcotest.(check bool)
      "no bit is no colour" true
      (Keyword.flag_color_of_keywords [ `Flagged ] = None);
    Alcotest.(check bool)
      "111 is undefined" true
      (Keyword.flag_color_of_keywords
         [ `MailFlagBit0; `MailFlagBit1; `MailFlagBit2 ]
      = None)

  (* A key repeated verbatim is a duplicate too, which the underlying map
     codec rejects before the case-insensitive check is reached. *)
  let test_exact_duplicate_keys_are_rejected () =
    match decode Email.jsont {|{"keywords":{"$seen":true,"$seen":true}}|} with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "duplicate keyword keys accepted"

  (* An encoder must report a malformed keyword the server sent as a
     Jsont.Error, which encode_string catches, rather than as an escaping
     Invalid_argument. *)
  let test_encoders_raise_jsont_errors () =
    let e =
      Email.v
        ~keywords:[ (`Custom "bad keyword", true) ]
        ~id:(Id.of_string_exn "M1") ()
    in
    (match encode Email.jsont e with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "a malformed keyword encoded");
    let dup =
      Email.v ~keywords:[ (`Custom "Label", true); (`Custom "label", true) ] ()
    in
    match encode Email.jsont dup with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "duplicate keywords encoded"

  let tests =
    [
      ("valid keywords", `Quick, test_valid);
      ("invalid keywords", `Quick, test_invalid);
      ( "exact duplicate keys are rejected",
        `Quick,
        test_exact_duplicate_keys_are_rejected );
      ("encoders raise Jsont errors", `Quick, test_encoders_raise_jsont_errors);
      ("client-side builders reject", `Quick, test_builders_reject);
      ("decoding is not validated", `Quick, test_decode_is_not_validated);
      ("of_string keeps custom spellings", `Quick, test_of_string);
      ("equality folds case", `Quick, test_equal);
      ("Apple Mail flag colours", `Quick, test_flag_colors);
    ]
end

(* Mailbox creation constraints (RFC 8621 Section 2) *)
module Mailbox_create_tests = struct
  let ok name f =
    match f () with
    | Ok v -> v
    | Error msg -> Alcotest.failf "%s: rejected: %s" name msg

  let bad name f =
    match f () with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s: expected a rejection" name

  (* "name ... MUST be a Net-Unicode string of at least 1 character in
     length" and "sortOrder ... MUST be an integer in the range
     0 <= sortOrder < 2^31". *)
  let test_create () =
    let m =
      ok "inbox child" (fun () ->
          Mailbox.create ~name:"Receipts" ~role:`Archive ~sort_order:10L
            ~parent_id:(Id.of_string_exn "Mbox1") ())
    in
    Alcotest.(check (option string)) "name" (Some "Receipts") m.Mailbox.name;
    Alcotest.(check bool) "sortOrder" true (m.Mailbox.sort_order = Some 10L);
    (* Server-set properties are left unset, so the create object omits
       them (RFC 8621 Section 2.5). *)
    Alcotest.(check bool) "id unset" true (m.Mailbox.id = None);
    Alcotest.(check bool)
      "totalEmails unset" true
      (m.Mailbox.total_emails = None);
    (match encode Mailbox.jsont m with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        check_omitted "mailbox create" "id" json;
        check_omitted "mailbox create" "myRights" json);
    bad "empty name" (fun () -> Mailbox.create ~name:"" ());
    Alcotest.check_raises "create_exn raises on an empty name"
      (Invalid_argument
         ("Mail_mailbox.create_exn: "
         ^ Result.get_error (Mailbox.create ~name:"" ())))
      (fun () -> ignore (Mailbox.create_exn ~name:"" ()));
    bad "negative sortOrder" (fun () ->
        Mailbox.create ~name:"Receipts" ~sort_order:(-1L) ());
    bad "sortOrder = 2^31" (fun () ->
        Mailbox.create ~name:"Receipts" ~sort_order:2147483648L ());
    ignore
      (ok "sortOrder = 2^31 - 1" (fun () ->
           Mailbox.create ~name:"Receipts" ~sort_order:2147483647L ()));
    ignore
      (ok "sortOrder = 0" (fun () ->
           Mailbox.create ~name:"Receipts" ~sort_order:0L ()))

  (* Section 2 gives parentId as "Id|null", and a client may name a Mailbox
     created earlier in the same /set (RFC 8620 Section 5.3). *)
  let test_create_under_a_creation_reference () =
    let m =
      ok "child of a new mailbox" (fun () ->
          Mailbox.create ~name:"Receipts"
            ~parent_id:(Id.of_creation_id_exn "top")
            ())
    in
    match encode Mailbox.jsont m with
    | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    | Ok json ->
        Alcotest.(check bool)
          "parentId is the creation reference" true
          (contains ~needle:{|"parentId":"#top"|} json)

  (* "name ... MUST be a Net-Unicode string", so a byte sequence that is not
     UTF-8 is rejected rather than sent on to be rejected by the server. *)
  let test_create_rejects_a_malformed_name () =
    bad "invalid UTF-8" (fun () -> Mailbox.create ~name:"\xff\xfe" ());
    bad "a noncharacter" (fun () -> Mailbox.create ~name:"a\xef\xbf\xbf" ());
    ignore (ok "UTF-8" (fun () -> Mailbox.create ~name:"Reçus" ()))

  let tests =
    [
      ("create validates name and sortOrder", `Quick, test_create);
      ( "create rejects a malformed name",
        `Quick,
        test_create_rejects_a_malformed_name );
      ( "create under a creation reference",
        `Quick,
        test_create_under_a_creation_reference );
    ]
end

(* Header property parsing (RFC 8621 Sections 4.1.2 and 4.2) *)
module Header_tests = struct
  let parses s =
    Alcotest.(check bool)
      (Printf.sprintf "%S parses" s)
      true
      (Email_header.header_property_of_string s <> None)

  let rejects s =
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected" s)
      true
      (Email_header.header_property_of_string s = None)

  let test_valid_combinations () =
    parses "header:From:asAddresses";
    parses "header:From:asGroupedAddresses:all";
    parses "header:Message-ID:asMessageIds";
    parses "header:Date:asDate";
    parses "header:List-Post:asURLs";
    parses "header:Subject:asText";
    parses "header:From";
    parses "header:From:all";
    (* A header field not defined in RFC 5322 or RFC 2369 allows any form *)
    parses "header:X-Spam-Score:asDate";
    parses "header:Content-Type"

  let test_invalid_combinations () =
    rejects "header:From:asDate";
    rejects "header:Date:asAddresses";
    rejects "header:Subject:asMessageIds";
    rejects "header:List-Post:asText";
    rejects "header:Message-ID:asURLs";
    rejects "header:From:asBogus"

  (* Section 4.1.4: a body part may carry header:* properties too. *)
  let test_body_part_header_property () =
    match Email.body_part_property_of_string "header:Content-Type" with
    | Some p ->
        Alcotest.(check string)
          "roundtrips to wire name" "header:Content-Type"
          (Email.body_part_property_to_string p)
    | None -> Alcotest.fail "header:Content-Type not accepted"

  let test_body_part_standard_property () =
    match Email.body_part_property_of_string "subParts" with
    | Some p ->
        Alcotest.(check string)
          "wire name" "subParts"
          (Email.body_part_property_to_string p)
    | None -> Alcotest.fail "subParts not accepted"

  (* List-Id is defined in RFC 2919, not in RFC 5322 or RFC 2369, so RFC 8621
     Section 4.1.2 leaves every form open to it. *)
  let test_list_id_is_custom () =
    List.iter
      (fun form -> parses (Printf.sprintf "header:List-Id:%s" form))
      [ "asText"; "asAddresses"; "asMessageIds"; "asDate"; "asURLs" ];
    Alcotest.(check bool)
      "List-Id is not a standard header" true
      (Email_header.standard_header_of_string "List-Id" = None)

  (* The RFC 5322 Section 3.6.7 trace fields are defined in RFC 5322 and have
     no parsed form, so only the raw form may be asked for. *)
  let test_trace_headers_are_raw_only () =
    List.iter
      (fun name ->
        parses (Printf.sprintf "header:%s" name);
        parses (Printf.sprintf "header:%s:all" name);
        List.iter
          (fun form -> rejects (Printf.sprintf "header:%s:%s" name form))
          [ "asText"; "asAddresses"; "asMessageIds"; "asDate"; "asURLs" ])
      [ "Received"; "Return-Path" ]

  let test_empty_names_are_rejected () =
    rejects "header:";
    rejects "header::all";
    rejects "header:X::all";
    rejects "From"

  (* Section 4.1.3: "{header-field-name}" is "any series of one or more
     printable ASCII characters (i.e., characters that have values between 33
     and 126, inclusive), except for colon (:)". *)
  let test_malformed_names_are_rejected () =
    rejects "header: ";
    rejects "header:X Y";
    rejects "header:X\ty";
    rejects "header:\xc3\xa9t\xc3\xa9";
    rejects "header:X\x7f"

  let test_constructors_check_the_name () =
    let raises name f =
      match f () with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    in
    let bad = [ ""; " "; "X Y"; "X\ty"; "X:Y"; "\xc3\xa9t\xc3\xa9"; "X\x7f" ] in
    List.iter
      (fun n ->
        raises "raw" (fun () -> Email_header.raw n);
        raises "text" (fun () -> Email_header.text (`Custom n));
        raises "addresses" (fun () -> Email_header.addresses (`Custom n));
        raises "grouped_addresses" (fun () ->
            Email_header.grouped_addresses (`Custom n));
        raises "message_ids" (fun () -> Email_header.message_ids (`Custom n));
        raises "date" (fun () -> Email_header.date (`Custom n));
        raises "urls" (fun () -> Email_header.urls (`Custom n)))
      bad;
    (* A well formed name still goes through, and a standard field never
       carries one. *)
    Alcotest.(check string)
      "custom name" "header:X-Spam-Score"
      (Email_header.header_property_to_string (Email_header.raw "X-Spam-Score"));
    Alcotest.(check string)
      "standard field" "header:From:asAddresses"
      (Email_header.header_property_to_string (Email_header.addresses `From))

  let test_constructors_check_custom_standard_names () =
    let raises name f =
      match f () with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "%s: expected Invalid_argument" name
    in
    raises "mixed-case From as date" (fun () ->
        Email_header.date (`Custom "fRoM"));
    raises "direct invalid form" (fun () ->
        Email_header.header_property_to_string
          (Email_header.Date { header = `Custom "From"; all = false }));
    raises "direct empty raw name" (fun () ->
        Email_header.header_property_to_string
          (Email_header.Raw { name = ""; all = false }));
    Alcotest.(check string)
      "custom date header" "header:X-Delivery-Date:asDate"
      (Email_header.header_property_to_string
         (Email_header.date (`Custom "X-Delivery-Date")))

  (* Every property a constructor builds parses back, so the two agree on
     which names are header field names. *)
  let test_constructors_roundtrip () =
    List.iter
      (fun p ->
        let s = Email_header.header_property_to_string p in
        Alcotest.(check bool)
          (Printf.sprintf "%S roundtrips" s)
          true
          (Email_header.header_property_of_string s = Some p))
      [
        Email_header.raw "X-Spam-Score";
        Email_header.raw ~all:true "Received";
        Email_header.text `Subject;
        Email_header.date `Date;
        Email_header.urls ~all:true `List_post;
        Email_header.message_ids `In_reply_to;
        Email_header.addresses (`Custom "X-Original-From");
      ]

  (* A standard field is held as its own constructor rather than as a custom
     one. *)
  let test_standard_header_is_typed () =
    Alcotest.(check bool)
      "header:From:asAddresses" true
      (Email_header.header_property_of_string "header:From:asAddresses"
      = Some (Email_header.Addresses { header = `From; all = false }))

  let test_value_printer_escapes_control_bytes () =
    let printed =
      Email_header.value_to_string
        (Email_header.String_single (Some "subject\n\027[2J"))
    in
    Alcotest.(check bool) "no newline" false (String.contains printed '\n');
    Alcotest.(check bool)
      "no escape byte" false
      (String.contains printed '\027')

  (* RFC 8621 Section 4.1.3 makes the empty array of an ":all" form the case
     of a field the message does not carry. *)
  let test_empty_all_prints_as_absent () =
    List.iter
      (fun v ->
        Alcotest.(check string)
          "(absent)" "(absent)"
          (Email_header.value_to_string v))
      [
        Email_header.String_all [];
        Email_header.Addresses_all [];
        Email_header.Grouped_all [];
        Email_header.Date_all [];
        Email_header.Strings_all [];
      ]

  (* An encoder names the form it was built for rather than a fixed one. *)
  let test_encode_mismatch_names_the_form () =
    let mismatch ~form ~all =
      match
        encode
          (Email_header.header_value_jsont ~form ~all)
          (Email_header.Date_single None)
      with
      | Ok _ -> Alcotest.failf "expected an encode failure"
      | Error e -> Jsont.Error.to_string e
    in
    Alcotest.(check bool)
      "raw" true
      (contains ~needle:"the raw form" (mismatch ~form:`Raw ~all:false));
    Alcotest.(check bool)
      "asText:all" true
      (contains ~needle:"the asText:all form" (mismatch ~form:`Text ~all:true));
    Alcotest.(check bool)
      "asURLs" true
      (contains ~needle:"the asURLs form" (mismatch ~form:`Urls ~all:false))

  (* The sub-second fraction a date was decoded with survives the printer. *)
  let test_value_printer_keeps_the_fraction () =
    let t =
      match Date.of_string "2019-04-01T12:34:56.123Z" with
      | Ok t -> t
      | Error e -> Alcotest.failf "date: %s" e
    in
    Alcotest.(check string)
      "fraction" "2019-04-01T12:34:56.123Z"
      (Email_header.value_to_string (Email_header.Date_single (Some t)))

  let tests =
    [
      ("an empty :all prints as absent", `Quick, test_empty_all_prints_as_absent);
      ( "an encode mismatch names the form",
        `Quick,
        test_encode_mismatch_names_the_form );
      ( "the value printer keeps the fraction",
        `Quick,
        test_value_printer_keeps_the_fraction );
      ("valid form/header combinations", `Quick, test_valid_combinations);
      ("List-Id takes any form", `Quick, test_list_id_is_custom);
      ( "trace fields take the raw form only",
        `Quick,
        test_trace_headers_are_raw_only );
      ( "empty names and forms are rejected",
        `Quick,
        test_empty_names_are_rejected );
      ("malformed names are rejected", `Quick, test_malformed_names_are_rejected);
      ( "constructors check the field name",
        `Quick,
        test_constructors_check_the_name );
      ( "constructors check custom standard names",
        `Quick,
        test_constructors_check_custom_standard_names );
      ("constructors roundtrip", `Quick, test_constructors_roundtrip);
      ("a standard field is typed", `Quick, test_standard_header_is_typed);
      ( "value printer escapes control bytes",
        `Quick,
        test_value_printer_escapes_control_bytes );
      ("invalid form/header combinations", `Quick, test_invalid_combinations);
      ("body part header property", `Quick, test_body_part_header_property);
      ("body part standard property", `Quick, test_body_part_standard_property);
    ]
end

(* Email/import (RFC 8621 Section 4.8) and Email/parse (Section 4.9) *)
module Import_tests = struct
  let id_v = Id.of_string_exn

  let test_null_maps () =
    let r =
      decode_file "Email/import" Email.Import.response_jsont
        "email/import_response_nulls.json"
    in
    Alcotest.(check bool)
      "oldState is None" true
      (r.Email.Import.old_state = None);
    Alcotest.(check bool) "created is None" true (r.Email.Import.created = None);
    Alcotest.(check bool)
      "notCreated is None" true
      (r.Email.Import.not_created = None)

  (* Cyrus sends an empty object rather than the null the RFC allows. *)
  let test_empty_maps () =
    let r =
      decode_file "Email/import" Email.Import.response_jsont
        "email/import_response_cyrus.json"
    in
    Alcotest.(check bool)
      "created is Some []" true
      (r.Email.Import.created = Some []);
    Alcotest.(check bool)
      "notCreated is Some []" true
      (r.Email.Import.not_created = Some []);
    Alcotest.(check bool)
      "no email under k1" true
      (Email.Import.created r (Id.creation "k1") = None)

  let test_created () =
    let r =
      decode_file "Email/import" Email.Import.response_jsont
        "email/import_response_created.json"
    in
    let created = Email.creation "k1" in
    let e = some "created k1" (Email.Import.created r created) in
    Alcotest.(check string) "id" "M1" (Id.to_string (some "id" e.Email.id));
    Alcotest.(check string)
      "blobId" "G1"
      (Id.to_string (some "blobId" e.Email.blob_id));
    let err =
      some "notCreated k2" (Email.Import.not_created r (Email.creation "k2"))
    in
    Alcotest.(check bool)
      "alreadyExists" true
      (err.Error.Set_error.type_ = `Already_exists);
    Alcotest.(check bool)
      "existingId" true
      (Option.map Id.to_string err.Error.Set_error.existing_id = Some "M9")

  (* An EmailImport carries the blob and where to file it; keywords default to
     {} and are omitted when empty, receivedAt is omitted when unset. *)
  let test_import_args_encode () =
    let e =
      Email.Import.email ~blob_id:(id_v "G1")
        ~mailbox_ids:[ id_v "Minbox" ]
        ~keywords:[ `Seen; `Custom "custom" ]
        ()
    in
    let json =
      match encode Email.Import.email_jsont e with
      | Ok s -> s
      | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
    in
    Alcotest.(check string)
      "EmailImport"
      {|{"blobId":"G1","mailboxIds":{"Minbox":true},"keywords":{"$seen":true,"custom":true}}|}
      json;
    let bare =
      Email.Import.email ~blob_id:(id_v "G1") ~mailbox_ids:[ id_v "Minbox" ] ()
    in
    let json =
      match encode Email.Import.email_jsont bare with
      | Ok s -> s
      | Error err -> Alcotest.failf "encode: %s" (Jsont.Error.to_string err)
    in
    Alcotest.(check string)
      "empty keywords omitted" {|{"blobId":"G1","mailboxIds":{"Minbox":true}}|}
      json

  let test_import_args_roundtrip () =
    let creation = Email.creation "k1" in
    let a =
      Email.Import.args ~account_id:(id_v "u1") ~if_in_state:"41"
        ~emails:
          [
            ( creation,
              Email.Import.email ~blob_id:(id_v "G1")
                ~mailbox_ids:[ id_v "Minbox" ]
                () );
          ]
        ()
    in
    let json =
      match encode Email.Import.args_jsont a with
      | Ok s -> s
      | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    in
    Alcotest.(check string)
      "args"
      {|{"accountId":"u1","ifInState":"41","emails":{"k1":{"blobId":"G1","mailboxIds":{"Minbox":true}}}}|}
      json;
    let response =
      decode_file "Email/import" Email.Import.response_jsont
        "email/import_response_created.json"
    in
    Alcotest.(check bool)
      "the same Email creation token reads the result" true
      (Option.is_some (Email.Import.created response creation))

  let tests =
    [
      ("response: created and notCreated null", `Quick, test_null_maps);
      ("response: Cyrus sends {} not null", `Quick, test_empty_maps);
      ("response: created and notCreated entries", `Quick, test_created);
      ("EmailImport encoding", `Quick, test_import_args_encode);
      ("Email/import arguments encoding", `Quick, test_import_args_roundtrip);
      ( "response roundtrip",
        `Quick,
        test_roundtrip "Email/import" Email.Import.response_jsont
          "email/import_response_created.json" );
    ]
end

module Parse_tests = struct
  let id_v = Id.of_string_exn

  (* Cyrus sends all three of parsed/notParsable/notFound as an explicit null
     when it has nothing to put in them. *)
  let test_null_maps () =
    let r =
      decode_file "Email/parse" Email.Parse.response_jsont
        "email/parse_response_nulls.json"
    in
    Alcotest.(check bool) "parsed is None" true (r.Email.Parse.parsed = None);
    Alcotest.(check bool)
      "notParsable is None" true
      (r.Email.Parse.not_parsable = None);
    Alcotest.(check bool)
      "notFound is None" true
      (r.Email.Parse.not_found = None)

  (* RFC 8621 Section 4.9: id, mailboxIds, keywords and receivedAt "will be
     null if requested" on a parsed Email. *)
  let test_parsed () =
    let r =
      decode_file "Email/parse" Email.Parse.response_jsont
        "email/parse_response.json"
    in
    let e = some "parsed G1" (Email.Parse.parsed r (id_v "G1")) in
    Alcotest.(check bool) "id is None" true (e.Email.id = None);
    Alcotest.(check bool) "mailboxIds is None" true (e.Email.mailbox_ids = None);
    Alcotest.(check bool) "keywords is None" true (e.Email.keywords = None);
    Alcotest.(check bool) "receivedAt is None" true (e.Email.received_at = None);
    Alcotest.(check string)
      "subject" "Attached message"
      (some "subject" e.Email.subject);
    Alcotest.(check int)
      "notParsable" 1
      (List.length (some "notParsable" r.Email.Parse.not_parsable));
    Alcotest.(check int)
      "notFound" 1
      (List.length (some "notFound" r.Email.Parse.not_found))

  let test_args_encode () =
    let a =
      Email.Parse.args ~account_id:(id_v "u1")
        ~blob_ids:[ id_v "G1" ]
        ~properties:[ "subject"; "from" ] ~fetch_text_body_values:true
        ~max_body_value_bytes:256L ()
    in
    let json =
      match encode Email.Parse.args_jsont a with
      | Ok s -> s
      | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    in
    Alcotest.(check string)
      "args"
      {|{"accountId":"u1","blobIds":["G1"],"properties":["subject","from"],"fetchTextBodyValues":true,"maxBodyValueBytes":256}|}
      json;
    (* The three fetch flags default to false and are omitted then. *)
    let bare =
      Email.Parse.args ~account_id:(id_v "u1") ~blob_ids:[ id_v "G1" ] ()
    in
    let json =
      match encode Email.Parse.args_jsont bare with
      | Ok s -> s
      | Error e -> Alcotest.failf "encode: %s" (Jsont.Error.to_string e)
    in
    Alcotest.(check string)
      "defaults omitted" {|{"accountId":"u1","blobIds":["G1"]}|} json

  let tests =
    [
      ("response: all maps null", `Quick, test_null_maps);
      ("response: parsed email has null metadata", `Quick, test_parsed);
      ("arguments encoding", `Quick, test_args_encode);
      ( "response roundtrip",
        `Quick,
        test_roundtrip "Email/parse" Email.Parse.response_jsont
          "email/parse_response.json" );
    ]
end

(* The filter and sort constructors of RFC 8621 Sections 2.3, 4.4 and 7.3. *)
module Query_tests = struct
  let id_v = Id.of_string_exn

  let json name jsont v =
    match encode jsont v with
    | Ok s -> s
    | Error e ->
        Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)

  let email_filter f = json "Email filter" Email.filter_jsont f
  let mailbox_filter f = json "Mailbox filter" Mailbox.filter_jsont f

  let submission_filter f =
    json "EmailSubmission filter" Submission.filter_jsont f

  let comparator c = json "Comparator" Filter.comparator_jsont c

  let date y m d =
    match Ptime.of_date_time ((y, m, d), ((0, 0, 0), 0)) with
    | Some t -> t
    | None -> Alcotest.fail "bad date"

  (* RFC 8621 Section 4.4.1: "if zero properties are specified, it is
     automatically true for all objects". *)
  let test_empty_conditions () =
    Alcotest.(check string) "Email" "{}" (email_filter (Email.filter ()));
    Alcotest.(check string) "Mailbox" "{}" (mailbox_filter (Mailbox.filter ()));
    Alcotest.(check string)
      "EmailSubmission" "{}"
      (submission_filter (Submission.filter ()))

  let test_email_filter () =
    Alcotest.(check string)
      "every condition of Section 4.4.1"
      {|{"inMailbox":"Minbox","inMailboxOtherThan":["Mtrash"],"before":"2019-01-02T00:00:00Z","after":"2019-01-01T00:00:00Z","minSize":100,"maxSize":200,"allInThreadHaveKeyword":"$seen","someInThreadHaveKeyword":"$flagged","noneInThreadHaveKeyword":"$draft","hasKeyword":"$answered","notKeyword":"junk","hasAttachment":true,"text":"t","from":"f","to":"o","cc":"c","bcc":"b","subject":"s","body":"y","header":["X-A","b"]}|}
      (email_filter
         (Email.filter ~in_mailbox:(id_v "Minbox")
            ~in_mailbox_other_than:[ id_v "Mtrash" ]
            ~before:(date 2019 1 2) ~after:(date 2019 1 1) ~min_size:100L
            ~max_size:200L ~all_in_thread_have_keyword:`Seen
            ~some_in_thread_have_keyword:`Flagged
            ~none_in_thread_have_keyword:`Draft ~has_keyword:`Answered
            ~not_keyword:(`Custom "junk") ~has_attachment:true ~text:"t"
            ~from:"f" ~to_:"o" ~cc:"c" ~bcc:"b" ~subject:"s" ~body:"y"
            ~header:("X-A", Some "b") ()));
    Alcotest.(check string)
      "a header field with no value" {|{"header":["X-A"]}|}
      (email_filter (Email.filter ~header:("X-A", None) ()))

  let test_mailbox_filter () =
    Alcotest.(check string)
      "every condition of Section 2.3"
      {|{"parentId":"Mtop","name":"Drafts","role":"drafts","hasAnyRole":true,"isSubscribed":false}|}
      (mailbox_filter
         (Mailbox.filter
            ~parent_id:(Some (id_v "Mtop"))
            ~name:"Drafts" ~role:(Some `Drafts) ~has_any_role:true
            ~is_subscribed:false ()));
    (* Section 2.3 types parentId and role T|null, so an explicit null asks
       for the top level and for no role. *)
    Alcotest.(check string)
      "the top level Mailboxes with no role" {|{"parentId":null,"role":null}|}
      (mailbox_filter (Mailbox.filter ~parent_id:None ~role:None ()))

  let test_submission_filter () =
    Alcotest.(check string)
      "every condition of Section 7.3"
      {|{"identityIds":["I1"],"emailIds":["M1"],"threadIds":["T1"],"undoStatus":"pending","before":"2019-01-02T00:00:00Z","after":"2019-01-01T00:00:00Z"}|}
      (submission_filter
         (Submission.filter
            ~identity_ids:[ id_v "I1" ]
            ~email_ids:[ id_v "M1" ]
            ~thread_ids:[ id_v "T1" ]
            ~undo_status:`Pending ~before:(date 2019 1 2) ~after:(date 2019 1 1)
            ()))

  (* A condition still combines with the operators of RFC 8620 Section 5.5. *)
  let test_filter_operator () =
    Alcotest.(check string)
      "NOT over one condition"
      {|{"operator":"NOT","conditions":[{"hasKeyword":"$seen"}]}|}
      (email_filter
         (Filter.Operator
            {
              Filter.operator = `Not;
              conditions = [ Email.filter ~has_keyword:`Seen () ];
            }))

  let test_filter_combinators () =
    let seen = Email.filter ~has_keyword:`Seen () in
    let inbox = Email.filter ~text:"x" () in
    Alcotest.(check string)
      "and_"
      {|{"operator":"AND","conditions":[{"hasKeyword":"$seen"},{"text":"x"}]}|}
      (email_filter (Filter.and_ [ seen; inbox ]));
    Alcotest.(check string)
      "or_ of nothing" {|{"operator":"OR","conditions":[]}|}
      (email_filter (Filter.or_ []));
    Alcotest.(check string)
      "not_" {|{"operator":"NOT","conditions":[{"hasKeyword":"$seen"}]}|}
      (email_filter (Filter.not_ [ seen ]))

  (* RFC 8621 Section 4.4.2. An ascending comparator omits isAscending, which
     RFC 8620 Section 5.5 defaults to true. *)
  let test_email_sort () =
    Alcotest.(check string)
      "receivedAt" {|{"property":"receivedAt"}|}
      (comparator (Email.sort `Received_at));
    Alcotest.(check string)
      "ascending true is the same as no argument" {|{"property":"size"}|}
      (comparator (Email.sort ~ascending:true `Size));
    Alcotest.(check string)
      "descending" {|{"property":"receivedAt","isAscending":false}|}
      (comparator (Email.sort ~ascending:false `Received_at));
    Alcotest.(check string)
      "a collation" {|{"property":"subject","collation":"i;ascii-casemap"}|}
      (comparator (Email.sort ~collation:"i;ascii-casemap" `Subject));
    Alcotest.(check (list string))
      "the wire names of Section 4.4.2" [ "from"; "to"; "sentAt" ]
      (List.map
         (fun c -> c.Filter.property)
         [ Email.sort `From; Email.sort `To; Email.sort `Sent_at ])

  (* Section 4.4.2: "when specifying a hasKeyword, allInThreadHaveKeyword, or
     someInThreadHaveKeyword sort, the Comparator object MUST also have a
     keyword property". *)
  let test_email_keyword_sort () =
    Alcotest.(check string)
      "the example of Section 4.4.2"
      {|{"property":"someInThreadHaveKeyword","isAscending":false,"keyword":"$flagged"}|}
      (comparator
         (Email.sort ~ascending:false (`Some_in_thread_have_keyword `Flagged)));
    Alcotest.(check string)
      "hasKeyword" {|{"property":"hasKeyword","keyword":"$seen"}|}
      (comparator (Email.sort (`Has_keyword `Seen)));
    Alcotest.(check string)
      "allInThreadHaveKeyword"
      {|{"property":"allInThreadHaveKeyword","keyword":"custom"}|}
      (comparator (Email.sort (`All_in_thread_have_keyword (`Custom "custom"))))

  let test_mailbox_sort () =
    Alcotest.(check string)
      "sortOrder" {|{"property":"sortOrder"}|}
      (comparator (Mailbox.sort `Sort_order));
    Alcotest.(check string)
      "name descending" {|{"property":"name","isAscending":false}|}
      (comparator (Mailbox.sort ~ascending:false `Name))

  let test_submission_sort () =
    Alcotest.(check (list string))
      "the wire names of Section 7.3"
      [ "emailId"; "threadId"; "sentAt" ]
      (List.map
         (fun c -> c.Filter.property)
         [
           Submission.sort `Email_id;
           Submission.sort `Thread_id;
           Submission.sort `Sent_at;
         ]);
    Alcotest.(check string)
      "descending with a collation"
      {|{"property":"emailId","isAscending":false,"collation":"i;octet"}|}
      (comparator
         (Submission.sort ~ascending:false ~collation:"i;octet" `Email_id))

  (* The id accessors, which read the property a /get returned. *)
  let test_id_accessors () =
    let i = id_v "X1" in
    Alcotest.(check (option string))
      "Email" (Some "X1")
      (Option.map Id.to_string (Email.id (Email.v ~id:i ())));
    Alcotest.(check (option string))
      "Mailbox" (Some "X1")
      (Option.map Id.to_string
         (Mailbox.id { Mailbox.empty with Mailbox.id = Some i }));
    Alcotest.(check (option string))
      "Thread" (Some "X1")
      (Option.map Id.to_string
         (Thread.id { Thread.id = Some i; email_ids = None }));
    Alcotest.(check (option string))
      "Identity" (Some "X1")
      (Option.map Id.to_string (Identity.id (Identity.v ~id:i ())));
    Alcotest.(check (option string))
      "EmailSubmission" (Some "X1")
      (Option.map Id.to_string (Submission.id (Submission.v ~id:i ())));
    Alcotest.(check (option string))
      "a property the /get did not ask for" None
      (Option.map Id.to_string (Email.id Email.empty))

  (* Section 5.1 lets a /get answer in any order, and the id accessor is what
     puts the order of the query back. *)
  let test_in_ids_order () =
    let e i = Email.v ~id:(id_v i) () in
    Alcotest.(check (list string))
      "the order of the ids" [ "b"; "a" ]
      (List.filter_map
         (fun x -> Option.map Id.to_string (Email.id x))
         (Method.in_ids_order ~id:Email.id
            [ id_v "b"; id_v "a" ]
            [ e "a"; e "b" ]))

  let test_typed_creation () =
    let parent = Mailbox.creation "parent" in
    let draft = Email.creation "draft" in
    Alcotest.(check string)
      "the key is the creation id" "parent"
      (Id.to_string (Id.creation_id parent));
    Alcotest.(check string)
      "the reference carries the hash" "#draft"
      (Id.to_string (Id.creation_ref draft));
    let created =
      Method.
        {
          account_id = id_v "a";
          old_state = None;
          new_state = "s";
          created =
            Some
              [
                ( Id.creation_id parent,
                  { Mailbox.empty with id = Some (id_v "m") } );
              ];
          updated = None;
          destroyed = None;
          not_created = None;
          not_updated = None;
          not_destroyed = None;
        }
    in
    Alcotest.(check bool)
      "created finds the record by its typed creation id" true
      (Option.is_some (Method.created created parent))

  let test_mailbox_list () =
    let e =
      Email.v ~mailbox_ids:[ (id_v "in", true); (id_v "out", false) ] ()
    in
    Alcotest.(check (list string))
      "the mailboxes mapped to true" [ "in" ]
      (List.map Id.to_string (Email.mailbox_list e));
    Alcotest.(check bool) "in the Inbox" true (Email.in_mailbox (id_v "in") e);
    Alcotest.(check bool) "not in out" false (Email.in_mailbox (id_v "out") e);
    Alcotest.(check int)
      "no property asked for" 0
      (List.length (Email.mailbox_list (Email.v ())))

  let test_mailbox_map_encoding () =
    let id = id_v "in" in
    (match encode Email.jsont (Email.v ~mailbox_ids:[ (id, false) ] ()) with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "mailboxIds mapped to false encoded");
    match decode Email.jsont {|{"mailboxIds":{"in":false}}|} with
    | Error e -> Alcotest.failf "tolerant decode: %s" (Jsont.Error.to_string e)
    | Ok email ->
        Alcotest.(check bool)
          "false mailbox retained on decode" true
          (email.Email.mailbox_ids = Some [ (id, false) ])

  let test_set_mailboxes () =
    let id = id_v "in" in
    (match Email.Patch.set_mailboxes [ id; id ] with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "duplicate mailbox ids accepted");
    let entry = Email.Patch.set_mailboxes [ id; id_v "archive" ] in
    let value =
      match Patch.entry_value entry with
      | Some value -> value
      | None -> Alcotest.fail "set_mailboxes produced null"
    in
    match encode Jsont.json value with
    | Error e -> Alcotest.failf "encode patch: %s" (Jsont.Error.to_string e)
    | Ok json ->
        Alcotest.(check string)
          "mailbox map" {|{"in":true,"archive":true}|} json

  let test_sending_address () =
    let with_email email = Identity.v ?email () in
    let check name expect email =
      Alcotest.(check (option string))
        name expect
        (Identity.sending_address ~local_part:"joe" (with_email email))
    in
    check "a fixed address" (Some "joe@example.org") (Some "joe@example.org");
    check "a wildcard local part" (Some "joe@example.org")
      (Some "*@example.org");
    check "an empty address" None (Some "");
    check "a bare star" None (Some "*");
    check "no address" None None

  let tests =
    [
      ("Identity sending address", `Quick, test_sending_address);
      ("typed creation ids", `Quick, test_typed_creation);
      ("mailbox_list and in_mailbox", `Quick, test_mailbox_list);
      ("mailbox map encoding", `Quick, test_mailbox_map_encoding);
      ("set_mailboxes validates ids", `Quick, test_set_mailboxes);
      ("a condition with no argument", `Quick, test_empty_conditions);
      ("Email filter", `Quick, test_email_filter);
      ("Mailbox filter", `Quick, test_mailbox_filter);
      ("EmailSubmission filter", `Quick, test_submission_filter);
      ("a condition under an operator", `Quick, test_filter_operator);
      ("filter combinators", `Quick, test_filter_combinators);
      ("Email sort", `Quick, test_email_sort);
      ("Email keyword sort", `Quick, test_email_keyword_sort);
      ("Mailbox sort", `Quick, test_mailbox_sort);
      ("EmailSubmission sort", `Quick, test_submission_sort);
      ("id accessors", `Quick, test_id_accessors);
      ("id accessor feeds in_ids_order", `Quick, test_in_ids_order);
    ]
end

let () =
  Alcotest.run "jmap-mail"
    [
      ("mailbox", Mailbox_tests.tests);
      ("email", Email_tests.tests);
      ("email-import", Import_tests.tests);
      ("email-parse", Parse_tests.tests);
      ("email-body", Body_tests.tests);
      ("identity", Identity_tests.tests);
      ("submission", Submission_tests.tests);
      ("vacation", Vacation_tests.tests);
      ("capability", Capability_tests.tests);
      ("snippet", Snippet_tests.tests);
      ("header", Header_tests.tests);
      ("keyword", Keyword_tests.tests);
      ("mailbox-create", Mailbox_create_tests.tests);
      ("query", Query_tests.tests);
    ]
