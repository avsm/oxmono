(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP Protocol codec tests using sample JSON files *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    i + n <= h && (String.sub haystack i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let decode jsont json_str = Jmap.Proto.Json.decode jsont json_str
let encode jsont value = Jmap.Proto.Json.encode jsont value

(* Test helpers *)

let test_decode_success name jsont path () =
  let json = read_file path in
  match decode jsont json with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "%s: expected success but got error: %s" name
        (Jsont.Error.to_string e)

let test_decode_failure name jsont path () =
  let json = read_file path in
  match decode jsont json with
  | Ok _ -> Alcotest.failf "%s: expected failure but got success" name
  | Error _ -> ()

let test_roundtrip name jsont path () =
  let json = read_file path in
  match decode jsont json with
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)
  | Ok value -> (
      match encode jsont value with
      | Error e ->
          Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)
      | Ok encoded -> (
          match decode jsont encoded with
          | Error e ->
              Alcotest.failf "%s: re-decode failed: %s" name
                (Jsont.Error.to_string e)
          | Ok _ -> ()))

(* Helpers for extracting values from optional fields in tests *)
let get_id opt =
  match opt with
  | Some id -> Jmap.Proto.Id.to_string id
  | None -> Alcotest.fail "expected id"

let get_string opt =
  match opt with Some s -> s | None -> Alcotest.fail "expected string"

let get_int64 opt =
  match opt with Some n -> n | None -> Alcotest.fail "expected int64"

let get_bool opt =
  match opt with Some b -> b | None -> Alcotest.fail "expected bool"

(* ID tests *)
module Id_tests = struct
  open Jmap.Proto

  let test_valid_simple () =
    let json = "\"abc123\"" in
    match decode Id.jsont json with
    | Ok id -> Alcotest.(check string) "id value" "abc123" (Id.to_string id)
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_valid_single_char () =
    let json = "\"a\"" in
    match decode Id.jsont json with
    | Ok id -> Alcotest.(check string) "id value" "a" (Id.to_string id)
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_valid_with_hyphen () =
    let json = "\"msg-2024-01\"" in
    match decode Id.jsont json with
    | Ok id ->
        Alcotest.(check string) "id value" "msg-2024-01" (Id.to_string id)
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_valid_with_underscore () =
    let json = "\"user_id_123\"" in
    match decode Id.jsont json with
    | Ok id ->
        Alcotest.(check string) "id value" "user_id_123" (Id.to_string id)
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_invalid_empty () =
    let json = "\"\"" in
    match decode Id.jsont json with
    | Ok _ -> Alcotest.fail "expected failure for empty id"
    | Error _ -> ()

  (* RFC 8620 Section 1.2 restricts an Id to the base64url alphabet, but that
     is a rule for the server allocating it. Decoding takes what a server sent;
     Id.of_string is where the alphabet is enforced. *)
  let off_alphabet name json value =
    ( name,
      `Quick,
      fun () ->
        (match decode Id.jsont json with
        | Ok id ->
            Alcotest.(check string) "decoded verbatim" value (Id.to_string id)
        | Error e ->
            Alcotest.failf "%s: a server id should decode: %s" name
              (Jsont.Error.to_string e));
        match Id.of_string value with
        | Ok _ ->
            Alcotest.failf "%s: of_string should enforce the alphabet" name
        | Error _ -> () )

  let test_invalid_not_string () =
    let json = "12345" in
    match decode Id.jsont json with
    | Ok _ -> Alcotest.fail "expected failure for non-string"
    | Error _ -> ()

  let test_edge_max_length () =
    let id_255 = String.make 255 'a' in
    let json = Printf.sprintf "\"%s\"" id_255 in
    match decode Id.jsont json with
    | Ok id ->
        Alcotest.(check int) "id length" 255 (String.length (Id.to_string id))
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_edge_over_max_length () =
    let id_256 = String.make 256 'a' in
    let json = Printf.sprintf "\"%s\"" id_256 in
    match decode Id.jsont json with
    | Ok _ -> Alcotest.fail "expected failure for 256 char id"
    | Error _ -> ()

  (* RFC 8620 Section 5.3: a client argument may name a record created in the
     same request as "#" followed by its creation id. *)
  let test_of_string_or_creation () =
    (match Id.of_string_or_creation "#draft1" with
    | Ok id ->
        Alcotest.(check (option string))
          "creation id" (Some "draft1") (Id.to_creation_id id);
        Alcotest.(check string) "to_string" "#draft1" (Id.to_string id)
    | Error msg -> Alcotest.failf "rejected: %s" msg);
    (match Id.of_string_or_creation "M1" with
    | Ok id -> Alcotest.(check bool) "plain id" false (Id.is_creation_ref id)
    | Error msg -> Alcotest.failf "rejected: %s" msg);
    match Id.of_string_or_creation "#" with
    | Ok _ -> Alcotest.fail "expected a bare # to be rejected"
    | Error _ -> ()

  (* The octet position in the message counts from the start of the whole
     reference, not from the id after the "#". *)
  let test_creation_position () =
    match Id.of_string_or_creation "#ab!c" with
    | Ok _ -> Alcotest.fail "expected a rejection"
    | Error msg ->
        Alcotest.(check bool)
          ("position 3 in " ^ msg) true
          (let n = String.length msg in
           let rec go i =
             i + 10 <= n && (String.sub msg i 10 = "position 3" || go (i + 1))
           in
           go 0)

  (* RFC 8620 Section 5.3 keys a create map by the bare creation id and names
     the record elsewhere in the request with the "#" prefixed form. *)
  let test_creation_token () =
    let c = Id.creation "draft1" in
    Alcotest.(check string)
      "create map key" "draft1"
      (Id.to_string (Id.creation_id c));
    Alcotest.(check string)
      "argument form" "#draft1"
      (Id.to_string (Id.creation_ref c));
    Alcotest.(check bool)
      "the key is not a reference" false
      (Id.is_creation_ref (Id.creation_id c));
    Alcotest.(check bool)
      "the argument form is" true
      (Id.is_creation_ref (Id.creation_ref c));
    Alcotest.(check (option string))
      "and reads back" (Some "draft1")
      (Id.to_creation_id (Id.creation_ref c));
    Alcotest.(check string)
      "pp_creation prints the creation id" "draft1"
      (Format.asprintf "%a" Id.pp_creation c)

  let test_creation_token_invalid () =
    Alcotest.check_raises "an ill formed creation id"
      (Invalid_argument
         "Invalid creation id: Invalid character '!' in Id at position 2")
      (fun () -> ignore (Id.creation "ab!" : unit Id.creation));
    Alcotest.check_raises "an empty creation id"
      (Invalid_argument "Invalid creation id: Id cannot be empty") (fun () ->
        ignore (Id.creation "" : unit Id.creation))

  let tests =
    [
      ("creation reference", `Quick, test_of_string_or_creation);
      ("creation reference position", `Quick, test_creation_position);
      ("creation token", `Quick, test_creation_token);
      ("creation token rejects a bad id", `Quick, test_creation_token_invalid);
      ("valid: simple", `Quick, test_valid_simple);
      ("valid: single char", `Quick, test_valid_single_char);
      ("valid: with hyphen", `Quick, test_valid_with_hyphen);
      ("valid: with underscore", `Quick, test_valid_with_underscore);
      ("invalid: empty", `Quick, test_invalid_empty);
      off_alphabet "off the alphabet: with space" "\"hello world\""
        "hello world";
      off_alphabet "off the alphabet: with @" "\"abc@def\"" "abc@def";
      ("invalid: not string", `Quick, test_invalid_not_string);
      ("edge: max length 255", `Quick, test_edge_max_length);
      ("edge: over max length 256", `Quick, test_edge_over_max_length);
    ]
end

(* Int53 tests *)
module Int53_tests = struct
  open Jmap.Proto

  let test_zero () =
    match decode Int53.Signed.jsont "0" with
    | Ok n -> Alcotest.(check int64) "value" 0L n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_positive () =
    match decode Int53.Signed.jsont "12345" with
    | Ok n -> Alcotest.(check int64) "value" 12345L n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_negative () =
    match decode Int53.Signed.jsont "-12345" with
    | Ok n -> Alcotest.(check int64) "value" (-12345L) n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_max_safe () =
    match decode Int53.Signed.jsont "9007199254740991" with
    | Ok n -> Alcotest.(check int64) "value" 9007199254740991L n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_min_safe () =
    match decode Int53.Signed.jsont "-9007199254740991" with
    | Ok n -> Alcotest.(check int64) "value" (-9007199254740991L) n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_over_max_safe () =
    match decode Int53.Signed.jsont "9007199254740992" with
    | Ok _ -> Alcotest.fail "expected failure for over max safe"
    | Error _ -> ()

  let test_under_min_safe () =
    match decode Int53.Signed.jsont "-9007199254740992" with
    | Ok _ -> Alcotest.fail "expected failure for under min safe"
    | Error _ -> ()

  let test_unsigned_zero () =
    match decode Int53.Unsigned.jsont "0" with
    | Ok n -> Alcotest.(check int64) "value" 0L n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_unsigned_max () =
    match decode Int53.Unsigned.jsont "9007199254740991" with
    | Ok n -> Alcotest.(check int64) "value" 9007199254740991L n
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_unsigned_negative () =
    match decode Int53.Unsigned.jsont "-1" with
    | Ok _ -> Alcotest.fail "expected failure for negative unsigned"
    | Error _ -> ()

  (* Fixture-driven cases: the JSON files under [int53/] are the same corpus
     the inline tests above cover, read through the codec from disk. *)
  let fixture_value path expect () =
    match decode Int53.Signed.jsont (read_file path) with
    | Ok n -> Alcotest.(check int64) path expect n
    | Error e ->
        Alcotest.failf "%s: decode failed: %s" path (Jsont.Error.to_string e)

  let fixture_rejected path () =
    match decode Int53.Signed.jsont (read_file path) with
    | Ok _ -> Alcotest.failf "%s: expected decode failure" path
    | Error _ -> ()

  let fixture_roundtrip path () = test_roundtrip path Int53.Signed.jsont path ()

  let tests =
    [
      ("signed: zero", `Quick, test_zero);
      ("signed: positive", `Quick, test_positive);
      ("signed: negative", `Quick, test_negative);
      ("signed: max safe", `Quick, test_max_safe);
      ("signed: min safe", `Quick, test_min_safe);
      ("signed: over max safe", `Quick, test_over_max_safe);
      ("signed: under min safe", `Quick, test_under_min_safe);
      ("unsigned: zero", `Quick, test_unsigned_zero);
      ("unsigned: max", `Quick, test_unsigned_max);
      ("unsigned: negative fails", `Quick, test_unsigned_negative);
      ("fixture: valid/zero", `Quick, fixture_value "int53/valid/zero.json" 0L);
      ( "fixture: valid/positive",
        `Quick,
        fixture_value "int53/valid/positive.json" 12345L );
      ( "fixture: valid/negative",
        `Quick,
        fixture_value "int53/valid/negative.json" (-12345L) );
      ( "fixture: valid/max_safe",
        `Quick,
        fixture_value "int53/valid/max_safe.json" 9007199254740991L );
      ( "fixture: valid/min_safe",
        `Quick,
        fixture_value "int53/valid/min_safe.json" (-9007199254740991L) );
      ( "fixture: edge/over_max_safe",
        `Quick,
        fixture_rejected "int53/edge/over_max_safe.json" );
      ( "fixture: edge/under_min_safe",
        `Quick,
        fixture_rejected "int53/edge/under_min_safe.json" );
      ( "fixture: invalid/float",
        `Quick,
        fixture_rejected "int53/invalid/float.json" );
      ( "fixture: invalid/null",
        `Quick,
        fixture_rejected "int53/invalid/null.json" );
      ( "fixture: roundtrip max_safe",
        `Quick,
        fixture_roundtrip "int53/valid/max_safe.json" );
    ]
end

(* Date tests *)
module Date_tests = struct
  open Jmap.Proto

  let test_utc_z () =
    match decode Date.utc_jsont "\"2024-01-15T10:30:00Z\"" with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_rfc3339_with_offset () =
    match decode Date.jsont "\"2024-01-15T10:30:00+05:30\"" with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_with_milliseconds () =
    match decode Date.jsont "\"2024-01-15T10:30:00.123Z\"" with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)

  let test_invalid_format () =
    match decode Date.jsont "\"January 15, 2024\"" with
    | Ok _ -> Alcotest.fail "expected failure for invalid format"
    | Error _ -> ()

  let test_not_string () =
    match decode Date.jsont "1705315800" with
    | Ok _ -> Alcotest.fail "expected failure for non-string"
    | Error _ -> ()

  let tests =
    [
      ("utc: Z suffix", `Quick, test_utc_z);
      ("rfc3339: with offset", `Quick, test_rfc3339_with_offset);
      ("rfc3339: with milliseconds", `Quick, test_with_milliseconds);
      ("invalid: bad format", `Quick, test_invalid_format);
      ("invalid: not string", `Quick, test_not_string);
    ]
end

(* Session tests *)
module Session_tests = struct
  open Jmap.Proto

  let test_minimal () =
    test_decode_success "minimal session" Session.jsont
      "session/valid/minimal.json" ()

  let test_with_mail () =
    test_decode_success "session with mail" Session.jsont
      "session/valid/with_mail.json" ()

  let test_roundtrip_minimal () =
    test_roundtrip "minimal session roundtrip" Session.jsont
      "session/valid/minimal.json" ()

  let test_values () =
    let json = read_file "session/valid/minimal.json" in
    match decode Session.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok session ->
        Alcotest.(check string)
          "username" "test@example.com" session.Session.username;
        Alcotest.(check string)
          "apiUrl" "https://api.example.com/jmap/" session.Session.api_url;
        Alcotest.(check string) "state" "abc123" session.Session.state;
        Alcotest.(check bool)
          "has core capability" true
          (Session.has_capability Capability.core session)

  let test_with_accounts () =
    test_decode_success "with accounts" Session.jsont
      "session/valid/with_accounts.json" ()

  let test_empty_accounts () =
    test_decode_success "empty accounts" Session.jsont
      "session/edge/empty_accounts.json" ()

  let test_accounts_values () =
    let json = read_file "session/valid/with_accounts.json" in
    match decode Session.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok session ->
        Alcotest.(check int)
          "accounts count" 2
          (List.length session.Session.accounts);
        Alcotest.(check int)
          "primary_accounts count" 2
          (List.length session.Session.primary_accounts)

  (* RFC 8621 Sections 1.3.1 and 1.3.2 put the mail and submission limits in
     accountCapabilities and leave the session wide value empty. *)
  let account_session_with account_capabilities =
    Printf.sprintf
      {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1,
        "maxConcurrentUpload":1,"maxSizeRequest":1,"maxConcurrentRequests":1,
        "maxCallsInRequest":1,"maxObjectsInGet":1,"maxObjectsInSet":1,
        "collationAlgorithms":[]},"urn:ietf:params:jmap:mail":{}},
      "accounts":{"acc1":{"name":"P","isPersonal":true,"isReadOnly":false,
        "accountCapabilities":{%s}}},
      "primaryAccounts":{},"username":"u","apiUrl":"/a","downloadUrl":"/d",
      "uploadUrl":"/u","eventSourceUrl":"/e","state":"s"}|}
      account_capabilities

  let account_session =
    account_session_with
      {|"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":null,
         "maxMailboxDepth":10,"maxSizeMailboxName":100,
         "maxSizeAttachmentsPerEmail":50000000,
         "emailQuerySortOptions":[],"mayCreateTopLevelMailbox":true},
         "urn:ietf:params:jmap:submission":{"maxDelayedSend":86400,
         "submissionExtensions":{}}|}

  let test_capability_lookups () =
    match decode Session.jsont account_session with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok session -> (
        (match Session.core_capability session with
        | None -> Alcotest.fail "expected a core capability"
        | Some c ->
            Alcotest.(check int64)
              "maxCallsInRequest" 1L c.Capability.Core.max_calls_in_request);
        match Session.find_account (Id.of_string_exn "acc1") session with
        | None -> Alcotest.fail "expected account acc1"
        | Some a ->
            (match Session.mail_capability a with
            | None -> Alcotest.fail "expected a mail capability"
            | Some m ->
                Alcotest.(check (option int64))
                  "maxMailboxDepth" (Some 10L)
                  m.Capability.Mail.max_mailbox_depth);
            (match Session.submission_capability a with
            | None -> Alcotest.fail "expected a submission capability"
            | Some sub ->
                Alcotest.(check (option int64))
                  "maxDelayedSend" (Some 86400L)
                  sub.Capability.Submission.max_delayed_send);
            Alcotest.(check bool)
              "no account found" true
              (Session.find_account (Id.of_string_exn "nope") session = None))

  let test_missing_api_url () =
    test_decode_failure "missing apiUrl" Session.jsont
      "session/invalid/missing_api_url.json" ()

  let test_missing_capabilities () =
    test_decode_failure "missing capabilities" Session.jsont
      "session/invalid/missing_capabilities.json" ()

  let test_missing_core_capability () =
    let json =
      {|{"capabilities":{"urn:ietf:params:jmap:mail":{}},
         "accounts":{},"primaryAccounts":{},"username":"u",
         "apiUrl":"/a","downloadUrl":"/d","uploadUrl":"/u",
         "eventSourceUrl":"/e","state":"s"}|}
    in
    match decode Session.jsont json with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "session without mandatory core capability accepted"

  let test_malformed_core_capability () =
    test_decode_failure "malformed core capability" Session.jsont
      "session/invalid/malformed_core_capability.json" ()

  let test_malformed_account_capabilities () =
    let rejected name capabilities =
      match decode Session.jsont (account_session_with capabilities) with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.failf "%s: malformed account capability was accepted" name
    in
    rejected "mail missing members"
      {|"urn:ietf:params:jmap:mail":{"maxMailboxDepth":10}|};
    rejected "mail minimum"
      {|"urn:ietf:params:jmap:mail":{"maxMailboxesPerEmail":0,
        "maxMailboxDepth":10,"maxSizeMailboxName":99,
        "maxSizeAttachmentsPerEmail":1,"emailQuerySortOptions":[],
        "mayCreateTopLevelMailbox":true}|};
    rejected "submission"
      {|"urn:ietf:params:jmap:submission":{"maxDelayedSend":86400}|};
    rejected "vacation response"
      {|"urn:ietf:params:jmap:vacationresponse":{"unexpected":true}|}

  let test_malformed_session_capabilities () =
    let rejected name capability =
      let json =
        String.concat ""
          [
            {|{"capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1,"maxConcurrentUpload":1,"maxSizeRequest":1,"maxConcurrentRequests":1,"maxCallsInRequest":1,"maxObjectsInGet":1,"maxObjectsInSet":1,"collationAlgorithms":[]},|};
            capability;
            {|},"accounts":{},"primaryAccounts":{},"username":"u","apiUrl":"/a","downloadUrl":"/d","uploadUrl":"/u","eventSourceUrl":"/e","state":"s"}|};
          ]
      in
      match decode Session.jsont json with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.failf "%s: malformed session capability was accepted" name
    in
    rejected "mail" {|"urn:ietf:params:jmap:mail":{"maxMailboxDepth":10}|};
    rejected "submission"
      {|"urn:ietf:params:jmap:submission":{"maxDelayedSend":1}|};
    rejected "vacation response"
      {|"urn:ietf:params:jmap:vacationresponse":{"unexpected":true}|}

  let tests =
    [
      ("valid: minimal", `Quick, test_minimal);
      ("valid: with mail", `Quick, test_with_mail);
      ("valid: with accounts", `Quick, test_with_accounts);
      ("edge: empty accounts", `Quick, test_empty_accounts);
      ("roundtrip: minimal", `Quick, test_roundtrip_minimal);
      ("values: minimal", `Quick, test_values);
      ("values: accounts", `Quick, test_accounts_values);
      ("capability lookups", `Quick, test_capability_lookups);
      ("invalid: missing apiUrl", `Quick, test_missing_api_url);
      ("invalid: missing capabilities", `Quick, test_missing_capabilities);
      ("invalid: missing core capability", `Quick, test_missing_core_capability);
      ( "invalid: malformed core capability",
        `Quick,
        test_malformed_core_capability );
      ( "invalid: malformed account capabilities",
        `Quick,
        test_malformed_account_capabilities );
      ( "invalid: malformed session capabilities",
        `Quick,
        test_malformed_session_capabilities );
    ]
end

(* Request tests *)
module Request_tests = struct
  open Jmap.Proto

  let test_roundtrip_result_reference () =
    test_roundtrip "result reference roundtrip" Request.jsont
      "request/valid/with_result_reference.json" ()

  let test_single_method () =
    test_decode_success "single method" Request.jsont
      "request/valid/single_method.json" ()

  let test_multiple_methods () =
    test_decode_success "multiple methods" Request.jsont
      "request/valid/multiple_methods.json" ()

  let test_with_created_ids () =
    test_decode_success "with created ids" Request.jsont
      "request/valid/with_created_ids.json" ()

  let test_empty_methods () =
    test_decode_success "empty methods" Request.jsont
      "request/valid/empty_methods.json" ()

  let test_values () =
    let json = read_file "request/valid/single_method.json" in
    match decode Request.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok request ->
        Alcotest.(check int) "using count" 2 (List.length request.Request.using);
        Alcotest.(check int)
          "method calls count" 1
          (List.length request.Request.method_calls)

  let test_roundtrip () =
    test_roundtrip "single method roundtrip" Request.jsont
      "request/valid/single_method.json" ()

  let test_with_creation_refs () =
    test_decode_success "with creation refs" Request.jsont
      "request/valid/with_creation_refs.json" ()

  let test_with_result_reference () =
    test_decode_success "with result reference" Request.jsont
      "request/valid/with_result_reference.json" ()

  (* The printed form is indented JSON that decodes back to the request. *)
  let test_pp () =
    let json = read_file "request/valid/single_method.json" in
    match decode Request.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok request -> (
        let printed = Format.asprintf "%a" Request.pp request in
        Alcotest.(check bool)
          ("several lines: " ^ printed)
          true
          (String.contains printed '\n');
        Alcotest.(check bool)
          ("the calls are printed: " ^ printed)
          true
          (contains ~needle:"methodCalls" printed
          && contains ~needle:"Mailbox/get" printed);
        match decode Request.jsont printed with
        | Error e ->
            Alcotest.failf "the printed request is not JSON: %s"
              (Jsont.Error.to_string e)
        | Ok r ->
            Alcotest.(check int)
              "the same calls come back"
              (List.length request.Request.method_calls)
              (List.length r.Request.method_calls))

  let test_missing_using () =
    test_decode_failure "missing using" Request.jsont
      "request/invalid/missing_using.json" ()

  let test_not_object () =
    test_decode_failure "not an object" Request.jsont
      "request/invalid/not_object.json" ()

  let tests =
    [
      ("valid: single method", `Quick, test_single_method);
      ("valid: multiple methods", `Quick, test_multiple_methods);
      ("valid: with created ids", `Quick, test_with_created_ids);
      ("valid: empty methods", `Quick, test_empty_methods);
      ("values: single method", `Quick, test_values);
      ("roundtrip: single method", `Quick, test_roundtrip);
      ("valid: with creation refs", `Quick, test_with_creation_refs);
      ("valid: with result reference", `Quick, test_with_result_reference);
      ("pp: indented JSON", `Quick, test_pp);
      ( "roundtrip: with result reference",
        `Quick,
        test_roundtrip_result_reference );
      ("invalid: missing using", `Quick, test_missing_using);
      ("invalid: not object", `Quick, test_not_object);
    ]
end

(* Response tests *)
module Response_tests = struct
  open Jmap.Proto

  let test_success () =
    test_decode_success "success" Response.jsont "response/valid/success.json"
      ()

  let test_with_created_ids () =
    test_decode_success "with created ids" Response.jsont
      "response/valid/with_created_ids.json" ()

  let test_with_error () =
    test_decode_success "with error" Response.jsont
      "response/valid/with_error.json" ()

  let test_multiple_responses () =
    test_decode_success "multiple responses" Response.jsont
      "response/valid/multiple_responses.json" ()

  (* RFC 8620 Section 3.6.2: a failed call answers with the method name
     "error" and the error object as its arguments. *)
  let test_error () =
    let json = read_file "response/valid/with_error.json" in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp -> (
        match Response.find_response "c1" resp with
        | None -> Alcotest.fail "expected a response for c1"
        | Some inv -> (
            Alcotest.(check bool) "is_error" true (Response.is_error inv);
            match Response.error inv with
            | Some (Ok e) ->
                Alcotest.(check string)
                  "type" "unknownMethod"
                  (Error.Method_error.type_to_string e.Error.Method_error.type_)
            | Some (Error e) ->
                Alcotest.failf "error did not decode: %s"
                  (Jsont.Error.to_string e)
            | None -> Alcotest.fail "expected an error"))

  let test_error_not_an_error () =
    let json = read_file "response/valid/success.json" in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp -> (
        match resp.Response.method_responses with
        | inv :: _ ->
            Alcotest.(check bool) "not an error" true (Response.error inv = None)
        | [] -> Alcotest.fail "expected a response")

  (* An "error" response whose arguments are not a method error object is
     reported rather than silently taken for "no error". *)
  let test_error_undecodable () =
    let json =
      {|{"methodResponses":[["error",{"type":42},"c1"]],"sessionState":"s"}|}
    in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp -> (
        match Response.find_response "c1" resp with
        | Some inv -> (
            match Response.error inv with
            | Some (Error _) -> ()
            | Some (Ok _) -> Alcotest.fail "expected a decode failure"
            | None -> Alcotest.fail "expected an error")
        | None -> Alcotest.fail "expected a response for c1")

  let test_values () =
    let json = read_file "response/valid/success.json" in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok response ->
        Alcotest.(check string)
          "session state" "session123" response.Response.session_state;
        Alcotest.(check int)
          "method responses count" 1
          (List.length response.Response.method_responses)

  let test_pp () =
    let json = read_file "response/valid/success.json" in
    match decode Response.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok response -> (
        let printed = Format.asprintf "%a" Response.pp response in
        Alcotest.(check bool)
          ("several lines: " ^ printed)
          true
          (String.contains printed '\n');
        Alcotest.(check bool)
          ("the responses are printed: " ^ printed)
          true
          (contains ~needle:"methodResponses" printed
          && contains ~needle:"session123" printed);
        match decode Response.jsont printed with
        | Error e ->
            Alcotest.failf "the printed response is not JSON: %s"
              (Jsont.Error.to_string e)
        | Ok r ->
            Alcotest.(check string)
              "the same session state comes back"
              response.Response.session_state r.Response.session_state)

  let test_roundtrip () =
    test_roundtrip "success roundtrip" Response.jsont
      "response/valid/success.json" ()

  let test_missing_session_state () =
    test_decode_failure "missing sessionState" Response.jsont
      "response/invalid/missing_session_state.json" ()

  let tests =
    [
      ("valid: success", `Quick, test_success);
      ("valid: with created ids", `Quick, test_with_created_ids);
      ("valid: with error", `Quick, test_with_error);
      ("error: typed", `Quick, test_error);
      ("error: not an error", `Quick, test_error_not_an_error);
      ("error: undecodable", `Quick, test_error_undecodable);
      ("valid: multiple responses", `Quick, test_multiple_responses);
      ("values: success", `Quick, test_values);
      ("roundtrip: success", `Quick, test_roundtrip);
      ("pp: indented JSON", `Quick, test_pp);
      ("invalid: missing session state", `Quick, test_missing_session_state);
    ]
end

(* Invocation tests *)
module Invocation_tests = struct
  open Jmap.Proto

  let test_get () =
    test_decode_success "get" Invocation.jsont "invocation/valid/get.json" ()

  let test_set () =
    test_decode_success "set" Invocation.jsont "invocation/valid/set.json" ()

  let test_query () =
    test_decode_success "query" Invocation.jsont "invocation/valid/query.json"
      ()

  let test_values () =
    let json = read_file "invocation/valid/get.json" in
    match decode Invocation.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok inv ->
        Alcotest.(check string) "name" "Email/get" inv.Invocation.name;
        Alcotest.(check string)
          "method call id" "call-001" inv.Invocation.method_call_id

  let test_invalid_not_array () =
    test_decode_failure "not array" Invocation.jsont
      "invocation/invalid/not_array.json" ()

  let test_invalid_wrong_length () =
    test_decode_failure "wrong length" Invocation.jsont
      "invocation/invalid/wrong_length.json" ()

  let tests =
    [
      ("valid: get", `Quick, test_get);
      ("valid: set", `Quick, test_set);
      ("valid: query", `Quick, test_query);
      ("values: get", `Quick, test_values);
      ("invalid: not array", `Quick, test_invalid_not_array);
      ("invalid: wrong length", `Quick, test_invalid_wrong_length);
    ]
end

(* Capability tests *)
module Capability_tests = struct
  open Jmap.Proto

  let test_core () =
    test_decode_success "core" Capability.Core.jsont
      "capability/valid/core.json" ()

  let test_mail () =
    test_decode_success "mail" Capability.Mail.account_jsont
      "capability/valid/mail.json" ()

  let test_submission () =
    test_decode_success "submission" Capability.Submission.account_jsont
      "capability/valid/submission.json" ()

  let test_core_values () =
    let json = read_file "capability/valid/core.json" in
    match decode Capability.Core.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok cap ->
        Alcotest.(check int64)
          "maxSizeUpload" 50000000L cap.Capability.Core.max_size_upload;
        Alcotest.(check int64)
          "maxConcurrentUpload" 4L cap.Capability.Core.max_concurrent_upload;
        Alcotest.(check int64)
          "maxCallsInRequest" 16L cap.Capability.Core.max_calls_in_request

  let test_mail_values () =
    let json = read_file "capability/valid/mail.json" in
    match decode Capability.Mail.account_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok cap ->
        Alcotest.(check (option int64))
          "maxSizeMailboxName" (Some 490L)
          cap.Capability.Mail.max_size_mailbox_name;
        Alcotest.(check (option bool))
          "mayCreateTopLevelMailbox" (Some true)
          cap.Capability.Mail.may_create_top_level_mailbox

  let json_of s =
    match decode Jsont.json s with
    | Ok j -> j
    | Error e -> Alcotest.failf "json: %s" (Jsont.Error.to_string e)

  let test_of_json_unknown_uri () =
    match
      Capability.session_capability_of_json "urn:example:x"
        (json_of {|{"a":1}|})
    with
    | Ok (Capability.Unknown _) -> ()
    | Ok _ -> Alcotest.fail "expected Unknown"
    | Error msg -> Alcotest.failf "unexpected error: %s" msg

  let test_of_json_bad_known_uri () =
    match
      Capability.session_capability_of_json Capability.core (json_of {|{}|})
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected a decode error for an empty core object"

  (* RFC 8621 Section 1.3.3 gives the vacation response capability no
     settings, so a non-empty object is something else. *)
  let test_of_json_vacation () =
    (match
       Capability.session_capability_of_json Capability.vacation_response
         (json_of {|{}|})
     with
    | Ok Capability.Vacation_response -> ()
    | _ -> Alcotest.fail "expected Vacation_response");
    match
      Capability.session_capability_of_json Capability.vacation_response
        (json_of {|{"x":1}|})
    with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected an error for a non-empty object"

  (* An account-scoped Mail capability must survive [capability_to_json], whose
     two nullable members are mandatory at that scope. A session-scoped one is
     the empty object. *)
  let test_to_json_round_trip () =
    let json = read_file "capability/valid/mail.json" in
    let cap =
      match decode Capability.Mail.account_jsont json with
      | Ok cap -> Capability.Mail cap
      | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    in
    let uri, encoded = Capability.capability_to_json (Capability.mail, cap) in
    (match Capability.account_capability_of_json uri encoded with
    | Ok (Capability.Mail _) -> ()
    | Ok _ -> Alcotest.fail "expected Mail"
    | Error msg -> Alcotest.failf "account round trip failed: %s" msg);
    let uri, encoded =
      Capability.capability_to_json
        (Capability.mail, Capability.Mail (Capability.Mail.create ()))
    in
    match Capability.session_capability_of_json uri encoded with
    | Ok (Capability.Mail _) -> ()
    | Ok _ -> Alcotest.fail "expected Mail"
    | Error msg -> Alcotest.failf "session round trip failed: %s" msg

  let tests =
    [
      ("to_json: mail round trip", `Quick, test_to_json_round_trip);
      ("of_json: unknown uri", `Quick, test_of_json_unknown_uri);
      ("of_json: bad known uri", `Quick, test_of_json_bad_known_uri);
      ("of_json: vacation response", `Quick, test_of_json_vacation);
      ("valid: core", `Quick, test_core);
      ("valid: mail", `Quick, test_mail);
      ("valid: submission", `Quick, test_submission);
      ("values: core", `Quick, test_core_values);
      ("values: mail", `Quick, test_mail_values);
    ]
end

(* Method args/response tests *)
module Method_tests = struct
  open Jmap.Proto

  let test_get_args () =
    test_decode_success "get_args" Method.get_args_jsont
      "method/valid/get_args.json" ()

  let test_get_args_minimal () =
    test_decode_success "get_args_minimal" Method.get_args_jsont
      "method/valid/get_args_minimal.json" ()

  let test_query_response () =
    test_decode_success "query_response" Method.query_response_jsont
      "method/valid/query_response.json" ()

  let test_changes_response () =
    test_decode_success "changes_response" Method.changes_response_jsont
      "method/valid/changes_response.json" ()

  let test_get_args_values () =
    let json = read_file "method/valid/get_args.json" in
    match decode Method.get_args_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok args ->
        Alcotest.(check string)
          "accountId" "acc1"
          (Id.to_string args.account_id);
        Alcotest.(check (option (list string)))
          "properties"
          (Some [ "id"; "name"; "role" ])
          args.properties

  let test_query_response_values () =
    let json = read_file "method/valid/query_response.json" in
    match decode Method.query_response_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp ->
        Alcotest.(check int) "ids count" 5 (List.length resp.ids);
        Alcotest.(check int64) "position" 0L resp.position;
        Alcotest.(check bool)
          "canCalculateChanges" true resp.can_calculate_changes;
        Alcotest.(check (option int64)) "total" (Some 250L) resp.total

  let test_changes_response_values () =
    let json = read_file "method/valid/changes_response.json" in
    match decode Method.changes_response_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok resp ->
        Alcotest.(check string) "oldState" "old123" resp.old_state;
        Alcotest.(check string) "newState" "new456" resp.new_state;
        Alcotest.(check bool) "hasMoreChanges" false resp.has_more_changes;
        Alcotest.(check int) "created count" 2 (List.length resp.created);
        Alcotest.(check int) "destroyed count" 2 (List.length resp.destroyed)

  (* [query_args] is generic in the filter type; the fixture uses a Mail-style
     filter, whose conditions are kept here as generic JSON objects. *)
  let json_condition =
    Jsont.Object.map ~kind:"FilterCondition" Fun.id
    |> Jsont.Object.keep_unknown Unknown.mems ~enc:Fun.id
    |> Jsont.Object.finish

  let query_args_jsont = Method.query_args_jsont json_condition

  let test_query_args () =
    test_decode_success "query_args" query_args_jsont
      "method/valid/query_args.json" ()

  let test_query_args_values () =
    let json = read_file "method/valid/query_args.json" in
    match decode query_args_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok args ->
        Alcotest.(check string)
          "accountId" "acc1"
          (Id.to_string args.Method.account_id);
        Alcotest.(check bool) "filter present" true (args.Method.filter <> None);
        Alcotest.(check (option int))
          "sort count" (Some 1)
          (Option.map List.length args.Method.sort);
        Alcotest.(check int64) "position" 0L args.Method.position;
        Alcotest.(check (option int64)) "limit" (Some 100L) args.Method.limit;
        Alcotest.(check bool) "calculateTotal" true args.Method.calculate_total

  let tests =
    [
      ("valid: get_args", `Quick, test_get_args);
      ("valid: get_args_minimal", `Quick, test_get_args_minimal);
      ("valid: query_response", `Quick, test_query_response);
      ("valid: changes_response", `Quick, test_changes_response);
      ("values: get_args", `Quick, test_get_args_values);
      ("values: query_response", `Quick, test_query_response_values);
      ("values: changes_response", `Quick, test_changes_response_values);
      ("valid: query_args", `Quick, test_query_args);
      ("values: query_args", `Quick, test_query_args_values);
    ]
end

(* Error tests *)
module Error_tests = struct
  open Jmap.Proto

  let test_method_error () =
    test_decode_success "method_error" Error.Method_error.jsont
      "error/valid/method_error.json" ()

  let test_set_error () =
    test_decode_success "set_error" Error.Set_error.jsont
      "error/valid/set_error.json" ()

  let test_request_error () =
    test_decode_success "request_error" Error.Request_error.jsont
      "error/valid/request_error.json" ()

  let method_error_type_testable =
    Alcotest.testable
      (fun fmt t ->
        Format.pp_print_string fmt (Error.Method_error.type_to_string t))
      ( = )

  let test_method_error_values () =
    let json = read_file "error/valid/method_error.json" in
    match decode Error.Method_error.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok err ->
        Alcotest.(check method_error_type_testable)
          "type" `Unknown_method err.Error.Method_error.type_

  (* Additional error type tests *)
  let test_set_error_forbidden () =
    test_decode_success "set_error_forbidden" Error.Set_error.jsont
      "error/valid/set_error_forbidden.json" ()

  let test_set_error_not_found () =
    test_decode_success "set_error_not_found" Error.Set_error.jsont
      "error/valid/set_error_not_found.json" ()

  let test_set_error_invalid_properties () =
    test_decode_success "set_error_invalid_properties" Error.Set_error.jsont
      "error/valid/set_error_invalid_properties.json" ()

  let test_set_error_singleton () =
    test_decode_success "set_error_singleton" Error.Set_error.jsont
      "error/valid/set_error_singleton.json" ()

  let test_set_error_over_quota () =
    test_decode_success "set_error_over_quota" Error.Set_error.jsont
      "error/valid/set_error_over_quota.json" ()

  let test_method_error_invalid_arguments () =
    test_decode_success "method_error_invalid_arguments"
      Error.Method_error.jsont "error/valid/method_error_invalid_arguments.json"
      ()

  let test_method_error_server_fail () =
    test_decode_success "method_error_server_fail" Error.Method_error.jsont
      "error/valid/method_error_server_fail.json" ()

  let test_method_error_account_not_found () =
    test_decode_success "method_error_account_not_found"
      Error.Method_error.jsont "error/valid/method_error_account_not_found.json"
      ()

  let test_method_error_forbidden () =
    test_decode_success "method_error_forbidden" Error.Method_error.jsont
      "error/valid/method_error_forbidden.json" ()

  let test_method_error_account_read_only () =
    test_decode_success "method_error_account_read_only"
      Error.Method_error.jsont "error/valid/method_error_account_read_only.json"
      ()

  let test_request_error_not_json () =
    test_decode_success "request_error_not_json" Error.Request_error.jsont
      "error/valid/request_error_not_json.json" ()

  let test_request_error_limit () =
    test_decode_success "request_error_limit" Error.Request_error.jsont
      "error/valid/request_error_limit.json" ()

  let set_error_type_testable =
    Alcotest.testable
      (fun fmt t ->
        Format.pp_print_string fmt (Error.Set_error.type_to_string t))
      ( = )

  let test_set_error_types () =
    let json = read_file "error/valid/set_error_invalid_properties.json" in
    match decode Error.Set_error.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok err -> (
        Alcotest.(check set_error_type_testable)
          "type" `Invalid_properties err.Error.Set_error.type_;
        match err.Error.Set_error.properties with
        | None -> Alcotest.fail "expected properties"
        | Some props ->
            Alcotest.(check int) "properties count" 2 (List.length props))

  let tests =
    [
      ("valid: method_error", `Quick, test_method_error);
      ("valid: set_error", `Quick, test_set_error);
      ("valid: request_error", `Quick, test_request_error);
      ("valid: set_error forbidden", `Quick, test_set_error_forbidden);
      ("valid: set_error notFound", `Quick, test_set_error_not_found);
      ( "valid: set_error invalidProperties",
        `Quick,
        test_set_error_invalid_properties );
      ("valid: set_error singleton", `Quick, test_set_error_singleton);
      ("valid: set_error overQuota", `Quick, test_set_error_over_quota);
      ( "valid: method_error invalidArguments",
        `Quick,
        test_method_error_invalid_arguments );
      ("valid: method_error serverFail", `Quick, test_method_error_server_fail);
      ( "valid: method_error accountNotFound",
        `Quick,
        test_method_error_account_not_found );
      ("valid: method_error forbidden", `Quick, test_method_error_forbidden);
      ( "valid: method_error accountReadOnly",
        `Quick,
        test_method_error_account_read_only );
      ("valid: request_error notJSON", `Quick, test_request_error_not_json);
      ("valid: request_error limit", `Quick, test_request_error_limit);
      ("values: method_error", `Quick, test_method_error_values);
      ("values: set_error types", `Quick, test_set_error_types);
    ]
end

(* Mailbox tests *)
module Mailbox_tests = struct
  open Jmap.Proto

  let all_roles_roundtrip_jsont = Jsont.list Mailbox.jsont

  let test_all_roles_roundtrip () =
    test_roundtrip "all roles roundtrip" all_roles_roundtrip_jsont
      "mail/mailbox/valid/all_roles.json" ()

  let role_testable =
    Alcotest.testable
      (fun fmt t -> Format.pp_print_string fmt (Mailbox.role_to_string t))
      ( = )

  let test_simple () =
    test_decode_success "simple" Mailbox.jsont "mail/mailbox/valid/simple.json"
      ()

  let test_nested () =
    test_decode_success "nested" Mailbox.jsont "mail/mailbox/valid/nested.json"
      ()

  let test_values () =
    let json = read_file "mail/mailbox/valid/simple.json" in
    match decode Mailbox.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok mb ->
        Alcotest.(check string) "id" "mb1" (get_id mb.Mailbox.id);
        Alcotest.(check string) "name" "Inbox" (get_string mb.Mailbox.name);
        Alcotest.(check (option role_testable))
          "role" (Some `Inbox) mb.Mailbox.role;
        Alcotest.(check int64)
          "totalEmails" 150L
          (get_int64 mb.Mailbox.total_emails);
        Alcotest.(check int64)
          "unreadEmails" 5L
          (get_int64 mb.Mailbox.unread_emails)

  let test_roundtrip () =
    test_roundtrip "simple roundtrip" Mailbox.jsont
      "mail/mailbox/valid/simple.json" ()

  let test_with_all_roles () =
    test_decode_success "with all roles" Mailbox.jsont
      "mail/mailbox/valid/with_all_roles.json" ()

  let test_all_rights_false () =
    test_decode_success "all rights false" Mailbox.jsont
      "mail/mailbox/edge/all_rights_false.json" ()

  let test_roles_values () =
    let json = read_file "mail/mailbox/valid/with_all_roles.json" in
    match decode Mailbox.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok mb ->
        Alcotest.(check (option role_testable))
          "role" (Some `Archive) mb.Mailbox.role;
        Alcotest.(check int64)
          "totalEmails" 1000L
          (get_int64 mb.Mailbox.total_emails)

  (* [mail/mailbox/valid/all_roles.json] is an array of one mailbox per role. *)
  let all_roles_jsont = all_roles_roundtrip_jsont

  let test_all_roles () =
    test_decode_success "all roles" all_roles_jsont
      "mail/mailbox/valid/all_roles.json" ()

  let test_all_roles_values () =
    let json = read_file "mail/mailbox/valid/all_roles.json" in
    match decode all_roles_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok mbs ->
        let roles = List.map (fun (mb : Mailbox.t) -> mb.Mailbox.role) mbs in
        Alcotest.(check (list (option role_testable)))
          "roles"
          [
            Some `Inbox;
            Some `Drafts;
            Some `Sent;
            Some `Junk;
            Some `Trash;
            Some `Archive;
            Some `All;
            Some `Important;
            Some `Scheduled;
            (* RFC 8621 Section 2 has no "subscribed" role. *)
            Some (`Other "subscribed");
          ]
          roles

  let tests =
    [
      ("valid: simple", `Quick, test_simple);
      ("valid: nested", `Quick, test_nested);
      ("valid: with all roles", `Quick, test_with_all_roles);
      ("edge: all rights false", `Quick, test_all_rights_false);
      ("values: simple", `Quick, test_values);
      ("values: roles", `Quick, test_roles_values);
      ("roundtrip: simple", `Quick, test_roundtrip);
      ("valid: all roles", `Quick, test_all_roles);
      ("values: all roles", `Quick, test_all_roles_values);
      ("roundtrip: all roles", `Quick, test_all_roles_roundtrip);
    ]
end

(* Header property form allow-lists (RFC 8621 Sections 4.1.2.2-4.1.2.7) *)
module Header_form_tests = struct
  open Jmap.Proto

  let address_headers =
    [
      "From";
      "Sender";
      "Reply-To";
      "To";
      "Cc";
      "Bcc";
      "Resent-From";
      "Resent-Sender";
      "Resent-Reply-To";
      "Resent-To";
      "Resent-Cc";
      "Resent-Bcc";
    ]

  let message_id_headers =
    [ "Message-ID"; "In-Reply-To"; "References"; "Resent-Message-ID" ]

  let date_headers = [ "Date"; "Resent-Date" ]

  let url_headers =
    [
      "List-Help";
      "List-Unsubscribe";
      "List-Subscribe";
      "List-Post";
      "List-Owner";
      "List-Archive";
    ]

  let text_headers = [ "Subject"; "Comments"; "Keywords" ]

  (* RFC 5322 Section 3.6.7 trace fields.  RFC 8621 Section 4.1.2 gives them
     no parsed form, so only the raw form may be asked for. *)
  let trace_headers = [ "Received"; "Return-Path" ]

  let standard_headers =
    address_headers @ message_id_headers @ date_headers @ url_headers
    @ text_headers @ trace_headers

  let forms =
    [
      ("asText", text_headers);
      ("asAddresses", address_headers);
      ("asGroupedAddresses", address_headers);
      ("asMessageIds", message_id_headers);
      ("asDate", date_headers);
      ("asURLs", url_headers);
    ]

  let parses s = Email_header.header_property_of_string s <> None

  (* List-Id is defined in RFC 2919, which RFC 8621 Section 4.1.2 does not
     restrict, so every form is open to it. *)
  let custom_headers = [ "List-Id"; "X-Spam-Score" ]

  (* The full form x standard-header matrix, so that neither a widening nor a
     narrowing of the allow-lists can pass unnoticed. *)
  let test_matrix () =
    List.iter
      (fun (form, allowed) ->
        List.iter
          (fun name ->
            let s = Printf.sprintf "header:%s:%s" name form in
            Alcotest.(check bool) s (List.mem name allowed) (parses s))
          standard_headers;
        List.iter
          (fun name ->
            let s = Printf.sprintf "header:%s:%s" name form in
            Alcotest.(check bool) s true (parses s))
          custom_headers)
      forms

  (* Section 4.1.2.1: the raw form is defined for every header field. *)
  let test_raw_allows_every_header () =
    List.iter
      (fun name ->
        let s = Printf.sprintf "header:%s" name in
        Alcotest.(check bool) s true (parses s);
        let s = Printf.sprintf "header:%s:all" name in
        Alcotest.(check bool) s true (parses s))
      standard_headers

  (* A field not defined in RFC 5322 or RFC 2369 accepts every form. *)
  let test_custom_header_allows_every_form () =
    List.iter
      (fun (form, _) ->
        let s = Printf.sprintf "header:X-Custom:%s" form in
        Alcotest.(check bool) s true (parses s))
      forms

  let tests =
    [
      ("matrix: form x standard header", `Quick, test_matrix);
      ("raw: every header", `Quick, test_raw_allows_every_header);
      ("custom: every form", `Quick, test_custom_header_allows_every_form);
    ]
end

(* Email tests *)
module Email_tests = struct
  open Jmap.Proto

  let test_minimal () =
    test_decode_success "minimal" Email.jsont "mail/email/valid/minimal.json" ()

  let test_full () =
    test_decode_success "full" Email.jsont "mail/email/valid/full.json" ()

  let test_with_headers () =
    test_decode_success "with_headers" Email.jsont
      "mail/email/valid/with_headers.json" ()

  let test_minimal_values () =
    let json = read_file "mail/email/valid/minimal.json" in
    match decode Email.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok email ->
        Alcotest.(check string) "id" "e1" (get_id email.Email.id);
        Alcotest.(check string) "blobId" "blob1" (get_id email.Email.blob_id);
        Alcotest.(check int64) "size" 1024L (get_int64 email.Email.size)

  let test_full_values () =
    let json = read_file "mail/email/valid/full.json" in
    match decode Email.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok email -> (
        Alcotest.(check (option string))
          "subject" (Some "Re: Important meeting") email.Email.subject;
        Alcotest.(check bool)
          "hasAttachment" true
          (get_bool email.Email.has_attachment);
        (* Check from address *)
        match email.Email.from with
        | None -> Alcotest.fail "expected from address"
        | Some addrs ->
            Alcotest.(check int) "from count" 1 (List.length addrs);
            let addr = List.hd addrs in
            Alcotest.(check (option string))
              "from name" (Some "Alice Smith") addr.Email_address.name;
            Alcotest.(check string)
              "from email" "alice@example.com" addr.Email_address.email)

  let test_with_keywords () =
    test_decode_success "with keywords" Email.jsont
      "mail/email/valid/with_keywords.json" ()

  let test_multiple_mailboxes () =
    test_decode_success "multiple mailboxes" Email.jsont
      "mail/email/valid/multiple_mailboxes.json" ()

  let test_draft_email () =
    test_decode_success "draft email" Email.jsont
      "mail/email/valid/draft_email.json" ()

  let test_with_all_system_keywords () =
    test_decode_success "all system keywords" Email.jsont
      "mail/email/valid/with_all_system_keywords.json" ()

  let test_empty_keywords () =
    test_decode_success "empty keywords" Email.jsont
      "mail/email/edge/empty_keywords.json" ()

  let test_with_message_ids () =
    test_decode_success "with message ids" Email.jsont
      "mail/email/valid/with_message_ids.json" ()

  let test_keywords_values () =
    let json = read_file "mail/email/valid/with_keywords.json" in
    match decode Email.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok email ->
        let keywords = Option.value ~default:[] email.Email.keywords in
        Alcotest.(check int) "keywords count" 3 (List.length keywords);
        let has k = List.exists (fun (k', _) -> Keyword.equal k k') keywords in
        Alcotest.(check bool) "$seen present" true (has `Seen);
        Alcotest.(check bool) "$flagged present" true (has `Flagged)

  let test_mailbox_ids_values () =
    let json = read_file "mail/email/valid/multiple_mailboxes.json" in
    match decode Email.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok email ->
        let mailbox_ids = Option.value ~default:[] email.Email.mailbox_ids in
        Alcotest.(check int) "mailboxIds count" 3 (List.length mailbox_ids)

  let tests =
    [
      ("valid: minimal", `Quick, test_minimal);
      ("valid: full", `Quick, test_full);
      ("valid: with_headers", `Quick, test_with_headers);
      ("valid: with keywords", `Quick, test_with_keywords);
      ("valid: multiple mailboxes", `Quick, test_multiple_mailboxes);
      ("valid: draft email", `Quick, test_draft_email);
      ("valid: all system keywords", `Quick, test_with_all_system_keywords);
      ("valid: with message ids", `Quick, test_with_message_ids);
      ("edge: empty keywords", `Quick, test_empty_keywords);
      ("values: minimal", `Quick, test_minimal_values);
      ("values: full", `Quick, test_full_values);
      ("values: keywords", `Quick, test_keywords_values);
      ("values: mailboxIds", `Quick, test_mailbox_ids_values);
    ]
end

(* Thread tests *)
module Thread_tests = struct
  open Jmap.Proto

  let test_simple () =
    test_decode_success "simple" Thread.jsont "mail/thread/valid/simple.json" ()

  let test_conversation () =
    test_decode_success "conversation" Thread.jsont
      "mail/thread/valid/conversation.json" ()

  let test_values () =
    let json = read_file "mail/thread/valid/conversation.json" in
    match decode Thread.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok thread ->
        Alcotest.(check string) "id" "t2" (get_id thread.Thread.id);
        Alcotest.(check int)
          "emailIds count" 5
          (List.length (Option.value ~default:[] thread.Thread.email_ids))

  let tests =
    [
      ("valid: simple", `Quick, test_simple);
      ("valid: conversation", `Quick, test_conversation);
      ("values: conversation", `Quick, test_values);
    ]
end

(* Identity tests *)
module Identity_tests = struct
  open Jmap.Proto

  let test_simple () =
    test_decode_success "simple" Identity.jsont
      "mail/identity/valid/simple.json" ()

  let test_values () =
    let json = read_file "mail/identity/valid/simple.json" in
    match decode Identity.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok ident ->
        Alcotest.(check string)
          "name" "Work Identity"
          (get_string ident.Identity.name);
        Alcotest.(check string)
          "email" "john.doe@company.com"
          (get_string ident.Identity.email);
        Alcotest.(check bool)
          "mayDelete" true
          (get_bool ident.Identity.may_delete)

  let tests =
    [
      ("valid: simple", `Quick, test_simple);
      ("values: simple", `Quick, test_values);
    ]
end

(* Email address tests *)
module Email_address_tests = struct
  open Jmap.Proto

  let test_full () =
    test_decode_success "full" Email_address.jsont
      "mail/email_address/valid/full.json" ()

  let test_email_only () =
    test_decode_success "email_only" Email_address.jsont
      "mail/email_address/valid/email_only.json" ()

  let test_full_values () =
    let json = read_file "mail/email_address/valid/full.json" in
    match decode Email_address.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok addr ->
        Alcotest.(check (option string))
          "name" (Some "John Doe") addr.Email_address.name;
        Alcotest.(check string)
          "email" "john.doe@example.com" addr.Email_address.email

  let test_email_only_values () =
    let json = read_file "mail/email_address/valid/email_only.json" in
    match decode Email_address.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok addr ->
        Alcotest.(check (option string)) "name" None addr.Email_address.name;
        Alcotest.(check string)
          "email" "anonymous@example.com" addr.Email_address.email

  let tests =
    [
      ("valid: full", `Quick, test_full);
      ("valid: email_only", `Quick, test_email_only);
      ("values: full", `Quick, test_full_values);
      ("values: email_only", `Quick, test_email_only_values);
    ]
end

(* Vacation tests *)
module Vacation_tests = struct
  open Jmap.Proto

  let test_enabled () =
    test_decode_success "enabled" Vacation.jsont
      "mail/vacation/valid/enabled.json" ()

  let test_disabled () =
    test_decode_success "disabled" Vacation.jsont
      "mail/vacation/valid/disabled.json" ()

  let test_enabled_values () =
    let json = read_file "mail/vacation/valid/enabled.json" in
    match decode Vacation.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok vac ->
        Alcotest.(check (option bool))
          "isEnabled" (Some true) vac.Vacation.is_enabled;
        Alcotest.(check (option string))
          "subject" (Some "Out of Office") vac.Vacation.subject

  let test_disabled_values () =
    let json = read_file "mail/vacation/valid/disabled.json" in
    match decode Vacation.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok vac ->
        Alcotest.(check (option bool))
          "isEnabled" (Some false) vac.Vacation.is_enabled;
        Alcotest.(check (option string)) "subject" None vac.Vacation.subject

  let tests =
    [
      ("valid: enabled", `Quick, test_enabled);
      ("valid: disabled", `Quick, test_disabled);
      ("values: enabled", `Quick, test_enabled_values);
      ("values: disabled", `Quick, test_disabled_values);
    ]
end

(* Comparator tests *)
module Comparator_tests = struct
  open Jmap.Proto

  let test_minimal () =
    test_decode_success "minimal" Filter.comparator_jsont
      "filter/valid/comparator_minimal.json" ()

  let test_descending () =
    test_decode_success "descending" Filter.comparator_jsont
      "filter/valid/comparator_descending.json" ()

  let test_with_collation () =
    test_decode_success "with collation" Filter.comparator_jsont
      "filter/valid/comparator_with_collation.json" ()

  let test_minimal_values () =
    let json = read_file "filter/valid/comparator_minimal.json" in
    match decode Filter.comparator_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok comp ->
        Alcotest.(check string) "property" "size" comp.Filter.property;
        Alcotest.(check bool) "isAscending" true comp.Filter.is_ascending;
        Alcotest.(check (option string)) "collation" None comp.Filter.collation

  let test_collation_values () =
    let json = read_file "filter/valid/comparator_with_collation.json" in
    match decode Filter.comparator_jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok comp ->
        Alcotest.(check string) "property" "subject" comp.Filter.property;
        Alcotest.(check (option string))
          "collation" (Some "i;unicode-casemap") comp.Filter.collation

  let tests =
    [
      ("valid: minimal", `Quick, test_minimal);
      ("valid: descending", `Quick, test_descending);
      ("valid: with collation", `Quick, test_with_collation);
      ("values: minimal", `Quick, test_minimal_values);
      ("values: with collation", `Quick, test_collation_values);
    ]
end

(* EmailBody tests *)
module EmailBody_tests = struct
  open Jmap.Proto

  let test_text_part () =
    test_decode_success "text part" Email_body.Part.jsont
      "mail/email_body/valid/text_part.json" ()

  let test_multipart () =
    test_decode_success "multipart" Email_body.Part.jsont
      "mail/email_body/valid/multipart.json" ()

  let test_multipart_mixed () =
    test_decode_success "multipart mixed" Email_body.Part.jsont
      "mail/email_body/valid/multipart_mixed.json" ()

  let test_with_inline_image () =
    test_decode_success "with inline image" Email_body.Part.jsont
      "mail/email_body/valid/with_inline_image.json" ()

  let test_with_language () =
    test_decode_success "with language" Email_body.Part.jsont
      "mail/email_body/valid/with_language.json" ()

  let test_deep_nesting () =
    test_decode_success "deep nesting" Email_body.Part.jsont
      "mail/email_body/edge/deep_nesting.json" ()

  let test_multipart_values () =
    let json = read_file "mail/email_body/valid/multipart.json" in
    match decode Email_body.Part.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok part -> (
        Alcotest.(check (option string))
          "partId" (Some "0") part.Email_body.Part.part_id;
        Alcotest.(check (option string))
          "type" (Some "multipart/alternative") part.Email_body.Part.type_;
        match part.Email_body.Part.sub_parts with
        | None -> Alcotest.fail "expected sub_parts"
        | Some subs ->
            Alcotest.(check int) "sub_parts count" 2 (List.length subs))

  let tests =
    [
      ("valid: text part", `Quick, test_text_part);
      ("valid: multipart", `Quick, test_multipart);
      ("valid: multipart mixed", `Quick, test_multipart_mixed);
      ("valid: with inline image", `Quick, test_with_inline_image);
      ("valid: with language", `Quick, test_with_language);
      ("edge: deep nesting", `Quick, test_deep_nesting);
      ("values: multipart", `Quick, test_multipart_values);
    ]
end

(* EmailSubmission tests *)
module EmailSubmission_tests = struct
  open Jmap.Proto

  let test_simple () =
    test_decode_success "simple" Submission.jsont
      "mail/submission/valid/simple.json" ()

  let test_with_envelope () =
    test_decode_success "with envelope" Submission.jsont
      "mail/submission/valid/with_envelope.json" ()

  let test_final_status () =
    test_decode_success "final status" Submission.jsont
      "mail/submission/valid/final_status.json" ()

  let test_simple_values () =
    let json = read_file "mail/submission/valid/simple.json" in
    match decode Submission.jsont json with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok sub -> (
        Alcotest.(check string) "id" "sub1" (get_id sub.Submission.id);
        (* Check undoStatus is Pending *)
        match sub.Submission.undo_status with
        | Some `Pending -> ()
        | _ -> Alcotest.fail "expected undoStatus to be pending")

  let tests =
    [
      ("valid: simple", `Quick, test_simple);
      ("valid: with envelope", `Quick, test_with_envelope);
      ("valid: final status", `Quick, test_final_status);
      ("values: simple", `Quick, test_simple_values);
    ]
end

(* URI template expansion (RFC 6570 level 1) *)
module Template_tests = struct
  open Jmap.Proto

  let expanded = function
    | Ok value -> value
    | Error error ->
        Alcotest.failf "expansion failed: %a" Httpz_uri.Template.pp_error error

  let vars =
    [
      ("accountId", "a1");
      ("blobId", "G1");
      ("type", "image/png");
      ("name", "a b.png");
    ]

  let test_expand () =
    Alcotest.(check string)
      "download url" "/jmap/download/a1/G1/a%20b.png?type=image%2Fpng"
      (expanded
         (Template.expand ~vars
            "/jmap/download/{accountId}/{blobId}/{name}?type={type}"))

  let test_unknown_variable () =
    Alcotest.(check string)
      "unknown expands to nothing" "/x//y"
      (expanded (Template.expand ~vars "/x/{nosuch}/y"))

  let test_unclosed_brace () =
    Alcotest.(check bool)
      "strict parse error" true
      (Result.is_error (Template.expand ~vars "/x/{accountId"))

  let test_httpz_features () =
    Alcotest.(check string)
      "reserved expansion" "/x/image/png"
      (expanded (Template.expand ~vars "/x/{+type}"));
    Alcotest.(check string)
      "literal encoded brace stays encoded" "/x/%7B/a1"
      (expanded (Template.expand ~vars "/x/%7B/{accountId}"))

  let test_blob_download_url () =
    let d =
      {
        Blob.account_id = Id.of_string_exn "a1";
        blob_id = Id.of_string_exn "G1";
        type_ = "image/png";
        name = "a b.png";
      }
    in
    let source = "https://x/dl/{accountId}/{blobId}/{type}/{name}" in
    let expected = "https://x/dl/a1/G1/image%2Fpng/a%20b.png" in
    Alcotest.(check string)
      "expanded" expected
      (expanded (Blob.expand_download_url ~template:source d));
    let template = Result.get_ok (Httpz_uri.Template.of_string source) in
    match Blob.expand_download_template ~template d with
    | Ok actual -> Alcotest.(check string) "typed expansion" expected actual
    | Error error ->
        Alcotest.failf "typed expansion failed: %a" Httpz_uri.Template.pp_error
          error

  let tests =
    [
      ("expand", `Quick, test_expand);
      ("unknown variable", `Quick, test_unknown_variable);
      ("unclosed brace", `Quick, test_unclosed_brace);
      ("Httpz features", `Quick, test_httpz_features);
      ("download url", `Quick, test_blob_download_url);
    ]
end

(* Object-as-map codecs *)
module Json_map_tests = struct
  open Jmap.Proto

  let test_decode_sorted () =
    match decode Json_map.string_to_bool {|{"b":true,"a":false}|} with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok l ->
        Alcotest.(check (list string))
          "keys sorted" [ "a"; "b" ] (List.map fst l)

  let test_encode_duplicate_key () =
    match encode Json_map.string_to_bool [ ("a", true); ("a", true) ] with
    | Ok s -> Alcotest.failf "expected a duplicate key error, got %s" s
    | Error _ -> ()

  let test_creation_reference_key () =
    match decode Json_map.id_to_bool {|{"#draft1":true}|} with
    | Error e -> Alcotest.failf "decode failed: %s" (Jsont.Error.to_string e)
    | Ok [ (id, true) ] ->
        Alcotest.(check (option string))
          "creation id" (Some "draft1") (Id.to_creation_id id)
    | Ok _ -> Alcotest.fail "expected one entry"

  let tests =
    [
      ("decode sorts keys", `Quick, test_decode_sorted);
      ("encode rejects duplicates", `Quick, test_encode_duplicate_key);
      ("creation reference key", `Quick, test_creation_reference_key);
    ]
end

module Json_tests = struct
  open Jmap.Proto

  let expect_rejected source =
    match Json.decode Jsont.json source with
    | Ok _ -> Alcotest.failf "expected rejection for %s" source
    | Error _ -> ()

  let test_duplicate_members () =
    expect_rejected {|{"a":1,"a":2}|};
    expect_rejected {|{"a":1,"\u0061":2}|};
    expect_rejected {|{"outer":{"a":1,"a":2}}|}

  let test_distinct_objects () =
    match Json.decode Jsont.json {|[{"a":1},{"a":2}]|} with
    | Ok _ -> ()
    | Error error ->
        Alcotest.failf "valid members rejected: %s"
          (Jsont.Error.to_string error)

  let test_numbers () =
    List.iter
      (fun source ->
        match Json.decode Jsont.json source with
        | Ok _ -> ()
        | Error error ->
            Alcotest.failf "valid JSON Number %s rejected: %s" source
              (Jsont.Error.to_string error))
      [ "1.5"; "1.00000000000000001"; "9007199254740992" ];
    expect_rejected "1e999";
    (match Json.decode Int53.Signed.jsont "9007199254740992" with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "Int53 accepted 2^53");
    match Json.decode Int53.Signed.jsont "1.0e3" with
    | Ok value -> Alcotest.(check int64) "integral exponent" 1000L value
    | Error error ->
        Alcotest.failf "integral exponent rejected: %s"
          (Jsont.Error.to_string error)

  let test_unicode () =
    expect_rejected {|"\uFDD0"|};
    expect_rejected {|{"\uFFFF":true}|};
    expect_rejected {|"\uD800"|};
    expect_rejected ("\"" ^ "bad\255utf8" ^ "\"")

  let test_encode_restrictions () =
    let rejected value =
      match Json.encode Jsont.json value with
      | Error _ -> ()
      | Ok encoded ->
          Alcotest.failf "expected restricted JSON to be rejected, got %s"
            encoded
    in
    rejected (Jsont.Json.number Float.infinity);
    rejected (Jsont.Json.string "noncharacter:\239\183\144");
    let member value =
      Jsont.Json.mem (Jsont.Json.name "duplicate") (Jsont.Json.number value)
    in
    rejected (Jsont.Json.object' [ member 1.; member 2. ]);
    match Json.encode Jsont.json (Jsont.Json.number 1.5) with
    | Ok "1.5" -> ()
    | Ok value -> Alcotest.failf "unexpected fractional encoding: %s" value
    | Error error ->
        Alcotest.failf "fractional Number rejected: %s"
          (Jsont.Error.to_string error)

  let tests =
    [
      ("duplicate members", `Quick, test_duplicate_members);
      ("members in distinct objects", `Quick, test_distinct_objects);
      ("numbers", `Quick, test_numbers);
      ("Unicode strings", `Quick, test_unicode);
      ("encode restrictions", `Quick, test_encode_restrictions);
    ]
end

(* Run all tests *)
let () =
  Alcotest.run "JMAP Proto Codecs"
    [
      ("Id", Id_tests.tests);
      ("Template", Template_tests.tests);
      ("Json", Json_tests.tests);
      ("Json_map", Json_map_tests.tests);
      ("Int53", Int53_tests.tests);
      ("Date", Date_tests.tests);
      ("Session", Session_tests.tests);
      ("Request", Request_tests.tests);
      ("Response", Response_tests.tests);
      ("Invocation", Invocation_tests.tests);
      ("Capability", Capability_tests.tests);
      ("Method", Method_tests.tests);
      ("Error", Error_tests.tests);
      ("Comparator", Comparator_tests.tests);
      ("Mailbox", Mailbox_tests.tests);
      ("Header_forms", Header_form_tests.tests);
      ("Email", Email_tests.tests);
      ("EmailBody", EmailBody_tests.tests);
      ("Thread", Thread_tests.tests);
      ("Identity", Identity_tests.tests);
      ("Email_address", Email_address_tests.tests);
      ("EmailSubmission", EmailSubmission_tests.tests);
      ("Vacation", Vacation_tests.tests);
    ]
