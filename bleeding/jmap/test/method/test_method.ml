(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Codec tests for the RFC 8620 Section 5-7 method, filter, blob, push and
    error types.

    The bulk of these exercise members whose specification type is [T|null]:
    servers do send an explicit JSON [null] (the RFC's own examples do), which
    an "optional member" codec accepts only in its absent form. *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let fixture name = read_file ("fixtures/" ^ name)
let decode jsont json_str = Jsont_bytesrw.decode_string' jsont json_str
let encode jsont value = Jsont_bytesrw.encode_string' jsont value

let decode_exn name jsont json =
  match decode jsont json with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "%s: decode failed: %s" name (Jsont.Error.to_string e)

let encode_exn name jsont v =
  match encode jsont v with
  | Ok s -> s
  | Error e ->
      Alcotest.failf "%s: encode failed: %s" name (Jsont.Error.to_string e)

let decode_fixture name jsont path = decode_exn name jsont (fixture path)

let expect_decode_failure name jsont json =
  match decode jsont json with
  | Ok _ -> Alcotest.failf "%s: expected a decode failure" name
  | Error _ -> ()

(* Decode, encode and decode again: catches members that decode but cannot be
   re-encoded, and members dropped on the way through. *)
let roundtrip name jsont json =
  let v = decode_exn name jsont json in
  let s = encode_exn name jsont v in
  ignore (decode_exn (name ^ " (re-decode)") jsont s);
  s

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0

open Jmap.Proto

let id_v = Id.of_string_exn

(* Shared fixtures of test/proto, referenced from here as well. *)
let proto_method path = read_file ("../proto/method/" ^ path)
let proto_filter path = read_file ("../proto/filter/" ^ path)

(* A /get, /set or /copy over opaque objects: the generic codecs are what is
   under test, not the object type. *)
let json_set_args = Method.set_args_jsont Jsont.json
let json_set_response = Method.set_response_jsont Jsont.json
let json_copy_args = Method.copy_args_jsont Jsont.json
let json_copy_response = Method.copy_response_jsont Jsont.json

(* RFC 8620 Section 5.5 leaves the shape of a FilterCondition to the data
   type being queried; the shared fixtures are Email conditions, kept here as
   generic JSON objects. *)
let json_condition =
  Jsont.Object.map ~kind:"FilterCondition" Fun.id
  |> Jsont.Object.keep_unknown Unknown.mems ~enc:Fun.id
  |> Jsont.Object.finish

let json_query_args = Method.query_args_jsont json_condition
let json_query_changes_args = Method.query_changes_args_jsont json_condition
let json_filter = Filter.filter_jsont json_condition

module Null_tests = struct
  let test_get_args_null_ids () =
    let a =
      decode_fixture "get_args" Method.get_args_jsont "get_args_null_ids.json"
    in
    Alcotest.(check bool) "ids is None" true (a.Method.ids = None);
    Alcotest.(check bool) "properties is None" true (a.Method.properties = None)

  (* RFC 8620 Section 5.1: null means "fetch all", which is distinct from an
     absent member for a reader, so it must be encoded explicitly. *)
  let test_get_args_encodes_null_ids () =
    let a = Method.get_args ~account_id:(id_v "acc1") () in
    let s = encode_exn "get_args" Method.get_args_jsont a in
    Alcotest.(check bool)
      "encodes \"ids\":null" true
      (contains ~needle:"\"ids\":null" s)

  let test_changes_args_null_max_changes () =
    let a =
      decode_fixture "changes_args" Method.changes_args_jsont
        "changes_args_null_max.json"
    in
    Alcotest.(check bool) "maxChanges is None" true (a.Method.max_changes = None)

  let test_set_args_nulls () =
    let a = decode_fixture "set_args" json_set_args "set_args_nulls.json" in
    Alcotest.(check bool) "ifInState" true (a.Method.if_in_state = None);
    Alcotest.(check bool) "create" true (a.Method.create = None);
    Alcotest.(check bool) "update" true (a.Method.update = None);
    Alcotest.(check bool) "destroy" true (a.Method.destroy = None)

  let test_set_response_null_old_state () =
    let r =
      decode_fixture "set_response" json_set_response
        "set_response_null_state.json"
    in
    Alcotest.(check bool) "oldState" true (r.Method.old_state = None);
    Alcotest.(check bool) "created" true (r.Method.created = None);
    Alcotest.(check bool) "notCreated" true (r.Method.not_created = None);
    Alcotest.(check string) "newState" "state456" r.Method.new_state

  let test_copy_args_nulls () =
    let a = decode_fixture "copy_args" json_copy_args "copy_args_nulls.json" in
    Alcotest.(check bool) "ifFromInState" true (a.Method.if_from_in_state = None);
    Alcotest.(check bool) "ifInState" true (a.Method.if_in_state = None);
    Alcotest.(check bool)
      "destroyFromIfInState" true
      (a.Method.destroy_from_if_in_state = None);
    Alcotest.(check int) "create" 1 (List.length a.Method.create)

  let test_copy_response_nulls () =
    let r =
      decode_fixture "copy_response" json_copy_response
        "copy_response_nulls.json"
    in
    Alcotest.(check bool) "oldState" true (r.Method.old_state = None);
    Alcotest.(check bool) "created" true (r.Method.created = None);
    Alcotest.(check bool) "notCreated" true (r.Method.not_created = None)

  let test_query_args_nulls () =
    let a =
      decode_fixture "query_args" json_query_args "query_args_nulls.json"
    in
    Alcotest.(check bool) "filter" true (a.Method.filter = None);
    Alcotest.(check bool) "sort" true (a.Method.sort = None);
    Alcotest.(check bool) "anchor" true (a.Method.anchor = None);
    Alcotest.(check bool) "limit" true (a.Method.limit = None)

  let test_query_changes_args_nulls () =
    let a =
      decode_fixture "query_changes_args" json_query_changes_args
        "query_changes_args_nulls.json"
    in
    Alcotest.(check bool) "filter" true (a.Method.filter = None);
    Alcotest.(check bool) "sort" true (a.Method.sort = None);
    Alcotest.(check bool) "maxChanges" true (a.Method.max_changes = None);
    Alcotest.(check bool) "upToId" true (a.Method.up_to_id = None)

  let test_blob_copy_response_nulls () =
    let r =
      decode_fixture "Blob/copy" Blob.copy_response_jsont
        "blob_copy_response_nulls.json"
    in
    Alcotest.(check bool) "copied" true (r.Blob.copied = None);
    Alcotest.(check bool) "notCopied" true (r.Blob.not_copied = None)

  let test_blob_copy_response_values () =
    let r =
      decode_fixture "Blob/copy" Blob.copy_response_jsont
        "blob_copy_response.json"
    in
    (match r.Blob.copied with
    | Some [ (from_id, to_id) ] ->
        Alcotest.(check string) "from" "b1" (Id.to_string from_id);
        Alcotest.(check string) "to" "b1-copy" (Id.to_string to_id)
    | _ -> Alcotest.fail "expected one copied blob");
    match r.Blob.not_copied with
    | Some [ (_, err) ] ->
        Alcotest.(check bool)
          "notFound" true
          (err.Error.Set_error.type_ = `Not_found)
    | _ -> Alcotest.fail "expected one notCopied blob"

  let test_set_error_null_description () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_null_description.json"
    in
    Alcotest.(check bool) "type" true (e.Error.Set_error.type_ = `Forbidden);
    Alcotest.(check bool)
      "description" true
      (e.Error.Set_error.description = None)

  let tests =
    [
      ("get args: ids null", `Quick, test_get_args_null_ids);
      ("get args: None encodes as null", `Quick, test_get_args_encodes_null_ids);
      ( "changes args: maxChanges null",
        `Quick,
        test_changes_args_null_max_changes );
      ("set args: all null", `Quick, test_set_args_nulls);
      ("set response: oldState null", `Quick, test_set_response_null_old_state);
      ("copy args: state args null", `Quick, test_copy_args_nulls);
      ("copy response: created null", `Quick, test_copy_response_nulls);
      ("query args: all null", `Quick, test_query_args_nulls);
      ("queryChanges args: all null", `Quick, test_query_changes_args_nulls);
      ("Blob/copy response: copied null", `Quick, test_blob_copy_response_nulls);
      ("Blob/copy response: values", `Quick, test_blob_copy_response_values);
      ("SetError: description null", `Quick, test_set_error_null_description);
    ]
end

module Query_tests = struct
  (* RFC 8620 Section 5.5: "limit" is returned when the server enforced a
     different limit than the one requested. *)
  let test_query_response_limit () =
    let r =
      decode_fixture "query_response" Method.query_response_jsont
        "query_response_limit.json"
    in
    Alcotest.(check (option int64)) "limit" (Some 50L) r.Method.limit;
    Alcotest.(check (option int64)) "total" (Some 250L) r.Method.total;
    Alcotest.(check int64) "position" 42L r.Method.position

  let test_query_response_no_limit () =
    let r =
      decode_exn "query_response" Method.query_response_jsont
        (proto_method "valid/query_response.json")
    in
    Alcotest.(check (option int64)) "limit" None r.Method.limit

  let test_query_response_limit_roundtrip () =
    let s =
      roundtrip "query_response" Method.query_response_jsont
        (fixture "query_response_limit.json")
    in
    Alcotest.(check bool)
      "limit survives" true
      (contains ~needle:"\"limit\":50" s)

  let test_query_changes_response () =
    let r =
      decode_fixture "queryChanges response" Method.query_changes_response_jsont
        "query_changes_response.json"
    in
    match r.Method.added with
    | [ item ] ->
        Alcotest.(check string) "id" "e1" (Id.to_string item.Filter.id);
        Alcotest.(check int64) "index" 0L item.Filter.index
    | _ -> Alcotest.fail "expected one added item"

  (* Builders exist for every argument record (RFC 8620 Sections 5.4, 5.6). *)
  let test_copy_args_builder () =
    let a =
      Method.copy_args ~from_account_id:(id_v "a1") ~account_id:(id_v "a2")
        ~create:[ (Id.creation "c1", Jsont.Json.null ()) ]
        ()
    in
    Alcotest.(check bool)
      "onSuccessDestroyOriginal defaults to false" false
      a.Method.on_success_destroy_original;
    let s = encode_exn "copy_args" json_copy_args a in
    Alcotest.(check bool)
      "omits onSuccessDestroyOriginal" false
      (contains ~needle:"onSuccessDestroyOriginal" s)

  let test_query_changes_args_builder () =
    let a =
      Method.query_changes_args ~account_id:(id_v "a1") ~since_query_state:"qs0"
        ~max_changes:20L ()
    in
    let s = encode_exn "query_changes_args" json_query_changes_args a in
    Alcotest.(check bool)
      "maxChanges" true
      (contains ~needle:"\"maxChanges\":20" s);
    Alcotest.(check bool)
      "calculateTotal omitted" false
      (contains ~needle:"calculateTotal" s)

  let tests =
    [
      ("query response: limit", `Quick, test_query_response_limit);
      ("query response: no limit", `Quick, test_query_response_no_limit);
      ( "query response: limit roundtrip",
        `Quick,
        test_query_response_limit_roundtrip );
      ("queryChanges response: added items", `Quick, test_query_changes_response);
      ("copy args builder", `Quick, test_copy_args_builder);
      ("queryChanges args builder", `Quick, test_query_changes_args_builder);
    ]
end

module Comparator_tests = struct
  (* RFC 8621 Section 4.4.2: the hasKeyword / allInThreadHaveKeyword /
     someInThreadHaveKeyword sorts require a "keyword" property. *)
  let test_keyword_decode () =
    let c =
      decode_fixture "Comparator" Filter.comparator_jsont
        "comparator_keyword.json"
    in
    Alcotest.(check string)
      "property" "someInThreadHaveKeyword" c.Filter.property;
    Alcotest.(check (option string))
      "keyword" (Some "$flagged") c.Filter.keyword;
    Alcotest.(check bool) "isAscending" false c.Filter.is_ascending

  let test_keyword_roundtrip () =
    let s =
      roundtrip "Comparator" Filter.comparator_jsont
        (fixture "comparator_keyword.json")
    in
    Alcotest.(check bool)
      "keyword survives" true
      (contains ~needle:"\"keyword\":\"$flagged\"" s)

  let test_keyword_builder () =
    let c = Filter.comparator ~keyword:"$seen" "hasKeyword" in
    Alcotest.(check (option string)) "keyword" (Some "$seen") c.Filter.keyword;
    let s = encode_exn "Comparator" Filter.comparator_jsont c in
    Alcotest.(check bool)
      "isAscending omitted when true" false
      (contains ~needle:"isAscending" s)

  let test_no_keyword () =
    let c =
      decode_exn "Comparator" Filter.comparator_jsont
        (proto_filter "valid/comparator_with_collation.json")
    in
    Alcotest.(check (option string)) "keyword" None c.Filter.keyword;
    Alcotest.(check (option string))
      "collation" (Some "i;unicode-casemap") c.Filter.collation

  let tests =
    [
      ("keyword: decode", `Quick, test_keyword_decode);
      ("keyword: roundtrip", `Quick, test_keyword_roundtrip);
      ("keyword: builder", `Quick, test_keyword_builder);
      ("keyword: absent", `Quick, test_no_keyword);
    ]
end

module Filter_tests = struct
  (* The operator fixtures under test/proto/filter were previously unused. *)
  let decode_filter path () =
    let f = decode_exn path json_filter (proto_filter path) in
    match f with Filter.Operator _ | Filter.Condition _ -> ()

  let test_operators () =
    let check path op n =
      match decode_exn path json_filter (proto_filter path) with
      | Filter.Operator o ->
          Alcotest.(check bool)
            (path ^ " operator") true (o.Filter.operator = op);
          Alcotest.(check int)
            (path ^ " conditions") n
            (List.length o.Filter.conditions)
      | Filter.Condition _ -> Alcotest.failf "%s: expected an operator" path
    in
    check "valid/and_operator.json" `And 2;
    check "valid/or_operator.json" `Or 2;
    check "valid/not_operator.json" `Not 1;
    check "valid/nested.json" `And 3;
    check "valid/nested_and_or.json" `And 2;
    check "valid/deeply_nested.json" `And 2;
    check "edge/empty_conditions.json" `And 0

  let test_simple_condition () =
    match
      decode_exn "simple_condition" json_filter
        (proto_filter "valid/simple_condition.json")
    with
    | Filter.Condition _ -> ()
    | Filter.Operator _ -> Alcotest.fail "expected a condition"

  let test_deeply_nested_shape () =
    match
      decode_exn "deeply_nested" json_filter
        (proto_filter "valid/deeply_nested.json")
    with
    | Filter.Operator o -> (
        match o.Filter.conditions with
        | Filter.Operator n :: _ -> (
            Alcotest.(check bool) "NOT" true (n.Filter.operator = `Not);
            match n.Filter.conditions with
            | [ Filter.Operator inner ] ->
                Alcotest.(check bool) "OR" true (inner.Filter.operator = `Or)
            | _ -> Alcotest.fail "expected a nested OR")
        | _ -> Alcotest.fail "expected a nested NOT")
    | Filter.Condition _ -> Alcotest.fail "expected an operator"

  let test_roundtrip () =
    ignore (roundtrip "nested" json_filter (proto_filter "valid/nested.json"))

  let test_bad_operator () =
    expect_decode_failure "XOR" json_filter
      {|{"operator": "XOR", "conditions": []}|}

  (* A decode failure inside a nested condition keeps its position in the
     source text. *)
  let test_error_keeps_position () =
    let strict_condition =
      Jsont.Object.map ~kind:"FilterCondition" Fun.id
      |> Jsont.Object.mem "hasKeyword" Jsont.string ~enc:Fun.id
      |> Jsont.Object.finish
    in
    let jsont = Filter.filter_jsont strict_condition in
    match
      decode jsont {|{"operator":"AND","conditions":[{"hasKeyword":42}]}|}
    with
    | Ok _ -> Alcotest.fail "expected a decode failure"
    | Error e ->
        let msg = Jsont.Error.to_string e in
        Alcotest.(check bool)
          ("located: " ^ msg) true
          (contains ~needle:"line 1, characters 32-49" msg)

  let tests =
    [
      ("operators", `Quick, test_operators);
      ("simple condition", `Quick, test_simple_condition);
      ("deeply nested", `Quick, test_deeply_nested_shape);
      ("roundtrip", `Quick, test_roundtrip);
      ("unknown operator rejected", `Quick, test_bad_operator);
      ("errors keep their position", `Quick, test_error_keeps_position);
      ( "set_args fixture",
        `Quick,
        fun () ->
          ignore
            (decode_exn "set_args" json_set_args
               (proto_method "valid/set_args.json")) );
      ( "set_response fixture",
        `Quick,
        fun () ->
          ignore
            (decode_exn "set_response" json_set_response
               (proto_method "valid/set_response.json")) );
      ( "set_response_with_errors fixture",
        `Quick,
        fun () ->
          let r =
            decode_exn "set_response" json_set_response
              (proto_method "valid/set_response_with_errors.json")
          in
          match r.Method.not_destroyed with
          | Some [ (_, e) ] ->
              Alcotest.(check bool)
                "forbidden" true
                (e.Error.Set_error.type_ = `Forbidden)
          | _ -> Alcotest.fail "expected one notDestroyed error" );
      ( "simple_condition decodes",
        `Quick,
        decode_filter "valid/simple_condition.json" );
    ]
end

module Push_tests = struct
  (* The RFC 8620 Section 7.2.3 worked example, verbatim. *)
  let test_rfc_get_request () =
    let a =
      decode_fixture "PushSubscription/get" Push.get_args_jsont
        "push_get_request.json"
    in
    Alcotest.(check bool) "ids null means all" true (a.Push.ids = None)

  let test_get_args_encodes_null_ids () =
    let s =
      encode_exn "PushSubscription/get" Push.get_args_jsont (Push.get_args ())
    in
    Alcotest.(check string) "request body" {|{"ids":null}|} s

  let test_rfc_get_response () =
    let r =
      decode_fixture "PushSubscription/get" Push.get_response_jsont
        "push_get_response.json"
    in
    Alcotest.(check int) "list" 2 (List.length r.Push.list);
    Alcotest.(check int) "notFound" 0 (List.length r.Push.not_found);
    match r.Push.list with
    | first :: _ ->
        Alcotest.(check string)
          "id" "e50b2c1d-9553-41a3-b0a7-a7d26b599ee1"
          (Id.to_string first.Push.id);
        (* A conformant server never returns url or keys. *)
        Alcotest.(check bool) "url absent" true (first.Push.url = None);
        Alcotest.(check bool) "keys absent" true (first.Push.keys = None);
        Alcotest.(check (option (list string)))
          "types" (Some [ "Todo" ]) first.Push.types
    | [] -> Alcotest.fail "expected two subscriptions"

  let test_rfc_set_request () =
    let a =
      decode_fixture "PushSubscription/set" Push.set_args_jsont
        "push_set_request.json"
    in
    match a.Push.create with
    | Some [ (cid, c) ] ->
        Alcotest.(check string)
          "creation id" "4f29"
          (Id.to_string (Id.creation_id cid));
        Alcotest.(check string)
          "deviceClientId" "a889-ffea-910" c.Push.device_client_id;
        (* "types": null in the RFC's own request. *)
        Alcotest.(check bool) "types null" true (c.Push.types = None)
    | _ -> Alcotest.fail "expected one create"

  let test_rfc_set_response () =
    let r =
      decode_fixture "PushSubscription/set" Push.set_response_jsont
        "push_set_response.json"
    in
    match r.Push.created with
    | Some [ (cid, sub) ] ->
        Alcotest.(check string) "creation id" "4f29" (Id.to_string cid);
        Alcotest.(check string)
          "id" "P43dcfa4-1dd4-41ef-9156-2c89b3b19c60" (Id.to_string sub.Push.id);
        (* "keys": null, and neither deviceClientId nor url are returned. *)
        Alcotest.(check bool) "keys null" true (sub.Push.keys = None);
        Alcotest.(check bool)
          "deviceClientId absent" true
          (sub.Push.device_client_id = None);
        Alcotest.(check bool) "expires set" true (sub.Push.expires <> None)
    | _ -> Alcotest.fail "expected one created subscription"

  (* RFC 8620 Section 7.2 types "keys", "verificationCode", "expires" and
     "types" as "T|null", so an explicit null decodes; "deviceClientId" and
     "url" are plain "String" and so are absent rather than null when the
     server does not return them. *)
  let test_subscription_nulls () =
    let s =
      decode_fixture "PushSubscription" Push.jsont
        "push_subscription_nulls.json"
    in
    Alcotest.(check (option string))
      "deviceClientId" (Some "dev1") s.Push.device_client_id;
    Alcotest.(check bool) "url absent" true (s.Push.url = None);
    Alcotest.(check bool) "keys" true (s.Push.keys = None);
    Alcotest.(check bool)
      "verificationCode" true
      (s.Push.verification_code = None);
    Alcotest.(check bool) "expires" true (s.Push.expires = None);
    Alcotest.(check bool) "types" true (s.Push.types = None)

  (* Both are plain "String": a null is malformed, not "no value". *)
  let test_subscription_string_members_reject_null () =
    expect_decode_failure "PushSubscription url" Push.jsont
      {|{"id":"sub1","url":null}|};
    expect_decode_failure "PushSubscription deviceClientId" Push.jsont
      {|{"id":"sub1","deviceClientId":null}|}

  (* RFC 8620 Section 7.2: "url" "MUST begin with 'https://'", and
     "deviceClientId" identifies the client + device, so it cannot be empty. *)
  let test_create_args_validation () =
    let ok = function
      | Ok v -> v
      | Error msg -> Alcotest.failf "create_args rejected: %s" msg
    in
    let is_error name = function
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "%s: expected create_args to be rejected" name
    in
    let c =
      ok
        (Push.create_args ~device_client_id:"a889-ffea-910"
           ~url:"https://example.com/push/?device=X8980fc" ())
    in
    Alcotest.(check string)
      "deviceClientId" "a889-ffea-910" c.Push.device_client_id;
    is_error "http url"
      (Push.create_args ~device_client_id:"d" ~url:"http://example.com/" ());
    is_error "scheme-relative url"
      (Push.create_args ~device_client_id:"d" ~url:"//example.com/" ());
    is_error "bare scheme"
      (Push.create_args ~device_client_id:"d" ~url:"https://" ());
    is_error "query without host"
      (Push.create_args ~device_client_id:"d" ~url:"https://?push=1" ());
    is_error "path without host"
      (Push.create_args ~device_client_id:"d" ~url:"https:///push" ());
    is_error "malformed port"
      (Push.create_args ~device_client_id:"d" ~url:"https://example.com:x" ());
    is_error "empty port"
      (Push.create_args ~device_client_id:"d" ~url:"https://example.com:" ());
    is_error "out-of-range port"
      (Push.create_args ~device_client_id:"d" ~url:"https://example.com:65536"
         ());
    is_error "empty deviceClientId"
      (Push.create_args ~device_client_id:"" ~url:"https://example.com/" ());
    let base64url value =
      Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet value
    in
    let keys =
      Push.push_keys
        ~p256dh:(base64url ("\x04" ^ String.make 64 '\x00'))
        ~auth:(base64url (String.make 16 '\x00'))
    in
    Alcotest.(check bool) "valid Web Push keys" true (Result.is_ok keys);
    Alcotest.(check bool)
      "short auth secret" true
      (Result.is_error
         (Push.push_keys
            ~p256dh:(base64url ("\x04" ^ String.make 64 '\x00'))
            ~auth:(base64url "short")));
    (* RFC 7515 Section 2 omits the base64url padding. *)
    Alcotest.(check bool)
      "padded auth secret" true
      (Result.is_error
         (Push.push_keys
            ~p256dh:(base64url ("\x04" ^ String.make 64 '\x00'))
            ~auth:
              (Base64.encode_string ~pad:true ~alphabet:Base64.uri_safe_alphabet
                 (String.make 16 '\x00'))))

  let test_create_expiry_and_partial_update () =
    let expires =
      match Ptime.of_rfc3339 "2030-01-02T03:04:05Z" with
      | Ok (time, _, _) -> time
      | Error _ -> Alcotest.fail "test timestamp"
    in
    let create =
      Result.get_ok
        (Push.create_args ~device_client_id:"device"
           ~url:"https://example.com/push" ~expires ())
    in
    let encoded =
      encode_exn "PushSubscription create" Push.create_args_jsont create
    in
    Alcotest.(check bool)
      "expires encoded" true
      (contains ~needle:"\"expires\":\"2030-01-02T03:04:05Z\"" encoded);
    expect_decode_failure "verificationCode on create" Push.create_args_jsont
      {|{"deviceClientId":"device","url":"https://example.com/push","verificationCode":"code"}|};
    let response =
      decode_exn "PushSubscription/set" Push.set_response_jsont
        {|{"updated":{"sub1":{"expires":"2030-01-02T03:04:05Z"}}}|}
    in
    match response.Push.updated with
    | Some [ (_, Some properties) ] ->
        Alcotest.(check bool)
          "partial expires" true
          (Option.is_some properties.Push.expires)
    | _ -> Alcotest.fail "expected a partial updated response"

  (* RFC 8620 Section 5.3: a destroy entry may name a record created in the
     same call. *)
  let test_destroy_creation_reference () =
    let a =
      decode_exn "PushSubscription/set" Push.set_args_jsont
        {|{"destroy":["#4f29","P43dcfa4"]}|}
    in
    match a.Push.destroy with
    | Some [ cref; real ] ->
        Alcotest.(check bool)
          "creation reference" true (Id.is_creation_ref cref);
        Alcotest.(check (option string))
          "creation id" (Some "4f29") (Id.to_creation_id cref);
        Alcotest.(check bool) "plain id" false (Id.is_creation_ref real)
    | _ -> Alcotest.fail "expected two destroy entries"

  let test_destroy_bare_hash_rejected () =
    expect_decode_failure "PushSubscription/set" Push.set_args_jsont
      {|{"destroy":["#"]}|}

  (* RFC 8620 Sections 7.2.1/7.2.2: no accountId, no state, no ifInState,
     no oldState/newState. *)
  let test_no_account_id () =
    let s =
      encode_exn "PushSubscription/set" Push.set_args_jsont
        (Push.set_args ~destroy:[ id_v "sub1" ] ())
    in
    Alcotest.(check string) "set args" {|{"destroy":["sub1"]}|} s;
    let get_response =
      encode_exn "PushSubscription/get" Push.get_response_jsont
        { Push.list = []; not_found = [] }
    in
    Alcotest.(check string)
      "get response" {|{"list":[],"notFound":[]}|} get_response;
    let set_response =
      encode_exn "PushSubscription/set" Push.set_response_jsont
        {
          Push.created = None;
          updated = None;
          destroyed = Some [];
          not_created = None;
          not_updated = None;
          not_destroyed = None;
        }
    in
    Alcotest.(check string) "set response" {|{"destroyed":[]}|} set_response

  let test_push_verification () =
    let v =
      decode_fixture "PushVerification" Push.Push_verification.jsont
        "push_verification.json"
    in
    Alcotest.(check string)
      "pushSubscriptionId" "P43dcfa4-1dd4-41ef-9156-2c89b3b19c60"
      v.Push.Push_verification.push_subscription_id;
    Alcotest.(check string)
      "verificationCode" "da1f097b11ca17f06424e30bf02bfa67"
      v.Push.Push_verification.verification_code;
    let built =
      Push.Push_verification.v
        ~push_subscription_id:"P43dcfa4-1dd4-41ef-9156-2c89b3b19c60"
        ~verification_code:"da1f097b11ca17f06424e30bf02bfa67"
    in
    Alcotest.(check bool) "same object" true (built = v)

  let test_push_verification_bad_type () =
    expect_decode_failure "PushVerification" Push.Push_verification.jsont
      {|{"@type":"StateChange","pushSubscriptionId":"p","verificationCode":"c"}|}

  let test_state_change () =
    let sc =
      decode_fixture "StateChange" Push.State_change.jsont "state_change.json"
    in
    Alcotest.(check string) "@type" "StateChange" Push.State_change.type_name;
    match sc.Push.State_change.changed with
    | [ (account, types) ] ->
        Alcotest.(check string) "account" "a3123" (Id.to_string account);
        Alcotest.(check int) "types" 3 (List.length types)
    | _ -> Alcotest.fail "expected one account"

  let test_state_change_bad_type () =
    expect_decode_failure "StateChange" Push.State_change.jsont
      (fixture "state_change_bad_type.json")

  let test_state_change_builder () =
    let sc =
      Push.State_change.v
        [
          ( id_v "a1",
            [ { Push.State_change.type_name = "Email"; state = "s1" } ] );
        ]
    in
    let s = encode_exn "StateChange" Push.State_change.jsont sc in
    Alcotest.(check bool)
      "@type emitted" true
      (contains ~needle:{|"@type":"StateChange"|} s)

  let tests =
    [
      ("RFC 7.2.3: /get request", `Quick, test_rfc_get_request);
      ("/get args encode ids null", `Quick, test_get_args_encodes_null_ids);
      ("RFC 7.2.3: /get response", `Quick, test_rfc_get_response);
      ("RFC 7.2.3: /set request", `Quick, test_rfc_set_request);
      ("RFC 7.2.3: /set response", `Quick, test_rfc_set_response);
      ("subscription: nullable members", `Quick, test_subscription_nulls);
      ( "subscription: String members reject null",
        `Quick,
        test_subscription_string_members_reject_null );
      ("create_args: RFC 7.2 validation", `Quick, test_create_args_validation);
      ( "create expiry and partial update response",
        `Quick,
        test_create_expiry_and_partial_update );
      ("destroy: creation reference", `Quick, test_destroy_creation_reference);
      ("destroy: bare # rejected", `Quick, test_destroy_bare_hash_rejected);
      ("no accountId/state", `Quick, test_no_account_id);
      ("PushVerification", `Quick, test_push_verification);
      ("PushVerification: bad @type", `Quick, test_push_verification_bad_type);
      ("StateChange", `Quick, test_state_change);
      ("StateChange: bad @type", `Quick, test_state_change_bad_type);
      ("StateChange: builder", `Quick, test_state_change_builder);
    ]
end

module Set_error_tests = struct
  let check_type name expected e =
    Alcotest.(check string)
      name
      (Error.Set_error.type_to_string expected)
      (Error.Set_error.type_to_string e.Error.Set_error.type_)

  (* RFC 8620 Section 5.4: existingId MUST be present on alreadyExists. *)
  let test_already_exists () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_already_exists.json"
    in
    check_type "type" `Already_exists e;
    Alcotest.(check (option string))
      "existingId" (Some "M1234")
      (Option.map Id.to_string e.Error.Set_error.existing_id)

  let test_already_exists_roundtrip () =
    let s =
      roundtrip "SetError" Error.Set_error.jsont
        (fixture "set_error_already_exists.json")
    in
    Alcotest.(check bool)
      "existingId survives" true
      (contains ~needle:{|"existingId":"M1234"|} s)

  (* RFC 8621 Section 4.6. *)
  let test_blob_not_found () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_blob_not_found.json"
    in
    check_type "type" `Blob_not_found e;
    Alcotest.(check (option (list string)))
      "notFound"
      (Some [ "Bx1"; "Bx2" ])
      (Option.map (List.map Id.to_string) e.Error.Set_error.not_found)

  (* RFC 8621 Section 7.5. *)
  let test_too_many_recipients () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_too_many_recipients.json"
    in
    check_type "type" `Too_many_recipients e;
    Alcotest.(check (option int64))
      "maxRecipients" (Some 100L) e.Error.Set_error.max_recipients

  let test_invalid_recipients () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_invalid_recipients.json"
    in
    check_type "type" `Invalid_recipients e;
    Alcotest.(check (option (list string)))
      "invalidRecipients"
      (Some [ "not-an-address"; "another@@bad" ])
      e.Error.Set_error.invalid_recipients

  let test_too_large_max_size () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_too_large_max_size.json"
    in
    check_type "type" `Too_large e;
    Alcotest.(check (option int64))
      "maxSize" (Some 20971520L) e.Error.Set_error.max_size

  let test_mailbox_has_email () =
    let e =
      decode_fixture "SetError" Error.Set_error.jsont
        "set_error_mailbox_has_email.json"
    in
    check_type "type" `Mailbox_has_email e

  let test_spellings () =
    let check s t =
      Alcotest.(check string) s s (Error.Set_error.type_to_string t);
      Alcotest.(check bool)
        ("parse " ^ s) true
        (Error.Set_error.type_of_string s = t)
    in
    check "alreadyExists" `Already_exists;
    check "mailboxHasChild" `Mailbox_has_child;
    check "mailboxHasEmail" `Mailbox_has_email;
    check "blobNotFound" `Blob_not_found;
    check "tooManyKeywords" `Too_many_keywords;
    check "tooManyMailboxes" `Too_many_mailboxes;
    check "invalidEmail" `Invalid_email;
    check "tooManyRecipients" `Too_many_recipients;
    check "noRecipients" `No_recipients;
    check "invalidRecipients" `Invalid_recipients;
    check "cannotUnsend" `Cannot_unsend;
    check "forbiddenMailFrom" `Forbidden_mail_from;
    check "forbiddenFrom" `Forbidden_from;
    check "forbiddenToSend" `Forbidden_to_send

  let test_builder () =
    let e =
      match Error.Set_error.v ~existing_id:(id_v "M1") `Already_exists with
      | Ok e -> e
      | Error msg -> Alcotest.failf "Set_error.v rejected: %s" msg
    in
    let s = encode_exn "SetError" Error.Set_error.jsont e in
    Alcotest.(check string)
      "encoded" {|{"type":"alreadyExists","existingId":"M1"}|} s

  (* RFC 8620 Section 5.4 and RFC 8621 Sections 4.6 and 7.5 make one further
     member mandatory for four of the types. *)
  let test_builder_validation () =
    let is_error name = function
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "%s: expected Set_error.v to be rejected" name
    in
    is_error "alreadyExists" (Error.Set_error.v `Already_exists);
    is_error "blobNotFound" (Error.Set_error.v `Blob_not_found);
    is_error "tooManyRecipients" (Error.Set_error.v `Too_many_recipients);
    is_error "invalidRecipients" (Error.Set_error.v `Invalid_recipients);
    is_error "negative maxSize" (Error.Set_error.v ~max_size:(-1L) `Too_large);
    is_error "negative maxRecipients"
      (Error.Set_error.v ~max_recipients:(-1L) ~invalid_recipients:[]
         `Too_many_recipients);
    (match Error.Set_error.v ~not_found:[ id_v "B1" ] `Blob_not_found with
    | Ok _ -> ()
    | Error msg -> Alcotest.failf "blobNotFound with notFound rejected: %s" msg);
    (* A "Other" name that names a defined type is normalised, so the
       requirement still applies. *)
    is_error "normalised alreadyExists"
      (Error.Set_error.v (`Other "alreadyExists"))

  let tests =
    [
      ("builder validation", `Quick, test_builder_validation);
      ("alreadyExists", `Quick, test_already_exists);
      ("alreadyExists roundtrip", `Quick, test_already_exists_roundtrip);
      ("blobNotFound", `Quick, test_blob_not_found);
      ("tooManyRecipients", `Quick, test_too_many_recipients);
      ("invalidRecipients", `Quick, test_invalid_recipients);
      ("tooLarge maxSize", `Quick, test_too_large_max_size);
      ("mailboxHasEmail", `Quick, test_mailbox_has_email);
      ("spellings", `Quick, test_spellings);
      ("builder", `Quick, test_builder);
    ]
end

module Error_tests = struct
  (* RFC 7807 makes every member of a problem details object optional. *)
  let test_request_error_no_status () =
    let e =
      decode_fixture "Request error" Error.Request_error.jsont
        "request_error_no_status.json"
    in
    Alcotest.(check bool) "type" true (e.Error.Request_error.type_ = `Not_json);
    Alcotest.(check (option int)) "status" None e.Error.Request_error.status

  let test_request_error_status_is_a_number () =
    expect_decode_failure "Request error" Error.Request_error.jsont
      (fixture "request_error_string_status.json")

  let test_request_error_with_status () =
    let e =
      decode_exn "Request error" Error.Request_error.jsont
        (read_file "../proto/error/valid/request_error.json")
    in
    Alcotest.(check bool)
      "status present" true
      (e.Error.Request_error.status <> None)

  let test_method_error_types () =
    let check s t =
      Alcotest.(check string) s s (Error.Method_error.type_to_string t);
      Alcotest.(check bool)
        ("parse " ^ s) true
        (Error.Method_error.type_of_string s = t)
    in
    check "cannotCalculateChanges" `Cannot_calculate_changes;
    check "requestTooLarge" `Request_too_large;
    check "stateMismatch" `State_mismatch;
    check "anchorNotFound" `Anchor_not_found;
    check "unsupportedSort" `Unsupported_sort;
    check "unsupportedFilter" `Unsupported_filter;
    check "tooManyChanges" `Too_many_changes;
    check "fromAccountNotFound" `From_account_not_found;
    check "fromAccountNotSupportedByMethod"
      `From_account_not_supported_by_method

  let test_method_error_decode () =
    let e =
      decode_fixture "Method error" Error.Method_error.jsont
        "method_error_state_mismatch.json"
    in
    Alcotest.(check bool)
      "type" true
      (e.Error.Method_error.type_ = `State_mismatch)

  (* RFC 8620 Section 3.6.1: for urn:ietf:params:jmap:error:limit, "a 'limit'
     property MUST also be present on the 'problem details' object,
     containing the name of the limit being applied". *)
  let test_limit_error_decodes () =
    let e =
      decode_fixture "Request error" Error.Request_error.jsont
        "request_error_limit.json"
    in
    Alcotest.(check bool) "type" true (e.Error.Request_error.type_ = `Limit);
    Alcotest.(check (option string))
      "limit" (Some "maxSizeRequest") e.Error.Request_error.limit;
    match Error.Request_error.validate e with
    | Ok _ -> ()
    | Error msg -> Alcotest.failf "validate rejected a conformant error: %s" msg

  let test_limit_error_without_limit () =
    (* Decoding stays tolerant: the value is still usable. *)
    let e =
      decode_fixture "Request error" Error.Request_error.jsont
        "request_error_limit_no_limit.json"
    in
    Alcotest.(check bool) "type" true (e.Error.Request_error.type_ = `Limit);
    Alcotest.(check bool)
      "limit missing" true
      (e.Error.Request_error.limit = None);
    (* validate is what reports the violation. *)
    (match Error.Request_error.validate e with
    | Ok _ -> Alcotest.fail "validate accepted a limit error with no limit"
    | Error _ -> ());
    (* And the builder refuses to make one. *)
    (match Error.Request_error.v ~status:400 `Limit with
    | Ok _ -> Alcotest.fail "v accepted a limit error with no limit"
    | Error _ -> ());
    match
      Error.Request_error.v ~status:400 ~limit:"maxCallsInRequest" `Limit
    with
    | Error msg -> Alcotest.failf "v rejected a conformant error: %s" msg
    | Ok e -> (
        Alcotest.(check (option string))
          "limit" (Some "maxCallsInRequest") e.Error.Request_error.limit;
        (* Every other error type is fine without a limit. *)
        match Error.Request_error.v ~status:400 `Not_json with
        | Ok _ -> ()
        | Error msg -> Alcotest.failf "notJSON rejected: %s" msg)

  (* RFC 7807 Section 3.1: a problem type MAY add its own members, and
     RFC 8620 Section 2 tells clients to ignore what they do not know; they
     must survive a re-encode rather than being dropped. *)
  let test_request_error_extension_members () =
    let json = fixture "request_error_extension.json" in
    let e = decode_exn "Request error" Error.Request_error.jsont json in
    Alcotest.(check bool)
      "balance kept" true
      (Error.Request_error.unknown_member e "balance" <> None);
    Alcotest.(check bool)
      "accounts kept" true
      (Error.Request_error.unknown_member e "accounts" <> None);
    Alcotest.(check bool)
      "no such member" true
      (Error.Request_error.unknown_member e "nope" = None);
    let s = encode_exn "Request error" Error.Request_error.jsont e in
    Alcotest.(check bool)
      "balance re-encoded" true
      (contains ~needle:{|"balance":30|} s);
    Alcotest.(check bool)
      "accounts re-encoded" true
      (contains ~needle:{|"/account/67890"|} s)

  let test_method_error_extension_members () =
    let json = {|{"type":"serverFail","description":"boom","retryAfter":30}|} in
    let e = decode_exn "Method error" Error.Method_error.jsont json in
    Alcotest.(check bool)
      "retryAfter kept" true
      (Error.Method_error.unknown_member e "retryAfter" <> None);
    let s = encode_exn "Method error" Error.Method_error.jsont e in
    Alcotest.(check bool)
      "retryAfter re-encoded" true
      (contains ~needle:{|"retryAfter":30|} s)

  let test_set_error_extension_members () =
    let json = {|{"type":"forbidden","aclRequired":"mayAddItems"}|} in
    let e = decode_exn "SetError" Error.Set_error.jsont json in
    Alcotest.(check bool)
      "aclRequired kept" true
      (Error.Set_error.unknown_member e "aclRequired" <> None);
    let s = encode_exn "SetError" Error.Set_error.jsont e in
    Alcotest.(check bool)
      "aclRequired re-encoded" true
      (contains ~needle:{|"aclRequired":"mayAddItems"|} s)

  (* RFC 7807 Section 4.2: an absent "type" member means "about:blank". *)
  let test_request_error_absent_type () =
    let e =
      decode_exn "Request error" Error.Request_error.jsont
        {|{"status":400,"detail":"nope"}|}
    in
    Alcotest.(check bool)
      "about:blank" true
      (e.Error.Request_error.type_ = `Other "about:blank")

  let test_request_error_null_members () =
    let e =
      decode_exn "Request error" Error.Request_error.jsont
        {|{"type":"urn:ietf:params:jmap:error:notJSON","status":null,
           "title":null,"detail":null,"limit":null}|}
    in
    Alcotest.(check bool) "status" true (e.Error.Request_error.status = None);
    Alcotest.(check bool) "detail" true (e.Error.Request_error.detail = None)

  let test_request_error_status_range () =
    (match Error.Request_error.v ~status:1000 `Not_json with
    | Ok _ -> Alcotest.fail "expected status 1000 to be rejected"
    | Error _ -> ());
    match Error.Request_error.v ~status:(-1) `Not_json with
    | Ok _ -> Alcotest.fail "expected a negative status to be rejected"
    | Error _ -> ()

  (* A method error description may be sent as an explicit null. *)
  let test_method_error_null_description () =
    let e =
      decode_exn "Method error" Error.Method_error.jsont
        {|{"type":"serverFail","description":null}|}
    in
    Alcotest.(check bool)
      "description" true
      (e.Error.Method_error.description = None)

  let test_error_printers_escape_control_bytes () =
    let e =
      Result.get_ok
        (Error.Method_error.v ~description:"line one\n\027[2J"
           (`Other "extension\rname"))
    in
    let text = Error.Method_error.to_string e in
    List.iter
      (fun c ->
        Alcotest.(check bool)
          (Printf.sprintf "no control byte %d" (Char.code c))
          false (String.contains text c))
      [ '\n'; '\r'; '\027' ];
    Alcotest.(check bool)
      "escapes remain visible" true
      (contains ~needle:"\\x0A\\x1B" text)

  let tests =
    [
      ("request error: absent type", `Quick, test_request_error_absent_type);
      ("request error: null members", `Quick, test_request_error_null_members);
      ("request error: v checks status", `Quick, test_request_error_status_range);
      ( "method error: null description",
        `Quick,
        test_method_error_null_description );
      ( "error printers escape control bytes",
        `Quick,
        test_error_printers_escape_control_bytes );
      ("request error: no status", `Quick, test_request_error_no_status);
      ( "request error: status is a number",
        `Quick,
        test_request_error_status_is_a_number );
      ("request error: with status", `Quick, test_request_error_with_status);
      ("method error: RFC 8620 5/6 types", `Quick, test_method_error_types);
      ("method error: stateMismatch decodes", `Quick, test_method_error_decode);
      ("request error: limit carries a limit", `Quick, test_limit_error_decodes);
      ( "request error: limit without a limit",
        `Quick,
        test_limit_error_without_limit );
      ( "request error: extension members kept",
        `Quick,
        test_request_error_extension_members );
      ( "method error: extension members kept",
        `Quick,
        test_method_error_extension_members );
      ( "SetError: extension members kept",
        `Quick,
        test_set_error_extension_members );
    ]
end

(* PatchObject (RFC 8620 Section 5.3) *)
module Patch_tests = struct
  let json_of_patch p = encode_exn "PatchObject" Patch.jsont p

  let expect_invalid name f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.failf "%s: expected Invalid_argument" name

  let expect_error name = function
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s: expected an error" name

  (* "The keys are a path in JSON Pointer format [RFC6901], with an implicit
     leading '/'." *)
  let test_shape () =
    let p =
      Patch.v
        [
          Email.Patch.set_keyword `Seen;
          Email.Patch.add_to_mailbox (id_v "Marchive");
          Email.Patch.remove_from_mailbox (id_v "Minbox");
        ]
    in
    Alcotest.(check string)
      "encoded"
      {|{"keywords/$seen":true,"mailboxIds/Marchive":true,"mailboxIds/Minbox":null}|}
      (json_of_patch p)

  let test_empty () =
    Alcotest.(check string) "empty" "{}" (json_of_patch Patch.empty);
    Alcotest.(check bool) "is_empty" true (Patch.is_empty Patch.empty)

  let test_set_maps () =
    let p =
      Patch.v
        [
          Email.Patch.set_mailboxes [ id_v "Ma"; id_v "Mb" ];
          Email.Patch.set_keywords [ `Seen ];
        ]
    in
    Alcotest.(check string)
      "encoded"
      {|{"mailboxIds":{"Ma":true,"Mb":true},"keywords":{"$seen":true}}|}
      (json_of_patch p)

  (* Constructing a patch from tokens or from its wire key must identify the
     same JSON Pointer. Keep edge cases in one table so both entry points are
     exercised uniformly. *)
  let test_pointer_forms () =
    let cases =
      [
        ("slash and tilde", [ "keywords"; "a/b~c" ], "keywords/a~1b~0c");
        ("unescape order", [ "~1" ], "~01");
        ("empty middle token", [ "a"; ""; "b" ], "a//b");
      ]
    in
    List.iter
      (fun (name, tokens, key) ->
        let from_tokens = Patch.path tokens None in
        let from_key = Patch.pointer key None in
        Alcotest.(check string)
          (name ^ " from tokens") key
          (Patch.entry_key from_tokens);
        Alcotest.(check string)
          (name ^ " from key") key (Patch.entry_key from_key);
        expect_error
          (name ^ " denotes one path")
          (Patch.of_entries [ from_tokens; from_key ]))
      cases

  (* "There MUST NOT be two patches in the PatchObject where the pointer of
     one is the prefix of the pointer of the other." *)
  let test_prefix_rejected () =
    expect_invalid "prefix" (fun () ->
        Patch.v [ Email.Patch.set_keyword `Seen; Email.Patch.set_keywords [] ]);
    expect_invalid "prefix (other order)" (fun () ->
        Patch.v [ Email.Patch.set_keywords []; Email.Patch.set_keyword `Seen ]);
    expect_error "prefix"
      (Patch.of_entries
         [ Patch.pointer "alerts" None; Patch.pointer "alerts/1/offset" None ])

  let test_duplicate_rejected () =
    expect_invalid "duplicate" (fun () ->
        Patch.v
          [ Email.Patch.set_keyword `Seen; Email.Patch.remove_keyword `Seen ])

  (* Sibling paths under the same parent are fine: neither is a prefix of the
     other in the token sense. *)
  let test_siblings_allowed () =
    let p =
      Patch.v
        [ Email.Patch.set_keyword `Seen; Email.Patch.set_keyword `Flagged ]
    in
    Alcotest.(check int) "two entries" 2 (List.length (Patch.to_list p))

  let test_dash_object_member () =
    let patch = Patch.v [ Email.Patch.add_to_mailbox (Id.of_string_exn "-") ] in
    Alcotest.(check string)
      "legal mailbox Id" {|{"mailboxIds/-":true}|} (json_of_patch patch);
    let decoded =
      decode_exn "PatchObject" Patch.jsont {|{"keywords/-":null}|}
    in
    Alcotest.(check string)
      "legal keyword" {|{"keywords/-":null}|} (json_of_patch decoded)

  let test_empty_path_rejected () =
    expect_invalid "empty token list" (fun () -> Patch.path [] None);
    expect_invalid "one empty token" (fun () -> Patch.path [ "" ] None);
    expect_invalid "empty key" (fun () -> Patch.pointer "" None)

  let test_bad_escape_rejected () =
    List.iter
      (fun key -> expect_invalid key (fun () -> Patch.pointer key None))
      [ "keywords/a~"; "keywords/a~2b" ]

  (* "This patch definition is designed such that an entire Foo object is also
     a valid PatchObject." *)
  let test_of_json_roundtrip () =
    let json = {|{"keywords/$seen":true,"mailboxIds/Minbox":null}|} in
    let p = decode_exn "PatchObject" Patch.jsont json in
    Alcotest.(check string) "roundtrip" json (json_of_patch p);
    Alcotest.(check int) "entries" 2 (List.length (Patch.to_list p));
    Alcotest.(check bool)
      "null decodes to None" true
      (List.assoc "mailboxIds/Minbox" (Patch.to_list p) = None)

  let test_of_json_rejects_prefix () =
    expect_decode_failure "PatchObject" Patch.jsont
      {|{"keywords":{},"keywords/$seen":true}|}

  let test_of_json_rejects_non_object () =
    expect_decode_failure "PatchObject" Patch.jsont {|[1,2]|}

  let test_add () =
    let p = Patch.v [ Email.Patch.set_keyword `Seen ] in
    (match Patch.add (Email.Patch.set_keyword `Flagged) p with
    | Ok p -> Alcotest.(check int) "two" 2 (List.length (Patch.to_list p))
    | Error e -> Alcotest.failf "add: %s" e);
    expect_error "add prefix" (Patch.add (Email.Patch.set_keywords []) p)

  (* Each entry is checked against a map of the entries already added rather
     than against every one of them in turn, so a large patch is not
     quadratic. *)
  let test_large_patch () =
    let n = 20_000 in
    let entries =
      List.init n (fun i ->
          Patch.pointer
            (Printf.sprintf "mailboxIds/M%d" i)
            (Some (Jsont.Json.bool true)))
    in
    let start = Sys.time () in
    let p =
      match Patch.of_entries entries with
      | Ok p -> p
      | Error e -> Alcotest.failf "of_entries: %s" e
    in
    let elapsed = Sys.time () -. start in
    let l = Patch.to_list p in
    Alcotest.(check int) "entries" n (List.length l);
    Alcotest.(check string) "first key" "mailboxIds/M0" (fst (List.hd l));
    Alcotest.(check string)
      "last key"
      (Printf.sprintf "mailboxIds/M%d" (n - 1))
      (fst (List.nth l (n - 1)));
    Alcotest.(check bool)
      (Printf.sprintf "%d entries in %.3fs" n elapsed)
      true (elapsed < 1.0);
    (* A prefix of one of the 20000 is still caught. *)
    expect_error "prefix at scale"
      (Patch.add (Patch.pointer "mailboxIds" None) p)

  let test_set_field () =
    let p =
      Patch.v
        [
          Patch.set_field "name" (Jsont.Json.string "Archive");
          Patch.remove_field "role";
        ]
    in
    Alcotest.(check string)
      "encoded" {|{"name":"Archive","role":null}|} (json_of_patch p)

  let tests =
    [
      ("JSON shape", `Quick, test_shape);
      ("empty patch", `Quick, test_empty);
      ("whole-map replacement", `Quick, test_set_maps);
      ("shared JSON Pointer forms", `Quick, test_pointer_forms);
      ("prefix pointers rejected", `Quick, test_prefix_rejected);
      ("duplicate paths rejected", `Quick, test_duplicate_rejected);
      ("sibling paths allowed", `Quick, test_siblings_allowed);
      ("dash object member", `Quick, test_dash_object_member);
      ("empty path rejected", `Quick, test_empty_path_rejected);
      ("invalid escape rejected", `Quick, test_bad_escape_rejected);
      ("decode from JSON", `Quick, test_of_json_roundtrip);
      ("decode rejects prefix pair", `Quick, test_of_json_rejects_prefix);
      ("decode rejects non-object", `Quick, test_of_json_rejects_non_object);
      ("incremental add", `Quick, test_add);
      ("20000 entries", `Quick, test_large_patch);
      ("set and remove a field", `Quick, test_set_field);
    ]
end

(* Creation references (RFC 8620 Section 5.3) in client arguments. *)
module Creation_ref_tests = struct
  let test_update_map_key () =
    let a =
      decode_exn "set args" json_set_args
        {|{"accountId":"a1","update":{"#draft1":{"keywords/$seen":true}}}|}
    in
    match a.Method.update with
    | Some [ (id, _) ] ->
        Alcotest.(check (option string))
          "creation id" (Some "draft1") (Id.to_creation_id id)
    | _ -> Alcotest.fail "expected one update entry"

  let test_bare_hash_key_rejected () =
    expect_decode_failure "set args" json_set_args
      {|{"accountId":"a1","update":{"#":{}}}|}

  let test_destroy_creation_reference () =
    let a =
      decode_exn "set args" json_set_args
        {|{"accountId":"a1","destroy":["#draft1","M1"]}|}
    in
    match a.Method.destroy with
    | Some [ cref; real ] ->
        Alcotest.(check bool)
          "creation reference" true (Id.is_creation_ref cref);
        Alcotest.(check bool) "plain id" false (Id.is_creation_ref real);
        (* It encodes back to the "#"-prefixed form. *)
        let s = encode_exn "set args" json_set_args a in
        Alcotest.(check bool)
          "re-encoded" true
          (contains ~needle:{|"destroy":["#draft1","M1"]|} s)
    | _ -> Alcotest.fail "expected two destroy entries"

  (* A response-side Id stays strict: a server never sends a creation
     reference back. *)
  let test_response_ids_stay_strict () =
    expect_decode_failure "set response" json_set_response
      {|{"accountId":"a1","newState":"s","destroyed":["#draft1"]}|}

  let tests =
    [
      ("update map key", `Quick, test_update_map_key);
      ("bare # map key rejected", `Quick, test_bare_hash_key_rejected);
      ("destroy entry", `Quick, test_destroy_creation_reference);
      ("response ids stay strict", `Quick, test_response_ids_stay_strict);
    ]
end

(* Reading a response: the helpers over the generic /set and /get shapes. *)
module Response_helper_tests = struct
  (* RFC 8620 Section 5.3 reports the failures of a /set over three members,
     which a client that acts on none of them in particular reads as one. *)
  let test_pp_set_failure () =
    let e = Result.get_ok (Error.Set_error.v `Invalid_properties) in
    Alcotest.(check bool)
      "the id and the error type are printed" true
      (let s =
         Format.asprintf "%a" Method.pp_set_failure (Id.of_string_exn "m1", e)
       in
       String.starts_with ~prefix:"m1: " s && String.length s > 4)

  let test_set_failures () =
    let r =
      decode_exn "set_response" json_set_response
        (proto_method "valid/set_response_with_errors.json")
    in
    Alcotest.(check (list string))
      "notCreated, then notUpdated, then notDestroyed"
      [ "new2"; "existing1"; "old1" ]
      (List.map (fun (i, _) -> Id.to_string i) (Method.set_failures r));
    Alcotest.(check bool)
      "the SetError comes with them" true
      (match Method.set_failures r with
      | (_, e) :: _ -> e.Error.Set_error.type_ = `Invalid_properties
      | [] -> false);
    let ok =
      decode_exn "set_response" json_set_response
        (proto_method "valid/set_response.json")
    in
    Alcotest.(check int)
      "a call that succeeded" 0
      (List.length (Method.set_failures ok))

  (* RFC 8620 Section 5.3 keys the created map by creation id, which is how a
     client reads back the record the server made for it. *)
  let test_created () =
    let r =
      decode_exn "set_response" json_set_response
        (proto_method "valid/set_response.json")
    in
    Alcotest.(check (option string))
      "the record the server created"
      (Some {|{"id":"mb123","name":"Folder 1"}|})
      (Option.map
         (encode_exn "created" Jsont.json)
         (Method.created r (Id.creation "new1")));
    Alcotest.(check bool)
      "a creation id the call did not name" true
      (Method.created r (Id.creation "new9") = None);
    let failed =
      decode_exn "set_response" json_set_response
        (proto_method "valid/set_response_with_errors.json")
    in
    Alcotest.(check bool)
      "a create that failed is not here" true
      (Method.created failed (Id.creation "new2") = None);
    Alcotest.(check bool)
      "and the one that succeeded is" true
      (Method.created failed (Id.creation "new1") <> None);
    let null =
      decode_fixture "set_response" json_set_response
        "set_response_null_state.json"
    in
    Alcotest.(check bool)
      "an explicit null created map" true
      (Method.created null (Id.creation "new1") = None)

  (* The failure of one create is reported under the same creation id in
     notCreated. *)
  let test_not_created () =
    let failed =
      decode_exn "set_response" json_set_response
        (proto_method "valid/set_response_with_errors.json")
    in
    Alcotest.(check bool)
      "the SetError of the create that failed" true
      (match Method.not_created failed (Id.creation "new2") with
      | Some e -> e.Error.Set_error.type_ = `Invalid_properties
      | None -> false);
    Alcotest.(check bool)
      "the create that succeeded is not here" true
      (Method.not_created failed (Id.creation "new1") = None);
    let null =
      decode_fixture "set_response" json_set_response
        "set_response_null_state.json"
    in
    Alcotest.(check bool)
      "an explicit null notCreated map" true
      (Method.not_created null (Id.creation "new2") = None)

  let mailbox i = { Mailbox.empty with Mailbox.id = Some (id_v i) }
  let mailbox_id (m : Mailbox.t) = m.Mailbox.id

  let names l =
    List.map
      (fun (m : Mailbox.t) ->
        match m.Mailbox.id with Some i -> Id.to_string i | None -> "?")
      l

  (* RFC 8620 Section 5.1 lets a /get answer in any order, so the order of the
     query that named the ids is put back here. *)
  let test_in_ids_order () =
    let l = [ mailbox "a"; mailbox "b"; mailbox "c" ] in
    Alcotest.(check (list string))
      "the order of the ids" [ "c"; "a"; "b" ]
      (names
         (Method.in_ids_order ~id:mailbox_id [ id_v "c"; id_v "a"; id_v "b" ] l));
    Alcotest.(check (list string))
      "an id the list does not answer" [ "a" ]
      (names (Method.in_ids_order ~id:mailbox_id [ id_v "a"; id_v "z" ] l));
    Alcotest.(check (list string))
      "a record the ids do not name" [ "b" ]
      (names (Method.in_ids_order ~id:mailbox_id [ id_v "b" ] l));
    Alcotest.(check (list string))
      "a record without an id" [ "a" ]
      (names
         (Method.in_ids_order ~id:mailbox_id
            [ id_v "a" ]
            [ Mailbox.empty; mailbox "a" ]));
    Alcotest.(check (list string))
      "no ids" []
      (names (Method.in_ids_order ~id:mailbox_id [] l))

  let tests =
    [
      ("created reads the creation id back", `Quick, test_created);
      ("not_created reads the creation id back", `Quick, test_not_created);
      ("set_failures concatenates the three", `Quick, test_set_failures);
      ("pp_set_failure", `Quick, test_pp_set_failure);
      ("in_ids_order restores the query order", `Quick, test_in_ids_order);
    ]
end

let () =
  Alcotest.run "JMAP methods, filters, blobs, push and errors"
    [
      ("Nullable members", Null_tests.tests);
      ("Query", Query_tests.tests);
      ("Comparator", Comparator_tests.tests);
      ("Filter", Filter_tests.tests);
      ("Push", Push_tests.tests);
      ("PatchObject", Patch_tests.tests);
      ("SetError", Set_error_tests.tests);
      ("Error", Error_tests.tests);
      ("Creation references", Creation_ref_tests.tests);
      ("Response helpers", Response_helper_tests.tests);
    ]
