(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Y = Yamlrw_eio

let value = Alcotest.testable Yamlrw.pp ( = )

let sample : Yamlrw.value =
  `O
    [
      ("name", `String "yamlrw");
      ("tags", `A [ `String "a"; `String "b" ]);
      ("count", `Float 3.);
      ("nested", `O [ ("ok", `Bool true); ("none", `Null) ]);
    ]

(* Nine to the fourth power nodes once the aliases are expanded. *)
let bomb =
  {|a: &a [1, 2, 3, 4, 5, 6, 7, 8, 9]
b: &b [*a, *a, *a, *a, *a, *a, *a, *a, *a]
c: &c [*b, *b, *b, *b, *b, *b, *b, *b, *b]
d: &d [*c, *c, *c, *c, *c, *c, *c, *c, *c]
|}

let test_file_round_trip env () =
  let fs = Eio.Stdenv.cwd env in
  Y.to_file ~fs "round_trip.yml" sample;
  Alcotest.check value "read back" sample (Y.of_file ~fs "round_trip.yml")

let test_flow_round_trip () =
  let buf = Buffer.create 256 in
  Y.Write.value (Eio.Flow.buffer_sink buf) sample;
  Alcotest.check value "flow" sample
    (Y.Read.value (Eio.Flow.string_source (Buffer.contents buf)))

let test_node_limit () =
  match Y.Read.value ~max_nodes:100 (Eio.Flow.string_source bomb) with
  | _ -> Alcotest.fail "alias bomb accepted"
  | exception Yamlrw.Yamlrw_error e -> (
      match e.kind with
      | Yamlrw.Error.Alias_expansion_node_limit _ -> ()
      | _ -> Alcotest.failf "unexpected %s" (Yamlrw.Error.to_string e))

let test_documents () =
  let docs =
    Y.Read.documents (Eio.Flow.string_source "--- 1\n--- two\n--- [3]\n")
  in
  Alcotest.(check int) "three documents" 3 (List.length docs);
  let n =
    Y.Read.fold_documents
      (fun n _ -> n + 1)
      0
      (Eio.Flow.string_source "--- a\n--- b\n")
  in
  Alcotest.(check int) "fold" 2 n

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "yamlrw-eio"
    [
      ( "eio",
        [
          Alcotest.test_case "file round trip" `Quick (test_file_round_trip env);
          Alcotest.test_case "flow round trip" `Quick test_flow_round_trip;
          Alcotest.test_case "alias node limit" `Quick test_node_limit;
          Alcotest.test_case "documents" `Quick test_documents;
        ] );
    ]
