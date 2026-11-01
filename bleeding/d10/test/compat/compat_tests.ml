module Plan = D10ir.Plan

let get = function Ok x -> x | Error e -> Alcotest.fail e

let test_legacy_node () =
  let node =
    get
      (Plan.decode_node
         {|{
    "package":{"name":"demo","version":"1"}, "layer_hash":"abc",
    "archive":{"path":"source.tar","sha256":"def"},
    "script":"true", "prefix":"/prefix"
  }|})
  in
  Alcotest.(check (list string)) "absent env" [] node.env;
  Alcotest.(check int) "absent strip count" 1 node.archive.strip_components;
  Alcotest.(check string) "absent source digest" "" node.opam_file_sha256;
  let encoded = Plan.encode_node node in
  Alcotest.(check string)
    "round trip" encoded
    (Plan.encode_node (get (Plan.decode_node encoded)))

let test_legacy_plan () =
  let plan =
    get
      (Plan.of_string
         (Printf.sprintf
            {|{
    "schema_version":%d, "os_key":"test",
    "toolchain":{"name":"ox","base_layer":"abc"},
    "nodes":[], "roots":[]
  }|}
            Plan.current_schema_version))
  in
  Alcotest.(check string) "default archive root" "archives" plan.archive_root;
  Alcotest.(check int)
    "default metadata" 0
    (List.length plan.metadata.cli_invocation);
  let encoded = Plan.to_string plan in
  Alcotest.(check string)
    "plan round trip" encoded
    (Plan.to_string (get (Plan.of_string encoded)))

let () =
  Alcotest.run "d10 IR compatibility"
    [
      ( "compatibility",
        [
          Alcotest.test_case "legacy node defaults" `Quick test_legacy_node;
          Alcotest.test_case "legacy plan defaults" `Quick test_legacy_plan;
        ] );
    ]
