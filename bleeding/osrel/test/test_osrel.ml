let test_platform () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (Osrel.Arch.to_string (Osrel.Arch.of_string input)))
    [
      ("aarch64", "arm64");
      ("amd64", "x86_64");
      ("armv7l", "arm32");
      ("riscv64", "riscv64");
    ];
  Eio_main.run @@ fun env ->
  let platform =
    Osrel.detect ~proc_mgr:(Eio.Stdenv.process_mgr env) ~fs:(Eio.Stdenv.fs env)
  in
  Alcotest.(check bool) "positive job count" true (platform.jobs > 0)

let () =
  Alcotest.run "osrel"
    [ ("platform", [ Alcotest.test_case "detection" `Quick test_platform ]) ]
