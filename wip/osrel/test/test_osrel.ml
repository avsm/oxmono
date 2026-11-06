let test_platform () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (Osrel.Arch.to_string (Osrel.Arch.of_string input)))
    [
      ("aarch64", "arm64");
      ("AMD64", "x86_64");
      ("amd64", "x86_64");
      ("i686", "x86_32");
      ("armv7l", "arm32");
      ("earmv6hf", "arm32");
      ("ppc64le", "ppc64");
      ("powerpc", "ppc32");
      ("riscv64", "riscv64");
      ("loongarch64", "loongarch64");
    ];
  Alcotest.(check bool)
    "unknown architecture retained" true
    (Osrel.Arch.of_string "Mips" = `Unknown "mips");
  Eio_main.run @@ fun env ->
  let platform =
    Osrel.detect ~proc_mgr:(Eio.Stdenv.process_mgr env) ~fs:(Eio.Stdenv.fs env)
  in
  Alcotest.(check bool) "positive job count" true (platform.jobs > 0)

let () =
  Alcotest.run "osrel"
    [ ("platform", [ Alcotest.test_case "detection" `Quick test_platform ]) ]
