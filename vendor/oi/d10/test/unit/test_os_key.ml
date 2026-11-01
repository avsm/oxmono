let key kind version =
  let platform : Osrel.t =
    { arch = `Arm64; os = { kind; version; family = "unused" }; jobs = 1 }
  in
  D10.Os_key.(to_string (of_platform platform))

let partition () =
  let debian = key (`Linux `Debian) "13" in
  Alcotest.(check string) "debian" "debian~13~arm64" debian;
  Alcotest.(check bool)
    "distribution separates caches" false
    (debian = key (`Linux `Ubuntu) "13");
  Alcotest.(check bool)
    "version separates caches" false
    (debian = key (`Linux `Debian) "12");
  Alcotest.(check string)
    "macOS major" "macos~26~arm64"
    (key (`MacOS `Homebrew) "26.1");
  Alcotest.(check string)
    "Alpine series" "alpine~3.23~arm64"
    (key (`Linux `Alpine) "3.23.2")

let suite =
  ("platform keys", [ Alcotest.test_case "cache partitions" `Quick partition ])
