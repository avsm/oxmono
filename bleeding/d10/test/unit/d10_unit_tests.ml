let () =
  Alcotest.run "d10"
    [
      Test_os_key.suite;
      Test_lock.suite;
      Test_layer_rewrite.suite;
      Test_sysops.suite;
      Test_makefile.suite;
    ]
