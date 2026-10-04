let () =
  Alcotest.run "d10"
    [ Test_lock.suite; Test_layer_rewrite.suite; Test_sysops.suite ]
