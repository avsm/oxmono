let () =
  Alcotest.run "console_eio"
    [ Test_display.suite; Test_console_eio.suite; Test_prompt.suite ]
