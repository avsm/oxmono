(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcobar.run "console"
    [
      Fuzz_view.suite;
      Fuzz_width.suite;
      Fuzz_console_text.suite;
      Fuzz_color.suite;
      Fuzz_style.suite;
      Fuzz_border.suite;
      Fuzz_guide.suite;
      Fuzz_spinner.suite;
      Fuzz_theme.suite;
      Fuzz_bar.suite;
      Fuzz_rule.suite;
      Fuzz_panel.suite;
      Fuzz_table.suite;
      Fuzz_tree.suite;
      Fuzz_input.suite;
      Fuzz_display.suite;
      Fuzz_progress.suite;
      Fuzz_console.suite;
    ]
