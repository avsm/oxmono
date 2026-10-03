(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcotest.run "console"
    [
      Test_color.suite;
      Test_gradient.suite;
      Test_canvas.suite;
      Test_style.suite;
      Test_span.suite;
      Test_tree.suite;
      Test_rule.suite;
      Test_panel.suite;
      Test_border.suite;
      Test_guide.suite;
      Test_bar.suite;
      Test_theme.suite;
      Test_spinner.suite;
      Test_anim.suite;
      Test_layout.suite;
      Test_width.suite;
      Test_input.suite;
      Test_console.suite;
    ]
