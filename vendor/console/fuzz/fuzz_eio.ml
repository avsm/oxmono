(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let suite = Fuzz_console_eio.suite
let () = Alcobar.run "console-eio" [ suite ]
