(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let with_open_in ~subject ~refused path f =
  try Eioext.Path.with_open_private path f
  with Eioext.Path.Error message -> refused (Fmt.str "%s: %s" subject message)
