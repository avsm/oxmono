(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let is_alpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c
let is_graphic c = c > '\x20' && c <> '\x7f'
