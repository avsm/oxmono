(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let error fmt = Format.kasprintf (fun s -> Error s) fmt

let check cond fmt =
  Format.kdprintf
    (fun k -> if cond then Ok () else Error (Format.asprintf "%t" k))
    fmt

let ( let* ) = Result.bind
