(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let register_pp pp extract =
  Eio.Exn.register_pp (fun ppf err ->
      match extract err with
      | Some e ->
          pp ppf e;
          true
      | None -> false)

let raise_ err = raise (Eio.Exn.create err)
