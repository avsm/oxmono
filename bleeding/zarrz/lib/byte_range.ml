(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | From_start of { off : int; len : int option }
  | Suffix of int

let resolve ~size r =
  if size < 0 then invalid_arg "Byte_range.resolve: negative size";
  match r with
  | From_start { off; len } ->
      if off < 0 then invalid_arg "Byte_range.resolve: negative offset";
      let start = if off > size then size else off in
      let avail = size - start in
      let length =
        match len with
        | None -> avail
        | Some l ->
            if l < 0 then invalid_arg "Byte_range.resolve: negative length";
            if l > avail then avail else l
      in
      (start, length)
  | Suffix n ->
      if n < 0 then invalid_arg "Byte_range.resolve: negative suffix";
      let start = if n > size then 0 else size - n in
      (start, size - start)

let pp ppf = function
  | From_start { off; len = None } -> Format.fprintf ppf "bytes %d-" off
  | From_start { off; len = Some l } ->
      Format.fprintf ppf "bytes %d-%d" off (off + l - 1)
  | Suffix n -> Format.fprintf ppf "bytes -%d" n
