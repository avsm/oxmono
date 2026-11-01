(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Metadata of string
  | Store of string
  | Codec of string
  | Checksum_mismatch of { expected : int32; got : int32 }

exception E of t

let raise_ e = raise (E e)

let pp ppf = function
  | Metadata m -> Format.fprintf ppf "metadata: %s" m
  | Store m -> Format.fprintf ppf "store: %s" m
  | Codec m -> Format.fprintf ppf "codec: %s" m
  | Checksum_mismatch { expected; got } ->
      Format.fprintf ppf "checksum mismatch: expected %08lx, got %08lx"
        expected got

let to_string e = Format.asprintf "%a" pp e
