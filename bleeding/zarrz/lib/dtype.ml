(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float16
  | Bfloat16
  | Float32
  | Float64
  | Complex64
  | Complex128
  | Raw of int

let size = function
  | Bool | Int8 | Uint8 -> 1
  | Int16 | Uint16 | Float16 | Bfloat16 -> 2
  | Int32 | Uint32 | Float32 -> 4
  | Int64 | Uint64 | Float64 | Complex64 -> 8
  | Complex128 -> 16
  | Raw n -> n

let name = function
  | Bool -> "bool"
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Uint8 -> "uint8"
  | Uint16 -> "uint16"
  | Uint32 -> "uint32"
  | Uint64 -> "uint64"
  | Float16 -> "float16"
  | Bfloat16 -> "bfloat16"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Complex64 -> "complex64"
  | Complex128 -> "complex128"
  | Raw n -> Printf.sprintf "r%d" (n * 8)

let of_name = function
  | "bool" -> Some Bool
  | "int8" -> Some Int8
  | "int16" -> Some Int16
  | "int32" -> Some Int32
  | "int64" -> Some Int64
  | "uint8" -> Some Uint8
  | "uint16" -> Some Uint16
  | "uint32" -> Some Uint32
  | "uint64" -> Some Uint64
  | "float16" -> Some Float16
  | "bfloat16" -> Some Bfloat16
  | "float32" -> Some Float32
  | "float64" -> Some Float64
  | "complex64" -> Some Complex64
  | "complex128" -> Some Complex128
  | s ->
      let is_digits s =
        s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s
      in
      if String.length s > 1 && s.[0] = 'r'
         && is_digits (String.sub s 1 (String.length s - 1))
      then
        match int_of_string_opt (String.sub s 1 (String.length s - 1)) with
        | Some bits when bits > 0 && bits mod 8 = 0 -> Some (Raw (bits / 8))
        | Some _ | None -> None
      else None

let equal a b =
  match (a, b) with Raw x, Raw y -> x = y | a, b -> a = b

let pp ppf t = Format.pp_print_string ppf (name t)
