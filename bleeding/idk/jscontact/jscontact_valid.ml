(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = ('a, string) result

let ok v = Ok v
let error fmt = Format.kasprintf (fun s -> Error s) fmt

let check cond fmt =
  Format.kdprintf
    (fun k -> if cond then Ok () else Error (Format.asprintf "%t" k))
    fmt

let ( let* ) = Result.bind
let in_ prop = function Ok _ as v -> v | Error msg -> Error (prop ^ ": " ^ msg)

let opt validate = function
  | None -> Ok None
  | Some v -> Result.map Option.some (validate v)

let list validate l =
  let rec go i = function
    | [] -> Ok l
    | v :: vs -> (
        match validate v with
        | Ok _ -> go (i + 1) vs
        | Error msg -> Error (Printf.sprintf "[%d]: %s" i msg))
  in
  go 0 l

let map_entries key validate m =
  let rec go = function
    | [] -> Ok m
    | (k, v) :: es -> (
        match validate v with
        | Ok _ -> go es
        | Error msg -> Error (Printf.sprintf "%S: %s" (key k) msg))
  in
  go m

let entries validate m = map_entries Jscontact_id.to_string validate m
let string_entries validate m = map_entries Fun.id validate m
