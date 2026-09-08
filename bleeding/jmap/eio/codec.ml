(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let encode ?format jsont value = Jmap.Proto.Json.encode ?format jsont value

let decode ?locs ?max_depth jsont json =
  Jmap.Proto.Json.decode ?locs ?max_depth jsont json

let encode_exn ?format jsont value =
  match encode ?format jsont value with
  | Ok s -> s
  | Error e -> raise (Jsont.Error e)

let decode_exn ?locs ?max_depth jsont json =
  match decode ?locs ?max_depth jsont json with
  | Ok v -> v
  | Error e -> raise (Jsont.Error e)
