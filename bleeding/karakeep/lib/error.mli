(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  message : string;
  status_code : int;
  code : string;
}

val of_api_error : Openapi.Runtime.api_error -> t option
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val is_auth_error : t -> bool
val is_not_found : t -> bool
val handle_exn : exn -> int
val run : (unit -> unit) -> int

exception Exit_code of int

val wrap : (unit -> unit) -> unit
