(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val request : Termanil_model.request -> string
val response : (Termanil_model.response, string) result -> string
val parse_request : string -> (Termanil_model.request, string) result

val parse_response :
  string -> ((Termanil_model.response, string) result, string) result
(** The wire format is a versioned S-expression on one line. Parsers reject
    messages larger than 64 MiB and unsupported protocol versions. Credentials
    never form part of requests or responses. *)

val max_bytes : int
