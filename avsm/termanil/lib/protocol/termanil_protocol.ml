(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module S = Sexplib.Sexp
module M = Termanil_model

let max_bytes = 64 * 1024 * 1024

let encode payload =
  let raw = S.to_string_mach (S.List [ Atom "termanil/v4"; payload ]) in
  if String.length raw > max_bytes then failwith "Worker message exceeds 64 MiB";
  raw ^ "\n"

let request r = encode (M.sexp_of_request r)

let response r =
  encode
    (match r with
    | Ok r -> S.List [ Atom "ok"; M.sexp_of_response r ]
    | Error s -> S.List [ Atom "error"; Atom s ])

let decode f raw =
  if String.length raw > max_bytes then Error "Worker message exceeds 64 MiB"
  else
    try
      match S.of_string raw with
      | List [ Atom "termanil/v4"; payload ] -> Ok (f payload)
      | _ -> Error "Incompatible termanil worker protocol"
    with _ -> Error "Invalid termanil worker response"

let parse_request raw = decode M.request_of_sexp raw

let parse_response raw =
  decode
    (function
      | S.List [ Atom "ok"; payload ] -> Ok (M.response_of_sexp payload)
      | S.List [ Atom "error"; Atom s ] -> Error s
      | _ -> failwith "invalid result")
    raw
