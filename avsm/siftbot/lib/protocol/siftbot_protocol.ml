module S = Sexplib.Sexp
module M = Siftbot_model

let max_bytes = 4 * 1024 * 1024

let encode payload =
  let s = S.to_string_mach (S.List [ Atom "siftbot/v1"; payload ]) ^ "\n" in
  if String.length s > max_bytes then invalid_arg "Worker message too large";
  s

let decode f s =
  if String.length s > max_bytes then Error "Worker message too large"
  else
    try
      match S.of_string s with
      | List [ Atom "siftbot/v1"; payload ] -> Ok (f payload)
      | _ -> Error "Incompatible Siftbot protocol"
    with _ -> Error "Invalid Siftbot message"

let request r = encode (M.sexp_of_request r)
let response r = encode (M.sexp_of_response r)
let parse_request s = decode M.request_of_sexp s
let parse_response s = decode M.response_of_sexp s

let read ic =
  let b = Buffer.create 4096 in
  let rec loop n =
    if n > max_bytes then invalid_arg "Worker message too large";
    match input_char ic with
    | '\n' -> Buffer.contents b
    | c ->
        Buffer.add_char b c;
        loop (n + 1)
    | exception End_of_file -> Buffer.contents b
  in
  loop 0
