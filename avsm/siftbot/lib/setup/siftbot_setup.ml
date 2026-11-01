open Sexplib.Std

type profile = {
  name : string;
  jmap_profile : string;
  url : string;
  account : string;
  chat_model : string;
  has_token : bool;
}
[@@deriving sexp]

type request = List | Save of profile * string * bool [@@deriving sexp]

type response = Profiles of profile list | Saved of profile | Error of string
[@@deriving sexp]

let max_bytes = 65536

let encode sexp =
  let raw =
    Sexplib.Sexp.to_string_mach (List [ Atom "siftbot-setup/v1"; sexp ]) ^ "\n"
  in
  if String.length raw > max_bytes then invalid_arg "Setup message too large";
  raw

let decode f raw =
  if String.length raw > max_bytes then invalid_arg "Setup message too large";
  match Sexplib.Sexp.of_string raw with
  | List [ Atom "siftbot-setup/v1"; payload ] -> f payload
  | _ -> invalid_arg "Invalid setup message"

let request x = encode (sexp_of_request x)
let response x = encode (sexp_of_response x)

let parse_request raw =
  try Ok (decode request_of_sexp raw) with _ -> Error "Invalid setup request"

let parse_response raw =
  try decode response_of_sexp raw with _ -> Error "Invalid setup response"

let read ic =
  let b = Buffer.create 1024 in
  let rec loop () =
    match input_char ic with
    | '\n' -> Buffer.contents b
    | c ->
        if Buffer.length b >= max_bytes then
          invalid_arg "Setup message too large";
        Buffer.add_char b c;
        loop ()
    | exception End_of_file -> Buffer.contents b
  in
  loop ()
