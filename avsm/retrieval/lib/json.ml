let encode codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error e -> invalid_arg e

let decode codec s =
  match Jsont_bytesrw.decode_string codec s with
  | Ok v -> v
  | Error e -> invalid_arg e
