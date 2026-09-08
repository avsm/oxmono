let encode codec value =
  Jsont_bytesrw.encode_string' codec value
  |> Result.map_error (fun e -> Error.Json e)

let decode codec value =
  Jsont.Json.decode' codec value |> Result.map_error (fun e -> Error.Json e)

let decode_string codec value =
  Jsont_bytesrw.decode_string' ~locs:true codec value
  |> Result.map_error (fun e -> Error.Json e)
