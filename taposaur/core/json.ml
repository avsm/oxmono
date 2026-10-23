let checked ~kind check =
  Jsont.map ~kind ~enc:Fun.id ~dec:(fun v ->
    match check v with
    | Ok () -> v
    | Error e -> Jsont.Error.msg Jsont.Meta.none e)

let version =
  checked ~kind:"Schema version"
    (fun v -> if v = 1 then Ok () else Error "Unsupported schema version")
    Jsont.int

let filename =
  checked ~kind:"Artifact filename" (fun s ->
    if s = "" || s = "." || s = ".." ||
      String.exists (function '/' | '\\' | '\000' -> true | _ -> false) s
    then Error "Expected a filename within the paper directory"
    else Ok ()) Jsont.string
