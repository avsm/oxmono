let encode s = Base64.encode_string ~pad:false s
let invalid () = Error (`Msg "invalid base64")

(* [Base64.decode] is deliberately permissive about some inputs.  Matrix
   permits exactly the standard alphabet, in either its canonical padded or
   unpadded spelling, so validate the spelling before handing it to the
   package decoder and check its canonical re-encoding afterwards. *)
let decode s =
  let len = String.length s in
  let padding =
    match String.index_opt s '=' with None -> 0 | Some i -> len - i
  in
  let padded = padding <> 0 in
  let syntax_ok =
    if padded then
      len mod 4 = 0
      && (padding = 1 || padding = 2)
      && String.for_all
           (fun c ->
             (c >= 'A' && c <= 'Z')
             || (c >= 'a' && c <= 'z')
             || (c >= '0' && c <= '9')
             || c = '+' || c = '/' || c = '=')
           s
      &&
      let first = len - padding in
      String.for_all (fun c -> c <> '=') (String.sub s 0 first)
      && String.for_all (fun c -> c = '=') (String.sub s first padding)
    else
      let rem = len mod 4 in
      rem <> 1
      && String.for_all
           (fun c ->
             (c >= 'A' && c <= 'Z')
             || (c >= 'a' && c <= 'z')
             || (c >= '0' && c <= '9')
             || c = '+' || c = '/')
           s
  in
  if not syntax_ok then invalid ()
  else
    let normalized =
      if padded then Some s
      else
        match len mod 4 with
        | 0 -> Some s
        | 2 -> Some (s ^ "==")
        | 3 -> Some (s ^ "=")
        | _ -> None
    in
    match normalized with
    | None -> invalid ()
    | Some normalized -> (
        match Base64.decode ~pad:true normalized with
        | Error _ -> invalid ()
        | Ok decoded ->
            let canonical = Base64.encode_string ~pad:padded decoded in
            if String.equal s canonical then Ok decoded else invalid ())

let decode_opt s = Result.to_option (decode s)
