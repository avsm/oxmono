(* The AT Protocol data model writes [$bytes] as base64 without padding. Real
   records carry that form, so a decoder that requires padding rejects them. *)

let check name p = if not p then failwith name
let get = function Ok v -> v | Error e -> failwith e
let unpadded = "hfqO9uUkXe4qt0XtpiY8kX9iIJw"
let wrap b64 = Printf.sprintf {|{"$bytes":"%s"}|} b64
let decode codec s = Jsont_bytesrw.decode_string codec s
let encode codec v = get (Jsont_bytesrw.encode_string codec v)

let () =
  let bytes = get (decode Atp.Lex.bytes_jsont (wrap unpadded)) in
  check "unpadded base64 decodes" (String.length bytes = 20);
  check "padded base64 decodes to the same bytes"
    (get (decode Atp.Lex.bytes_jsont (wrap (unpadded ^ "="))) = bytes);
  check "bytes are written without padding"
    (encode Atp.Lex.bytes_jsont bytes = wrap unpadded);
  check "bytes round trip"
    (get (decode Atp.Lex.bytes_jsont (encode Atp.Lex.bytes_jsont bytes))
    = bytes);
  check "not base64 is rejected"
    (match decode Atp.Lex.bytes_jsont (wrap "!!!!") with
    | Error _ -> true
    | Ok _ -> false);
  (* Input that cannot be base64 is rejected, as it was before padding became
     optional. *)
  let rejected s =
    match decode Atp.Lex.bytes_jsont (wrap s) with
    | Error _ -> true
    | Ok _ -> false
  in
  check "one character is not base64" (rejected "a");
  check "too much padding is rejected" (rejected "aGk===");
  check "padding in the middle is rejected" (rejected "aG=k");
  check "a length of one more than a multiple of four is rejected"
    (rejected "aGVsb");
  check "a short unpadded final group is accepted"
    (get (decode Atp.Lex.bytes_jsont (wrap "aGk")) = "hi");
  (match decode Atp.Lex.jsont (wrap "a") with
  | Ok (`Bytes _) -> failwith "a malformed $bytes became bytes"
  | _ -> ());
  (* The generic value path takes the same two forms. *)
  (match get (decode Atp.Lex.jsont (wrap unpadded)) with
  | `Bytes b -> check "a generic value holds the bytes" (b = bytes)
  | _ -> failwith "an unpadded $bytes is not bytes");
  check "a generic value is written without padding"
    (encode Atp.Lex.jsont (`Bytes bytes) = wrap unpadded);
  print_endline "ok"
