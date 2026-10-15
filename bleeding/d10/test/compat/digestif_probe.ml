let () =
  let key = String.make 32 'k' in
  List.iter
    (fun input ->
      let digest = Digestif.digest_string_raw `BLAKE3 input in
      assert (Digestif.digest_size `BLAKE3 = 32);
      assert (digest = Digestif.BLAKE3.(to_raw_string (digest_string input)));
      let hmac = Digestif.hmacv_string_raw `BLAKE3 ~key [ input; "tail" ] in
      assert (
        hmac
        = Digestif.BLAKE3.(to_raw_string (hmacv_string ~key [ input; "tail" ]))))
    [ ""; "abc"; String.init 2049 (fun i -> Char.chr (i mod 256)) ];
  print_endline "Portable BLAKE3 helpers match the upstream module API"
