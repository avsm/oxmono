let () =
  let request = Bytes.of_string "GET / HTTP/1.1\r\nHost: example.test\r\n\r\n" in
  let #(status, _, _) =
    Httpz.parse request
      ~len:(Stdlib_stable.Int16_u.of_int (Bytes.length request))
      ~limits:Httpz.default_limits
  in
  assert (status = Httpz.Buf_read.Complete)
