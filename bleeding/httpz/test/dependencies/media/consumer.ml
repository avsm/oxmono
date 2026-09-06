let () =
  let fields = [ "name", "a b" ] in
  let encoded = Httpz_media.encode Httpz_media.form fields in
  assert (encoded = "name=a+b");
  assert (Httpz_media.decode Httpz_media.form encoded = Ok fields)
