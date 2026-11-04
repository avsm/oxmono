let codec json = Duraml_client.Codec.v
  ~encode:(fun value -> match Jsont_bytesrw.encode_string json value with
    | Ok data -> data
    | Error _ -> invalid_arg "Duraml JSON encoding failed")
  ~decode:(fun data -> Jsont_bytesrw.decode_string json data)
