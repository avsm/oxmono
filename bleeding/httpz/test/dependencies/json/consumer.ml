let () =
  assert (Httpz_media.Json.decode_string' Jsont.int "42" = Ok 42);
  assert
    (Result.is_error
       (Httpz_media.Json.decode_string' ~max_depth:1 Jsont.json "[[0]]"))
