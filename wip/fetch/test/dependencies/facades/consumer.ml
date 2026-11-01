let from_fetch : int Proffer.Media.t = Fetch.Json.v Jsont.int
let from_proffer : int Fetch.Media.t = Proffer.Json.v Jsont.int
let markdown : _ Fetch.Media.t = Proffer.Markdown.markdown ()

let () =
  assert (Proffer.Media.decode from_fetch "42" = Ok 42);
  assert (Fetch.Media.decode from_proffer "42" = Ok 42);
  assert (Proffer.Media.can_decode markdown);
  match Proffer.Media.decode from_fetch "null" with
  | Error (Fetch.Media.Malformed { detail = Proffer.Json.Error _; _ }) -> ()
  | _ -> failwith "Fetch and Proffer must share the Jsont detail constructor"

let delay : Duration.t = Duration.of_ms 500
let retry = Fetch.Retry.v ~backoff_factor:delay ()
let close_subscription = Fetch.Sse.close
let () = assert (Duration.to_ms retry.backoff_factor = 500)
