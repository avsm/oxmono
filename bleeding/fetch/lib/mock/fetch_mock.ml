open Fetch

let client fn : Fetch.plain = Fetch.Middleware.of_handler (fun ~sw:_ req -> fn req)

let respond
  ?(status = 200)
  ?headers
  ?(version = `HTTP_1_1)
  body
  (req : Middleware.request)
  =
  let headers = Option.value headers ~default:(Http.Header.init ()) in
  Fetch.Middleware.Pi.response ~close:(fun () -> ()) ~status ~headers ~version
    ~body:(Eio.Flow.string_source body)
    ~url:req.url
    ()
;;

module Sse = struct
  type sink = Buffer.t

  let emit sink value = Buffer.add_string sink value
  let send sink ?name ?id data = Httpz.Sse.send (emit sink) ?name ?id data
  let comment sink text = Httpz.Sse.comment (emit sink) text
  let retry sink milliseconds = Httpz.Sse.retry (emit sink) milliseconds

  let respond ?status ?headers ?version ?retry:retry_ms write req =
    let body = Buffer.create 256 in
    Option.iter (retry body) retry_ms;
    write body;
    let headers =
      Option.value headers ~default:(Http.Header.init ())
      |> fun headers ->
      Http.Header.replace headers "Content-Type" Httpz.Sse.media_type
      |> fun headers -> Http.Header.replace headers "Cache-Control" "no-store"
    in
    respond ?status ~headers ?version (Buffer.contents body) req
  ;;
end
