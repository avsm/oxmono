open Fetch

let client fn : Fetch.plain =
  Fetch.Middleware.of_handler (fun ~sw:_ req -> fn req)

let respond ?(status = 200) ?headers ?(version = `HTTP_1_1) body
    (req : Middleware.request) =
  let headers = match headers with Some h -> h | None -> Http.Header.init () in
  Fetch.Middleware.Pi.response ~status ~headers ~version
    ~body:(Eio.Flow.string_source body)
    ~url:req.url ()
