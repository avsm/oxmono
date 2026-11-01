(* End-to-end allocation includes the client, Eio, clocks and socket I/O.
   Use bench_alloc for the isolated framework cost. This benchmark keeps one
   loopback connection open and excludes connection setup and warmup. *)
open Proffer

let site =
  Site.of_routes
    [ Route.(get (s "hello")) (fun () _req respond -> Resp.text respond "hi") ]

let request = Cstruct.of_string "GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n"

let allocated_words () =
  let s = Gc.stat () in
  s.Gc.minor_words +. s.Gc.major_words -. s.Gc.promoted_words

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1000
  in
  if n <= 0 then invalid_arg "request count must be positive";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bound, set_bound = Eio.Promise.create () in
  let stop, set_stop = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Proffer_httpz.run ~sw ~port:0 ~stop
        ~on_listening:(Eio.Promise.resolve set_bound)
        env ~env:() site);
  let addr = Eio.Promise.await bound in
  Eio.Switch.run (fun client_sw ->
      let flow = Eio.Net.connect ~sw:client_sw env#net addr in
      let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
      let roundtrip () =
        Eio.Flow.write flow [ request ];
        if Eio.Buf_read.line reader <> "HTTP/1.1 200 OK" then
          failwith "unexpected response status";
        let rec headers () =
          if Eio.Buf_read.line reader <> "" then headers ()
        in
        headers ();
        if Eio.Buf_read.take 2 reader <> "hi" then
          failwith "unexpected response body"
      in
      for _ = 1 to 100 do
        roundtrip ()
      done;
      Gc.full_major ();
      let before = allocated_words () in
      let start = Eio.Time.Mono.now env#mono_clock in
      for _ = 1 to n do
        roundtrip ()
      done;
      let stop = Eio.Time.Mono.now env#mono_clock in
      let elapsed = Mtime.Span.to_float_ns (Mtime.span start stop) in
      Gc.full_major ();
      let allocated = allocated_words () -. before in
      Printf.printf
        "HTTP loopback keep-alive (client + server), %d requests:\n\
         %.1f allocated words/request, %.1f us/request\n\
         %!"
        n
        (allocated /. float n)
        (elapsed /. 1000. /. float n));
  Eio.Promise.resolve set_stop ()
