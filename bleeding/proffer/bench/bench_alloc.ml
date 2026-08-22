(* What a request costs on the heap.

   TODO.md quotes these numbers to decide where effort goes, so they need an
   oracle rather than a throwaway. This is a tool, not a test: the figures
   shift with the compiler and pinning them would fail a build for a reason
   nobody wants to chase. Run it and read it.

   The writer is a null one. A real backend's socket write is not what is
   being counted, and neither is the body: the point is what the machinery
   around the body costs, which is what the modes work bought down. *)

open Proffer

let compiled =
  Compiled.compile
    (Site.of_routes
       [ Route.(get (s "hello" /? nil)) (fun () _req respond ->
             Resp.text respond "hi")
       ])

(* The block is written [stack_] and the field built with [h_local], which is
   the pair that keeps it off the heap. Written any other way this row is not
   zero, which is the whole point of the row. *)
let header_site =
  Compiled.compile
    (Site.of_routes
       [ Route.(get (s "hello" /? nil)) (fun () _req respond ->
             let () =
               Resp.v respond ~content_type:"text/plain"
                 ~headers:
                   (stack_
                      [ Resp.h_local Httpz.Header_name.X_cache "hit" ])
                 (Body.String "hi")
             in
             ())
       ])

let etag_site =
  Compiled.compile
    (Site.of_routes
       [ Route.(get (s "hello" /? nil)) (fun () _req respond ->
             Resp.html respond ~etag:(`Strong "v1") "hi")
       ])

(* A stream, to see what the streaming path adds over a string body. The
   producer hands over bytes, which is the path an encoder takes. The buffer
   is made inside the handler because [bytes] is mutable, so a module-level
   one is [contended] and a portable handler cannot read it. A real encoder
   allocates its slice per encode for the same reason. *)
let stream_site =
  Compiled.compile
    (Site.of_routes
       [ Route.(get (s "hello" /? nil)) (fun () _req respond ->
             let payload = Bytes.of_string "hi" in
             Resp.stream respond "text/plain" (fun sink ->
                 Body.Sink.write_sub sink payload ~off:0 ~len:2))
       ])

let null_writer (_ : Backend.outcome @ local) = ()

let stream_writer (o : Backend.outcome @ local) =
  match o.body with
  | `Stream (_, write) ->
      write (Backend.sink ~emit_sub:(fun _ _ _ -> ()) (fun _ -> ()))
  | _ -> ()

(* [Gc.quick_stat] reads a per-domain counter that a large allocation does not
   flush, so the major heap is taken from [Gc.stat] with a collection either
   side. The loop runs the body [n] times and divides, since one iteration is
   below the resolution of a forced collection. *)
let words ~n f =
  Gc.full_major ();
  let s = Gc.stat () in
  let a = s.Gc.minor_words +. s.Gc.major_words in
  for _ = 1 to n do
    f ()
  done;
  Gc.full_major ();
  let s = Gc.stat () in
  let b = s.Gc.minor_words +. s.Gc.major_words in
  (b -. a) /. float n

let serve ?headers ?(target = "/hello") ?(meth = Httpz.Method.Get)
    ?(site = compiled) ?(writer = null_writer) () =
  let () =
    let local_ req = Req.v ~meth ~target ?headers () in
    Backend.handle site () req writer
  in
  ()

let row name v = Printf.printf "%-46s %6.1f words\n" name v

let () =
  let n = 20_000 in
  row "full serve, literal route, content type only"
    (words ~n (fun () -> serve ()));
  row "the same with an entity-tag"
    (words ~n (fun () -> serve ~site:etag_site ()));
  row "the same with a header field in a stack_ block"
    (words ~n (fun () -> serve ~site:header_site ()));
  row "a 404" (words ~n (fun () -> serve ~target:"/nope" ()));
  row "a streamed body, written"
    (words ~n (fun () -> serve ~site:stream_site ~writer:stream_writer ()));
  row "Req.v alone"
    (words ~n (fun () ->
         let () =
           let local_ req = Req.v ~meth:Httpz.Method.Get ~target:"/hello" () in
           ignore (Sys.opaque_identity (Req.path req))
         in
         ()));
  (* The query rows subtract a [Req.v] over the same target, so what is left
     is the lookup and not the request it runs against. *)
  let base =
    words ~n (fun () ->
        let () =
          let local_ req =
            Req.v ~meth:Httpz.Method.Get ~target:"/hello?a=b" ()
          in
          ignore (Sys.opaque_identity (Req.path req))
        in
        ())
  in
  row "one Req.query_param, present"
    (words ~n (fun () ->
         let () =
           let local_ req =
             Req.v ~meth:Httpz.Method.Get ~target:"/hello?a=b" ()
           in
           ignore (Sys.opaque_identity (Req.query_param req "a"))
         in
         ())
    -. base);
  row "one Req.query_param, absent"
    (words ~n (fun () ->
         let () =
           let local_ req =
             Req.v ~meth:Httpz.Method.Get ~target:"/hello?a=b" ()
           in
           ignore (Sys.opaque_identity (Req.query_param req "z"))
         in
         ())
    -. base)

(* Where the words in the rows above go. Each line is the one above it plus a
   single named thing, so the difference is what that thing costs. Read this
   before trying to shave the serving path: the machinery is already free and
   what is left is boxes the interface asks for. *)
let () =
  print_newline ();
  let n = 20_000 in
  let run_with describe =
    words ~n (fun () ->
        let () =
          let local_ req = Req.v ~meth:Httpz.Method.Get ~target:"/hello" () in
          Backend.run req describe null_writer
        in
        ())
  in
  row "Backend.run machinery, cheapest response"
    (run_with (fun r -> Resp.v r ~headers:Headers.empty Body.Empty));
  row "  + a content type, as ?content_type"
    (run_with (fun r ->
         Resp.v r ~headers:Headers.empty ~content_type:"text/plain"
           Body.Empty));
  row "  same content type, in a stack_ block instead"
    (run_with (fun r ->
         let () =
           Resp.v r
             ~headers:
               (stack_
                  [ Resp.h_local Httpz.Header_name.Content_type "text/plain" ])
             Body.Empty
         in
         ()));
  row "  + a string body (variant, Some, boxed Int64)"
    (run_with (fun r -> Resp.v r ~headers:Headers.empty (Body.String "hi")));
  (* Dispatch itself is free. What a route costs is the [Some] [Route.run]
     returns on a match, and a capture pattern's partial application. *)
  let cheap () _ (r : Resp.respond @ local) =
    Resp.v r ~headers:Headers.empty Body.Empty
  in
  let site routes =
    Compiled.compile
      (Site.with_fallback cheap (Site.of_routes routes))
  in
  let serve_site c t =
    words ~n (fun () ->
        let () =
          let local_ req = Req.v ~meth:Httpz.Method.Get ~target:t () in
          Backend.handle c () req null_writer
        in
        ())
  in
  row "dispatch: 8 routes, none matched"
    (serve_site
       (site
          (List.init 8 (fun i ->
               Route.(get (s (Printf.sprintf "r%d" i) /? nil)) cheap)))
       "/nope");
  row "dispatch: 8 routes, the last matched"
    (serve_site
       (site
          (List.init 8 (fun i ->
               Route.(get (s (Printf.sprintf "r%d" i) /? nil)) cheap)))
       "/r7");
  row "dispatch: one capture route, matched"
    (serve_site
       (site [ Route.(get (s "e" / str /? nil)) (fun _s -> cheap) ])
       "/e/abc");
  row "Backend.sink without emit_sub"
    (words ~n (fun () ->
         ignore (Sys.opaque_identity (Backend.sink (fun _ -> ())))));
  row "Backend.sink with emit_sub"
    (words ~n (fun () ->
         ignore
           (Sys.opaque_identity
              (Backend.sink ~emit_sub:(fun _ _ _ -> ()) (fun _ -> ())))))
