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
