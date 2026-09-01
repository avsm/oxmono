(* What a request costs on the heap.

   TODO.md quotes these numbers to decide where effort goes, so they need an
   oracle rather than a throwaway. This is a tool, not a test: the figures
   shift with the compiler and pinning them would fail a build for a reason
   nobody wants to chase. Run it and read it.

   The writer is a null one. A real backend's socket write is not what is
   being counted, and neither is the body: the point is what the machinery
   around the body costs, which is what the modes work bought down. *)

open Proffer

external opaque_local : string @ local -> string @ local = "%opaque"

let compiled =
  (Site.of_routes
       [ Route.(get (s "hello")) (fun () _req respond ->
             Resp.text respond "hi")
       ])

(* The block is written [stack_] and the field built with [h_local], which is
   the pair that keeps it off the heap. Written any other way this row is not
   zero, which is the whole point of the row. *)
let header_site =
  (Site.of_routes
       [ Route.(get (s "hello")) (fun () _req respond ->
             let () =
               Resp.v respond ~content_type:(This "text/plain")
                 ~headers:
                   (stack_
                      [ Resp.h_local Httpz.Header_name.X_cache "hit" ])
                 (stack_ (Body.String "hi"))
             in
             ())
       ])

(* Built once, which is what a memoised page or a static asset does: the
   cache holds the tag, not the digest it came from. A tag renders its wire
   form when it is built, so serving it costs nothing. *)
let reused_etag = Etag.strong "v1"

let etag_site =
  (Site.of_routes
       [ Route.(get (s "hello")) (fun () _req respond ->
             Resp.html respond ~etag:reused_etag "hi")
       ])

(* The other case, for contrast: a route that computes a fresh tag per
   request pays to build one. Rendering moved to construction, so this is
   where it is now paid. *)
let fresh_etag_site =
  (Site.of_routes
       [ Route.(get (s "hello")) (fun () _req respond ->
             Resp.html respond
               ~etag:(Etag.strong (Sys.opaque_identity "v1"))
               "hi")
       ])

(* A stream, to see what the streaming path costs beyond a string body. The
   producer writes a constant, so the row is the framework and not the
   producer: an encoder allocates its own buffer, and an earlier version of
   this row made a [Bytes.of_string] per request and charged it here. *)
let stream_site =
  (Site.of_routes
       [ Route.(get (s "hello")) (fun () _req respond ->
             Resp.stream respond "text/plain" (fun sink ->
                 Body.Sink.write sink "hi"))
       ])

let null_writer (_ : Backend.outcome @ local) = ()

let stream_writer (o : Backend.outcome @ local) =
  match o.body with
  | Backend.Stream { write; _ } ->
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

(* [site] and [writer] are required, not optional. A [Some] on a module-level
   value that is computed rather than constant is a real allocation, and as
   optional arguments they were charging two words to every row that named
   one. The harness has to be at least as careful as what it measures. *)
let serve ?headers ?(target = "/hello") ?(meth = Httpz.Method.Get) ~site
    ~writer () =
  let () =
    let local_ req = Req.v ~meth ~target ?headers () in
    Backend.handle site () req writer
  in
  ()

let row name v = Printf.printf "%-46s %6.1f words\n" name v

let () =
  let n = 20_000 in
  row "full serve, literal route, content type only"
    (words ~n (fun () -> serve ~site:compiled ~writer:null_writer ()));
  row "the same with an entity-tag, built once and reused"
    (words ~n (fun () -> serve ~site:etag_site ~writer:null_writer ()));
  row "the same with an entity-tag built per request"
    (words ~n (fun () -> serve ~site:fresh_etag_site ~writer:null_writer ()));
  row "the same with a header field in a stack_ block"
    (words ~n (fun () -> serve ~site:header_site ~writer:null_writer ()));
  row "a 404"
    (words ~n (fun () ->
         serve ~target:"/nope" ~site:compiled ~writer:null_writer ()));
  row "a streamed body, written"
    (words ~n (fun () ->
         serve ~site:stream_site ~writer:stream_writer ()));
  row "Req.v alone"
    (words ~n (fun () ->
         let () =
           let local_ req = Req.v ~meth:Httpz.Method.Get ~target:"/hello" () in
           let local_ _path = opaque_local (Req.path req) in ()
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
          let local_ _path = opaque_local (Req.path req) in ()
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
    (run_with (fun r ->
         Resp.v r ~content_type:Null ~headers:Headers.empty Body.Empty));
  row "  + a content type, as ~content_type"
    (run_with (fun r ->
         Resp.v r ~headers:Headers.empty
           ~content_type:(This (Sys.opaque_identity "text/plain"))
           Body.Empty));
  row "  same content type, in a stack_ block instead"
    (run_with (fun r ->
         let () =
           Resp.v r ~content_type:Null
             ~headers:
               (stack_
                  [ Resp.h_local Httpz.Header_name.Content_type "text/plain" ])
             Body.Empty
         in
         ()));
  (* [Sys.opaque_identity], because a literal is a static constant and would
     measure nothing. That goes for the content type too: [Some c] on a
     module-level string is static, so only a runtime one shows what the
     argument costs. *)
  row "  + a string body, built without stack_"
    (run_with (fun r ->
         Resp.v r ~content_type:Null ~headers:Headers.empty
           (Body.String (Sys.opaque_identity "hello"))));
  row "  the same body, built with stack_"
    (run_with (fun r ->
         let () =
           Resp.v r ~content_type:Null ~headers:Headers.empty
             (stack_ (Body.String (Sys.opaque_identity "hello")))
         in
         ()));
  (* Dispatch itself is free. What a route costs is the [Some] [Route.run]
     returns on a match, and a capture pattern's partial application. *)
  let cheap () _ (r : Resp.respond @ local) =
    Resp.v r ~content_type:Null ~headers:Headers.empty Body.Empty
  in
  let site routes =
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
               Route.(get (s (Printf.sprintf "r%d" i))) cheap)))
       "/nope");
  row "dispatch: 8 routes, the last matched"
    (serve_site
       (site
          (List.init 8 (fun i ->
               Route.(get (s (Printf.sprintf "r%d" i))) cheap)))
       "/r7");
  row "dispatch: one capture route, matched"
    (serve_site
       (site [ Route.(get (s "e" / str)) (fun _s -> cheap) ])
       "/e/abc");
  row "Backend.sink without emit_sub"
    (words ~n (fun () ->
         ignore (Sys.opaque_identity (Backend.sink (fun _ -> ())))));
  row "Backend.sink with emit_sub"
    (words ~n (fun () ->
         ignore
           (Sys.opaque_identity
              (Backend.sink ~emit_sub:(fun _ _ _ -> ()) (fun _ -> ())))))
