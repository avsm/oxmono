(* Conditional GET, HEAD and the body variants, all through the code a socket
   backend runs. *)

open Proffer
open Proffer.Route

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* 784111777 is Sun, 06 Nov 1994 08:49:37 GMT, the example date of RFC 9110. *)
let mtime = 784111777.
let imf = "Sun, 06 Nov 1994 08:49:37 GMT"
let earlier = "Sat, 05 Nov 1994 08:49:37 GMT"
let later = "Mon, 07 Nov 1994 08:49:37 GMT"
let cache = Cache_control.public ~max_age:(`Hours 1) ()

type env = { forced : int ref }

let routes =
  [
    get (s "page" /? nil) (fun _env _req ->
        Resp.html ~etag:(`Strong "v1") ~cache "<p>page</p>");
    get (s "weak" /? nil) (fun _env _req ->
        Resp.v ~etag:(`Weak "v1") ~cache
          ~headers:[ ("Vary", "Accept"); ("X-Extra", "1") ]
          ~content_type:"text/plain" (Body.String "weak"));
    get (s "dated" /? nil) (fun _env _req ->
        Resp.v ~last_modified:mtime ~content_type:"text/plain"
          (Body.String "dated"));
    get (s "delayed" /? nil) (fun env _req ->
        Resp.v ~etag:(`Strong "d1") ~content_type:"text/plain"
          (Body.Delayed
             {
               length = Some 7L;
               gen =
                 (fun () ->
                   incr env.forced;
                   "delayed");
             }));
    post (s "page" /? nil) (fun _env _req ->
        Resp.html ~etag:(`Strong "v1") ~cache "<p>posted</p>");
    get (s "stream" /? nil) (fun _env _req ->
        Resp.v ~content_type:"text/plain"
          (Body.Stream
             {
               length = None;
               write =
                 (fun sink ->
                   Body.Sink.write sink "a";
                   Body.Sink.write sink "bc");
             }));
  ]

let compiled = Compiled.compile (Site.of_routes routes)
let env = { forced = ref 0 }

let run ?headers meth target =
  Proffer_mock.request ?headers compiled env meth target

let body (o : Serve.outcome) =
  match o.Serve.body with
  | `String s -> s
  | `Empty -> ""
  | `Stream _ -> "<stream>"

let header (o : Serve.outcome) n = List.assoc_opt n o.Serve.headers

let () =
  let o = run `GET "/page" in
  check "200 without a condition" (Status.code o.Serve.status = 200);
  check "etag rendered" (header o "ETag" = Some "\"v1\"");
  check "cache rendered"
    (header o "Cache-Control" = Some "public, max-age=3600")

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] `GET "/page" in
  check "matching etag is 304" (Status.code o.Serve.status = 304);
  check "304 has no body" (body o = "");
  check "304 has no length" (o.Serve.content_length = None);
  check "304 keeps the etag" (header o "ETag" = Some "\"v1\"");
  check "304 keeps the cache policy"
    (header o "Cache-Control" = Some "public, max-age=3600");
  check "304 drops the content type" (header o "Content-Type" = None)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"other\"") ] `GET "/page" in
  check "other etag is 200" (Status.code o.Serve.status = 200);
  let o =
    run ~headers:[ ("If-None-Match", "\"a\", W/\"v1\" , \"b\"") ] `GET "/page"
  in
  check "etag list matches" (Status.code o.Serve.status = 304);
  let o = run ~headers:[ ("If-None-Match", "*") ] `GET "/page" in
  check "star matches" (Status.code o.Serve.status = 304);
  let o = run ~headers:[ ("If-None-Match", "v1") ] `GET "/page" in
  check "unquoted tag does not match" (Status.code o.Serve.status = 200)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] `GET "/weak" in
  check "weak against strong matches" (Status.code o.Serve.status = 304);
  check "304 keeps Vary" (header o "Vary" = Some "Accept");
  check "304 drops other headers" (header o "X-Extra" = None)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] `POST "/page" in
  check "a non-GET is not revalidated" (Status.code o.Serve.status = 200);
  check "the posted body is sent" (body o = "<p>posted</p>")

let () =
  let o = run `GET "/dated" in
  check "last modified rendered" (header o "Last-Modified" = Some imf);
  let o = run ~headers:[ ("If-Modified-Since", imf) ] `GET "/dated" in
  check "same second is 304" (Status.code o.Serve.status = 304);
  check "304 keeps last modified" (header o "Last-Modified" = Some imf);
  let o = run ~headers:[ ("If-Modified-Since", later) ] `GET "/dated" in
  check "a later date is 304" (Status.code o.Serve.status = 304);
  let o = run ~headers:[ ("If-Modified-Since", earlier) ] `GET "/dated" in
  check "an earlier date is 200" (Status.code o.Serve.status = 200);
  let o = run ~headers:[ ("If-Modified-Since", "yesterday") ] `GET "/dated" in
  check "an unparsable date is ignored" (Status.code o.Serve.status = 200)

let () =
  let o =
    run
      ~headers:[ ("If-None-Match", "\"none\""); ("If-Modified-Since", imf) ]
      `GET "/dated"
  in
  check "If-None-Match hides If-Modified-Since"
    (Status.code o.Serve.status = 200)

let () =
  let o = run `HEAD "/page" in
  check "head matches the get route" (Status.code o.Serve.status = 200);
  check "head has no body" (body o = "");
  check "head keeps the length" (o.Serve.content_length = Some 11L);
  check "head keeps the headers"
    (header o "Content-Type" = Some "text/html; charset=utf-8")

let () =
  check "delayed not yet forced" (!(env.forced) = 0);
  let o = run `HEAD "/delayed" in
  check "head does not force delayed" (!(env.forced) = 0);
  check "head reports the declared length" (o.Serve.content_length = Some 7L);
  let o = run ~headers:[ ("If-None-Match", "\"d1\"") ] `GET "/delayed" in
  check "304 does not force delayed" (!(env.forced) = 0);
  check "304 on delayed" (Status.code o.Serve.status = 304);
  let o = run `GET "/delayed" in
  check "get forces delayed once" (!(env.forced) = 1);
  check "delayed body is a string" (body o = "delayed");
  check "delayed length from the body" (o.Serve.content_length = Some 7L)

let () =
  let o = run `GET "/stream" in
  check "mock collects the stream" (body o = "abc");
  check "mock reports the collected length" (o.Serve.content_length = Some 3L);
  let o = run `HEAD "/stream" in
  check "head on a stream has no body" (body o = "");
  check "unknown stream length stays unknown" (o.Serve.content_length = None)

let () = Printf.printf "test_conditional: %d checks ok\n" !checks
