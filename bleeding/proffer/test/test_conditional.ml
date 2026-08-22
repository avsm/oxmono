(* Conditional GET, HEAD and the body variants, all through the code a socket
   backend runs. *)

open Proffer
open Proffer.Route
module H = Httpz.Header_name
module M = Httpz.Method

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
    get (s "page" /? nil) (fun _env _req respond ->
        Resp.html respond ~etag:(`Strong "v1") ~cache "<p>page</p>");
    get (s "weak" /? nil) (fun _env _req respond ->
        Resp.v respond ~etag:(`Weak "v1") ~cache
          ~headers:[ Resp.h H.Vary "Accept"; Resp.other "X-Extra" "1" ]
          ~content_type:"text/plain" (Body.String "weak"));
    get (s "dated" /? nil) (fun _env _req respond ->
        Resp.v respond ~last_modified:mtime ~headers:Headers.empty
          ~content_type:"text/plain" (Body.String "dated"));
    get (s "delayed" /? nil) (fun env _req respond ->
        Resp.v respond ~etag:(`Strong "d1") ~headers:Headers.empty
          ~content_type:"text/plain"
          (Body.Delayed
             {
               length = Some 7L;
               gen =
                 (fun () ->
                   incr env.forced;
                   "delayed");
             }));
    post (s "page" /? nil) (fun _env _req respond ->
        Resp.html respond ~etag:(`Strong "v1") ~cache "<p>posted</p>");
    get (s "boom" /? nil) (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:"text/plain"
          (Body.Delayed
             {
               length = None;
               gen = (fun () -> failwith "generator blew up");
             }));
    (* The instant travels in the path so that one route can serve the whole
       spread of dates the round trip covers. *)
    get
      (s "at" / conv ~name:"epoch" float_of_string_opt /? nil)
      (fun t _env _req respond ->
        Resp.v respond ~last_modified:t ~headers:Headers.empty
          ~content_type:"text/plain" (Body.String "at"));
    get (s "stream" /? nil) (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:"text/plain"
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

let body = Proffer_mock.body
let header_other o s = Proffer_mock.header_other o s
let header = Proffer_mock.header
let code o = Status.code (Proffer_mock.status o)
let length = Proffer_mock.content_length

let () =
  let o = run M.Get "/page" in
  check "200 without a condition" (code o = 200);
  check "etag rendered" (header o H.Etag = Some "\"v1\"");
  check "cache rendered"
    (header o H.Cache_control = Some "public, max-age=3600")

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] M.Get "/page" in
  check "matching etag is 304" (code o = 304);
  check "304 has no body" (body o = "");
  check "304 has no length" (length o = None);
  check "304 keeps the etag" (header o H.Etag = Some "\"v1\"");
  check "304 keeps the cache policy"
    (header o H.Cache_control = Some "public, max-age=3600");
  check "304 drops the content type" (header o H.Content_type = None)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"other\"") ] M.Get "/page" in
  check "other etag is 200" (code o = 200);
  let o =
    run ~headers:[ ("If-None-Match", "\"a\", W/\"v1\" , \"b\"") ] M.Get "/page"
  in
  check "etag list matches" (code o = 304);
  let o = run ~headers:[ ("If-None-Match", "*") ] M.Get "/page" in
  check "star matches" (code o = 304);
  let o = run ~headers:[ ("If-None-Match", "v1") ] M.Get "/page" in
  check "unquoted tag does not match" (code o = 200)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] M.Get "/weak" in
  check "weak against strong matches" (code o = 304);
  check "304 keeps Vary" (header o H.Vary = Some "Accept");
  check "304 drops other headers" (header_other o "X-Extra" = None)

let () =
  let o = run ~headers:[ ("If-None-Match", "\"v1\"") ] M.Post "/page" in
  check "a non-GET is not revalidated" (code o = 200);
  check "the posted body is sent" (body o = "<p>posted</p>")

let () =
  let o = run M.Get "/dated" in
  check "last modified rendered" (header o H.Last_modified = Some imf);
  let o = run ~headers:[ ("If-Modified-Since", imf) ] M.Get "/dated" in
  check "same second is 304" (code o = 304);
  check "304 keeps last modified" (header o H.Last_modified = Some imf);
  let o = run ~headers:[ ("If-Modified-Since", later) ] M.Get "/dated" in
  check "a later date is 304" (code o = 304);
  let o = run ~headers:[ ("If-Modified-Since", earlier) ] M.Get "/dated" in
  check "an earlier date is 200" (code o = 200);
  let o = run ~headers:[ ("If-Modified-Since", "yesterday") ] M.Get "/dated" in
  check "an unparsable date is ignored" (code o = 200)

let () =
  let o =
    run
      ~headers:[ ("If-None-Match", "\"none\""); ("If-Modified-Since", imf) ]
      M.Get "/dated"
  in
  check "If-None-Match hides If-Modified-Since"
    (code o = 200)

let () =
  let o = run M.Head "/page" in
  check "head matches the get route" (code o = 200);
  check "head has no body" (body o = "");
  check "head keeps the length" (length o = Some 11L);
  check "head keeps the headers"
    (header o H.Content_type = Some "text/html; charset=utf-8")

let () =
  check "delayed not yet forced" (!(env.forced) = 0);
  let o = run M.Head "/delayed" in
  check "head does not force delayed" (!(env.forced) = 0);
  check "head reports the declared length" (length o = Some 7L);
  let o = run ~headers:[ ("If-None-Match", "\"d1\"") ] M.Get "/delayed" in
  check "304 does not force delayed" (!(env.forced) = 0);
  check "304 on delayed" (code o = 304);
  let o = run M.Get "/delayed" in
  check "get forces delayed once" (!(env.forced) = 1);
  check "delayed body is a string" (body o = "delayed");
  check "delayed length from the body" (length o = Some 7L)

let () =
  let o = run M.Get "/stream" in
  check "mock collects the stream" (body o = "abc");
  check "mock reports the collected length" (length o = Some 3L);
  let o = run M.Head "/stream" in
  check "head on a stream has no body" (body o = "");
  check "unknown stream length stays unknown" (length o = None)

(* The generator runs under the same guard as the handler, so a failure is a
   500 and not a dropped connection. *)
let () =
  let seen = ref None in
  let o =
    Proffer_mock.request
      ~on_error:(fun e -> seen := Some e)
      compiled env M.Get "/boom"
  in
  check "a generator that raises is 500" (code o = 500);
  check "on_error is told about the generator"
    (match !seen with Some (Failure _) -> true | _ -> false);
  check "the 500 has a body" (body o = "Internal Server Error\n");
  let o = Proffer_mock.request compiled env M.Head "/boom" in
  check "head does not run the generator" (code o = 200)

(* An impossible date is no date, so the condition is dropped and the full
   response goes out. *)
let () =
  let since v = run ~headers:[ ("If-Modified-Since", v) ] M.Get "/dated" in
  check "31 February is not a date"
    (code (since "Wed, 31 Feb 2000 00:00:00 GMT") = 200);
  check "31 April is not a date"
    (code (since "Thu, 31 Apr 2000 00:00:00 GMT") = 200);
  check "29 February 2100 is not a date"
    (code (since "Mon, 29 Feb 2100 00:00:00 GMT") = 200);
  check "29 February 2000 is a date"
    (code (since "Tue, 29 Feb 2000 00:00:00 GMT") = 304);
  check "a disagreeing weekday is still a date"
    (code (since "Mon, 29 Feb 2000 00:00:00 GMT") = 304)

(* RFC 9110 section 5.6.7 requires a recipient to accept the two obsolete
   forms as well as IMF-fixdate. Dates are httpz's now and it parses all
   three, so both of these name the same instant as [imf] and answer 304.
   Proffer's own parser read IMF-fixdate alone and dropped the condition. *)
let () =
  let since v =
    run ~headers:[ ("If-Modified-Since", v) ] M.Get "/dated"
  in
  check "RFC 850 is a date"
    (code (since "Sunday, 06-Nov-94 08:49:37 GMT") = 304);
  check "asctime is a date" (code (since "Sun Nov  6 08:49:37 1994") = 304)

(* [Resp.v ~last_modified] prints an IMF-fixdate and If-Modified-Since reads
   one back. A date survives the pair when its printed form gives a 304
   against the instant it came from and the printed form of the second before
   gives a 200, which together pin the parsed value to the second it was
   printed from. *)
let at t = Printf.sprintf "/at/%.0f" t

let printed t =
  match header (run M.Get (at t)) H.Last_modified
    with
  | Some v -> v
  | None -> failwith "no Last-Modified"

let since t v = code (run ~headers:[ ("If-Modified-Since", v) ] M.Get (at t))

let () =
  let spread =
    [
      ("the epoch", 0.);
      ("a leap day", 951782400.);
      ("the last second of 1999", 946684799.);
      ("the first second of 2000", 946684800.);
      ("a recent date", 1755561600.);
      ("the last representable second", 253402300799.);
    ]
  in
  List.iter
    (fun (name, t) ->
      check (name ^ " round trips") (since t (printed t) = 304);
      check
        (name ^ " is later than the second before it")
        (since t (printed (t -. 1.)) = 200))
    spread;
  (* The first representable second has nothing before it, so the pair is read
     from the other side. *)
  (* httpz clamps below year 1, so that is the floor now. *)
  let t = -62135596800. in
  check "the first representable second round trips"
    (since t (printed t) = 304);
  check "the second after it is later" (since (t +. 1.) (printed t) = 200)

let () = Printf.printf "test_conditional: %d checks ok\n" !checks
