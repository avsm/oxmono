(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* numptyd, driven as numpty drives it: a real 'numpty-cpu netd' over pipes,
   against a server this test stands up on the loopback. Nothing here reaches
   the internet, and nothing here needs a fixture on disk.

   The properties that matter are the ones a network tool gets wrong quietly. A
   404 must be reported as a 404 rather than returned as though it were the
   article. A body over the bound must be refused with the size it would have
   been rather than cut down to fit. And a program numptyd runs must not be
   handed the pipe the protocol arrives on. *)

module Proto = Numpty_net.Proto

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let holds s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* The binary under test, passed by the dune rule so that the test runs the one
   this build produced. *)
let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "numpty-cpu"

(* ------------------------------------------------------------------ *)
(* A server on the loopback                                            *)
(* ------------------------------------------------------------------ *)

let page =
  "<html><head><title>T</title><style>p{color:red}</style>\n\
   <script>var secret = \"nachos\";</script></head>\n\
   <body><h1>Hi</h1><p>Some&nbsp;text &amp; more</p><!-- a note \
   --></body></html>"

(* Larger than the bound the fetch that asks for it is given, and declared in a
   Content-Length so that the refusal can name the size. *)
let big = String.make 200_000 'x'

let reply flow ~status ~reason ~ctype ~body ~extra ~head =
  let text =
    Printf.sprintf
      "HTTP/1.1 %d %s\r\n\
       Content-Type: %s\r\n\
       Content-Length: %d\r\n\
       %sConnection: close\r\n\
       \r\n\
       %s"
      status reason ctype (String.length body) extra
      (if head then "" else body)
  in
  Eio.Flow.copy_string text flow

(* One request, answered by the path it asks for. A client that gives up part
   way through, which is what a curl refusing a body over its bound does, closes
   the connection under the write, so the write is allowed to fail. *)
let serve ~port flow =
  let r = Eio.Buf_read.of_flow flow ~max_size:0x10000 in
  let request = Eio.Buf_read.line r in
  let rec drain () =
    match Eio.Buf_read.line r with "" -> () | _ -> drain ()
  in
  (try drain () with End_of_file -> ());
  let verb, path =
    match String.split_on_char ' ' request with
    | verb :: path :: _ -> (verb, path)
    | _ -> ("GET", "/")
  in
  let head = verb = "HEAD" in
  let reply = reply flow ~extra:"" ~head in
  try
    match path with
    | "/page.html" ->
        reply ~status:200 ~reason:"OK" ~ctype:"text/html; charset=utf-8"
          ~body:page
    | "/big.txt" -> reply ~status:200 ~reason:"OK" ~ctype:"text/plain" ~body:big
    | "/plain.txt" ->
        reply ~status:200 ~reason:"OK" ~ctype:"text/plain" ~body:"<b>as is</b>"
    | "/moved" ->
        Eio.Flow.copy_string
          (Printf.sprintf
             "HTTP/1.1 302 Found\r\n\
              Location: http://127.0.0.1:%d/page.html\r\n\
              Content-Length: 0\r\n\
              Connection: close\r\n\
              \r\n"
             port)
          flow
    | _ ->
        reply ~status:404 ~reason:"Not Found" ~ctype:"text/html"
          ~body:"<html><body><p>no such page</p></body></html>"
  with Eio.Io _ | End_of_file -> ()

let with_site ~sw ~net f =
  let listening =
    Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listening with
    | `Tcp (_, p) -> p
    | `Unix _ -> 0
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw listening ~on_error:ignore (fun flow _ ->
            serve ~port flow)
      done;
      `Stop_daemon);
  f (Printf.sprintf "http://127.0.0.1:%d" port)

(* ------------------------------------------------------------------ *)
(* numptyd over its pipes                                              *)
(* ------------------------------------------------------------------ *)

(* The parent's ends of the child's own sides are closed at once, or the
   child's stdin would never reach end of file however long the test waited. *)
let with_netd ~sw ~proc f =
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let err_r, err_w = Eio_unix.pipe sw in
  let child =
    Eio.Process.spawn ~sw proc ~stdin:in_r ~stdout:out_w ~stderr:err_w
      [ exe; "netd" ]
  in
  Eio.Flow.close in_r;
  Eio.Flow.close out_w;
  Eio.Flow.close err_w;
  let r = Eio.Buf_read.of_flow out_r ~max_size:Agentkit.Line.max_line in
  Fun.protect
    ~finally:(fun () ->
      try Eio.Process.signal child Sys.sigkill
      with Eio.Io _ | Invalid_argument _ -> ())
    (fun () -> f ~child ~r ~stdin:in_w ~stderr:err_r)

(* A server that has stopped answering must fail the test rather than hang the
   suite. The window covers a curl's own --max-time and a little more. *)
let raw ~clock r =
  match
    Eio.Time.with_timeout clock 90. (fun () -> Ok (Eio.Buf_read.line r))
  with
  | Ok l -> Some l
  | Error `Timeout -> failwith "numptyd said nothing within 90 seconds"
  | exception End_of_file -> None

let read ~clock r =
  match raw ~clock r with
  | None -> `Eof
  | Some line -> Proto.read_to_client (Eio.Buf_read.of_string (line ^ "\n"))

let greeting ~clock r =
  match read ~clock r with
  | `Msg (Proto.Hello h) -> h
  | `Msg _ -> failwith "numptyd said something before it greeted"
  | `Eof -> failwith "numptyd stopped before it greeted"
  | `Bad l -> failwith ("numptyd wrote a line that is not a message: " ^ l)

let call ~clock ~w ~r id op =
  Proto.write_to_server w (Proto.Call { id; op });
  Eio.Buf_write.flush w;
  let traces = ref [] in
  let rec await () =
    match read ~clock r with
    | `Msg (Proto.Trace t) ->
        traces := t :: !traces;
        await ()
    | `Msg (Proto.Result res) when res.id = id -> (res.output, List.rev !traces)
    | `Msg (Proto.Result _) -> failwith "numptyd answered a call nobody made"
    | `Msg (Proto.Hello _) -> failwith "numptyd greeted twice"
    | `Eof -> failwith "numptyd stopped in the middle of a call"
    | `Bad l -> failwith ("numptyd wrote a line that is not a message: " ^ l)
  in
  await ()

(* The status the child left, or a value no exit has when it is still there
   after a minute. A wedged numptyd must fail the test rather than hang it. *)
let exited ~clock child =
  match
    Eio.Time.with_timeout clock 60. (fun () -> Ok (Eio.Process.await child))
  with
  | Ok (`Exited n) -> n
  | Ok (`Signaled n) -> -n
  | Error `Timeout -> -128

(* ------------------------------------------------------------------ *)
(* The tests                                                           *)
(* ------------------------------------------------------------------ *)

let serves env =
  let proc = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env
  and net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  with_site ~sw ~net @@ fun site ->
  with_netd ~sw ~proc @@ fun ~child ~r ~stdin ~stderr:_ ->
  ( Eio.Buf_write.with_flow stdin @@ fun w ->
    let h = greeting ~clock r in
    check "the greeting names the curl numptyd found" h.curl;
    check "and says so in the note a person reads" (holds h.status "curl");
    let out, traces =
      call ~clock ~w ~r 1
        (Proto.Fetch
           { url = site ^ "/page.html"; render = Proto.Text; max_bytes = None })
    in
    check "a fetch traces itself under the call's id"
      (traces <> []
      && List.for_all (fun (t : Proto.trace) -> t.id = Some 1) traces);
    check "a fetch reports the status, the final url and the content type"
      (holds out ("200 " ^ site ^ "/page.html") && holds out "text/html");
    check "and how many bytes came back"
      (holds out (Printf.sprintf "%d bytes" (String.length page)));
    check "the text render decodes the common entities"
      (holds out "Some text & more");
    check "the text render drops what a script element holds"
      (not (holds out "nachos"));
    check "the text render drops what a style element holds"
      (not (holds out "color:red"));
    check "the text render drops a comment" (not (holds out "a note"));
    check "the text render leaves no tags" (not (holds out "<"));
    let out, _ =
      call ~clock ~w ~r 2
        (Proto.Fetch
           { url = site ^ "/page.html"; render = Proto.Raw; max_bytes = None })
    in
    check "the raw render is the bytes as they arrived" (holds out page);
    let out, _ =
      call ~clock ~w ~r 3
        (Proto.Fetch
           { url = site ^ "/plain.txt"; render = Proto.Text; max_bytes = None })
    in
    check "a body that is not html is not reduced" (holds out "<b>as is</b>");
    (* The failure the repository's rule about silencing is written against.
       Returning the error page as though it were the document is what an agent
       is least able to detect for itself. *)
    let out, _ =
      call ~clock ~w ~r 4
        (Proto.Fetch
           { url = site ^ "/gone"; render = Proto.Text; max_bytes = None })
    in
    check "a 404 is reported as a 404" (holds out ("404 " ^ site ^ "/gone"));
    check "and says the status is not a 2xx" (holds out "not a 2xx");
    check "with the body it did send after it" (holds out "no such page");
    (* Refused rather than truncated, and with the size it would have been, so
       that a caller can decide what to do instead. *)
    let out, _ =
      call ~clock ~w ~r 5
        (Proto.Fetch
           {
             url = site ^ "/big.txt";
             render = Proto.Raw;
             max_bytes = Some 1000;
           })
    in
    check "a body over the bound is refused" (holds out "Refused");
    check "the refusal names the size it would have been"
      (holds out (string_of_int (String.length big)));
    check "and the bound that refused it" (holds out "1000 byte bound");
    check "and returns none of the body" (not (holds out "xxxxxxxxxx"));
    let out, _ =
      call ~clock ~w ~r 6 (Proto.Head { url = site ^ "/page.html" })
    in
    check "head reports the status and the size the server declares"
      (holds out ("200 " ^ site ^ "/page.html")
      && holds out (Printf.sprintf "%d bytes declared" (String.length page)));
    check "and returns no body" (not (holds out "Some text"));
    let out, _ =
      call ~clock ~w ~r 7
        (Proto.Fetch
           { url = site ^ "/moved"; render = Proto.Text; max_bytes = None })
    in
    check "a redirect is followed and the final url reported"
      (holds out ("200 " ^ site ^ "/page.html"));
    let out, _ =
      call ~clock ~w ~r 8 (Proto.Run { program = "echo"; args = [ "hi" ] })
    in
    check "run reports how the program ended and what it wrote"
      (out = "echo exited 0\nhi\n");
    let out, _ =
      call ~clock ~w ~r 9 (Proto.Run { program = "false"; args = [] })
    in
    check "a program that fails is reported as failing"
      (holds out "false exited 1");
    (* A command that reads standard input must not be handed the pipe the
       protocol arrives on. One that were would take this call's successor for
       its input, and the call would never be answered. The bound in [call] is
       what would catch that, so the check is that this returns at all, and
       empty. *)
    let out, _ =
      call ~clock ~w ~r 10 (Proto.Run { program = "cat"; args = [] })
    in
    check "a program that reads standard input is given an empty one"
      (out = "cat exited 0\n");
    let out, _ =
      call ~clock ~w ~r 11 (Proto.Run { program = "echo"; args = [ "after" ] })
    in
    check "and the call after it is still the one answered"
      (out = "echo exited 0\nafter\n") );
  (* End of file on stdin is a shutdown. *)
  Eio.Flow.close stdin;
  check "closing stdin ends numptyd" (exited ~clock child = 0)

(* A program that is not there is a result the model can act on rather than a
   fault of the protocol. *)
let no_such_program env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  with_netd ~sw ~proc @@ fun ~child ~r ~stdin ~stderr:_ ->
  ( Eio.Buf_write.with_flow stdin @@ fun w ->
    ignore (greeting ~clock r);
    let out, _ =
      call ~clock ~w ~r 1
        (Proto.Run { program = "no-such-program-here"; args = [] })
    in
    check "a program that is not on the PATH is answered in words"
      (holds out "could not be run" && holds out "no-such-program-here");
    check "and the answer says how a program is looked for"
      (holds out "no shell");
    Proto.write_to_server w Proto.Shutdown );
  Eio.Flow.close stdin;
  check "a shutdown message ends numptyd" (exited ~clock child = 0)

(* A line numptyd cannot read is a fault of the peer, which is this same
   binary. It reports the line and leaves, rather than skipping it and going on
   to answer the wrong question. *)
let garbage env =
  let proc = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  with_netd ~sw ~proc @@ fun ~child ~r ~stdin ~stderr ->
  ( Eio.Buf_write.with_flow stdin @@ fun w ->
    ignore (greeting ~clock r);
    Eio.Buf_write.string w "not a message\n" );
  let status = exited ~clock child in
  check "a line that is not a message ends numptyd nonzero"
    (status <> 0 && status <> -1);
  let said = Eio.Buf_read.(parse_exn take_all) ~max_size:100_000 stderr in
  check "and the offending line is on its standard error"
    (holds said "not a message")

let () =
  Eio_main.run (fun env ->
      serves env;
      no_such_program env;
      garbage env);
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
