(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests of numptyd's line protocol.

   Two properties matter. Every message shape must survive a round trip, since
   both ends are the same binary and a shape that does not survive is a bug no
   version negotiation will catch. And a line that does not parse must come
   back as [`Bad] with the offending text, never as a skipped line, because a
   skipped line desynchronises a stream of sequential calls silently.

   The exact-byte cases pin the wire against the design's examples, so a change
   of member order or of the flat argument encoding shows up here. *)

module Proto = Numpty_net.Proto

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

(* Serialise with a real [Buf_write] over a buffer sink, which is what the
   client and the server hold. [Eio_mock.Backend] supplies the scheduler that
   [with_flow]'s fiber needs. *)
let written write msgs =
  Eio_mock.Backend.run @@ fun () ->
  let b = Buffer.create 256 in
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink b) (fun w ->
      List.iter (write w) msgs);
  Buffer.contents b

let to_server_bytes msg = written Proto.write_to_server [ msg ]
let to_client_bytes msg = written Proto.write_to_client [ msg ]

let server_round_trip name msg =
  let bytes = to_server_bytes msg in
  check name (Proto.read_to_server (Eio.Buf_read.of_string bytes) = `Msg msg)

let client_round_trip name msg =
  let bytes = to_client_bytes msg in
  check name (Proto.read_to_client (Eio.Buf_read.of_string bytes) = `Msg msg)

(* Round trips: every message shape, and every operation a call can name. *)

let () =
  let call id op = Proto.Call { Proto.id; op } in
  server_round_trip "a text fetch round trips"
    (call 1
       (Proto.Fetch
          { url = "http://127.0.0.1/x"; render = Proto.Text; max_bytes = None }));
  server_round_trip "a raw fetch with a bound round trips"
    (call 2
       (Proto.Fetch
          {
            url = "http://127.0.0.1/x";
            render = Proto.Raw;
            max_bytes = Some 4096;
          }));
  server_round_trip "head round trips"
    (call 3 (Proto.Head { url = "http://127.0.0.1/x" }));
  server_round_trip "run round trips"
    (call 4 (Proto.Run { program = "git"; args = [ "status"; "--short" ] }));
  server_round_trip "run with no arguments round trips"
    (call 5 (Proto.Run { program = "uname"; args = [] }));
  server_round_trip "shutdown round trips" Proto.Shutdown;
  client_round_trip "hello round trips"
    (Proto.Hello { status = "numpty: curl 8.7.1"; curl = true });
  client_round_trip "trace round trips"
    (Proto.Trace { id = Some 1; line = "fetch: http://127.0.0.1/x" });
  client_round_trip "trace without id round trips"
    (Proto.Trace { id = None; line = "numptyd: started" });
  client_round_trip "result round trips"
    (Proto.Result { id = 1; output = "200 http://127.0.0.1/x" })

(* Exact bytes: one message per line, in the order the design writes them. *)

let () =
  check "hello line"
    (to_client_bytes
       (Proto.Hello { status = "numpty: curl 8.7.1"; curl = true })
    = "{\"hello\":{\"status\":\"numpty: curl 8.7.1\",\"curl\":true}}\n");
  check "fetch call line"
    (to_server_bytes
       (Proto.Call
          {
            id = 1;
            op =
              Proto.Fetch
                { url = "http://x/"; render = Proto.Text; max_bytes = None };
          })
    = "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\",\"render\":\"text\"}}\n"
    );
  check "a fetch without a bound writes no max_bytes"
    (to_server_bytes
       (Proto.Call
          {
            id = 1;
            op =
              Proto.Fetch
                { url = "http://x/"; render = Proto.Raw; max_bytes = Some 8 };
          })
    = "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\",\"render\":\"raw\",\"max_bytes\":8}}\n"
    );
  check "run call line"
    (to_server_bytes
       (Proto.Call
          { id = 2; op = Proto.Run { program = "git"; args = [ "status" ] } })
    = "{\"call\":{\"id\":2,\"op\":\"run\",\"program\":\"git\",\"args\":[\"status\"]}}\n"
    );
  check "trace line drops an absent id"
    (to_client_bytes (Proto.Trace { id = None; line = "x" })
    = "{\"trace\":{\"line\":\"x\"}}\n");
  check "shutdown line" (to_server_bytes Proto.Shutdown = "{\"shutdown\":{}}\n")

(* Faults: each malformed line comes back whole, as [`Bad]. *)

let bad_to_server name line =
  check name
    (Proto.read_to_server (Eio.Buf_read.of_string (line ^ "\n")) = `Bad line)

let bad_to_client name line =
  check name
    (Proto.read_to_client (Eio.Buf_read.of_string (line ^ "\n")) = `Bad line)

let () =
  bad_to_server "bad json is bad" "{\"call\":";
  bad_to_server "empty line is bad" "";
  bad_to_server "unknown outer member is bad" "{\"greeting\":{}}";
  bad_to_server "two outer members is bad"
    "{\"call\":{\"id\":1,\"op\":\"head\",\"url\":\"x\"},\"shutdown\":{}}";
  bad_to_server "unknown op is bad" "{\"call\":{\"id\":1,\"op\":\"telnet\"}}";
  bad_to_server "a fetch with no url is bad"
    "{\"call\":{\"id\":1,\"op\":\"fetch\",\"render\":\"text\"}}";
  bad_to_server "a fetch with no render is bad"
    "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\"}}";
  bad_to_server "a render nobody serves is bad"
    "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\",\"render\":\"markdown\"}}";
  bad_to_server "a fractional bound is bad"
    "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\",\"render\":\"raw\",\"max_bytes\":1.5}}";
  bad_to_server "a bound past the wire's integers is bad"
    "{\"call\":{\"id\":1,\"op\":\"fetch\",\"url\":\"http://x/\",\"render\":\"raw\",\"max_bytes\":1e300}}";
  bad_to_server "run with no args member is bad"
    "{\"call\":{\"id\":1,\"op\":\"run\",\"program\":\"git\"}}";
  (* An argument list with one element that is not a string is refused whole.
     Dropping it would run a command the caller did not write. *)
  bad_to_server "run with a non-string argument is bad"
    "{\"call\":{\"id\":1,\"op\":\"run\",\"program\":\"git\",\"args\":[\"log\",7]}}";
  bad_to_server "missing id is bad" "{\"call\":{\"op\":\"head\",\"url\":\"x\"}}";
  bad_to_server "a raw invalid byte on the wire is bad"
    "{\"call\":{\"id\":1,\"op\":\"head\",\"url\":\"a\xffb\"}}";
  bad_to_server "a client message is bad on the server side"
    "{\"result\":{\"id\":1,\"output\":\"ok\"}}";
  bad_to_client "missing hello member is bad" "{\"hello\":{\"status\":\"x\"}}";
  bad_to_client "wrong hello type is bad"
    "{\"hello\":{\"status\":\"x\",\"curl\":\"yes\"}}";
  bad_to_client "non-object payload is bad" "{\"trace\":\"x\"}";
  bad_to_client "a server message is bad on the client side" "{\"shutdown\":{}}"

(* Ends and streams. *)

let () =
  check "eof on empty input"
    (Proto.read_to_server (Eio.Buf_read.of_string "") = `Eof);
  check "eof on the client side"
    (Proto.read_to_client (Eio.Buf_read.of_string "") = `Eof);
  let two =
    written Proto.write_to_client
      [
        Proto.Trace { id = Some 1; line = "fetch: http://x/" };
        Proto.Result { id = 1; output = "200 http://x/" };
      ]
  in
  let r = Eio.Buf_read.of_string two in
  check "first of two messages"
    (Proto.read_to_client r
    = `Msg (Proto.Trace { id = Some 1; line = "fetch: http://x/" }));
  check "second of two messages"
    (Proto.read_to_client r
    = `Msg (Proto.Result { id = 1; output = "200 http://x/" }));
  check "eof after two messages" (Proto.read_to_client r = `Eof)

(* Bytes that are not valid UTF-8. A fetched body carries them, so the message
   must survive rather than fault. *)

let () =
  let msg = Proto.Result { id = 1; output = "a\xffb" } in
  check "an invalid byte in a body survives as U+FFFD"
    (Proto.read_to_client (Eio.Buf_read.of_string (to_client_bytes msg))
    = `Msg (Proto.Result { id = 1; output = "a\xef\xbf\xbdb" }));
  check "valid utf-8 is untouched"
    (Proto.read_to_client
       (Eio.Buf_read.of_string
          (to_client_bytes (Proto.Result { id = 1; output = "caf\xc3\xa9" })))
    = `Msg (Proto.Result { id = 1; output = "caf\xc3\xa9" }))

(* Every operation names itself the same way on the wire and in a message about
   it, since a timeout and a journal record both quote that name. *)

let () =
  check "an op names itself as the wire does"
    (Proto.name
       (Proto.Fetch { url = "x"; render = Proto.Raw; max_bytes = None })
     = "fetch"
    && Proto.name (Proto.Head { url = "x" }) = "head"
    && Proto.name (Proto.Run { program = "x"; args = [] }) = "run")

let () =
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
