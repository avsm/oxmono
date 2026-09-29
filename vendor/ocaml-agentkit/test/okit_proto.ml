(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Unit tests of okitd's line protocol.

   Two properties matter. Every message shape must survive a round trip, since
   both ends are the same binary and a shape that does not survive is a bug no
   version negotiation will catch. And a line that does not parse must come
   back as [`Bad] with the offending text, never as a skipped line, because a
   skipped line desynchronises a stream of sequential calls silently.

   The two exact-byte cases pin the wire against the design's examples, so a
   change of member order or of the flat argument encoding shows up here. *)

module Proto = Okit.Proto

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
  server_round_trip "build round trips" (call 1 (Proto.Build { targets = "." }));
  server_round_trip "test round trips" (call 2 Proto.Test);
  server_round_trip "promote round trips"
    (call 3 (Proto.Promote { path = "lib/a.ml" }));
  server_round_trip "project round trips"
    (call 4 (Proto.Project { module_ = "Fix" }));
  server_round_trip "after_write round trips"
    (call 5 (Proto.After_write { path = "lib/a.ml"; verb = "edit" }));
  server_round_trip "outline round trips"
    (call 6 (Proto.Outline { path = "lib/a.ml"; source = "let x = 1\n" }));
  server_round_trip "type_at round trips"
    (call 7
       (Proto.Type_at
          { path = "lib/a.ml"; source = "let x = 1\n"; line = 1; col = 4 }));
  server_round_trip "locate round trips"
    (call 8
       (Proto.Locate
          { path = "lib/a.ml"; source = "let x = 1\n"; line = 1; col = 4 }));
  server_round_trip "errors round trips"
    (call 9 (Proto.Errors { path = "lib/a.ml"; source = "let x = 1\n" }));
  server_round_trip "occurrences round trips"
    (call 10
       (Proto.Occurrences
          { path = "lib/a.ml"; source = "let x = 1\n"; line = 1; col = 4 }));
  server_round_trip "search round trips"
    (call 11
       (Proto.Search
          {
            path = "lib/a.ml";
            source = "let x = 1\n";
            query = "int -> string";
            limit = 10;
          }));
  server_round_trip "complete round trips"
    (call 12
       (Proto.Complete
          {
            path = "lib/a.ml";
            source = "let x = 1\n";
            line = 1;
            col = 4;
            prefix = "List.";
          }));
  server_round_trip "bash round trips"
    (call 13 (Proto.Bash { command = "echo \"hi\"\n" }));
  server_round_trip "shutdown round trips" Proto.Shutdown;
  client_round_trip "hello round trips"
    (Proto.Hello
       { status = "okit: dune tools active"; dune = true; merlin = false });
  client_round_trip "trace round trips"
    (Proto.Trace { id = Some 1; line = "dune: build ." });
  client_round_trip "trace without id round trips"
    (Proto.Trace { id = None; line = "okitd: started" });
  client_round_trip "result round trips"
    (Proto.Result { id = 1; output = "build ok" })

(* Exact bytes: the design's own examples, one message per line. *)

let () =
  check "hello line"
    (to_client_bytes
       (Proto.Hello
          {
            status = "okit: dune tools active (merlin found)";
            dune = true;
            merlin = true;
          })
    = "{\"hello\":{\"status\":\"okit: dune tools active (merlin \
       found)\",\"dune\":true,\"merlin\":true}}\n");
  check "build call line"
    (to_server_bytes (Proto.Call { id = 1; op = Proto.Build { targets = "." } })
    = "{\"call\":{\"id\":1,\"op\":\"build\",\"targets\":\".\"}}\n");
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
    "{\"call\":{\"id\":1,\"op\":\"test\"},\"shutdown\":{}}";
  bad_to_server "unknown op is bad"
    "{\"call\":{\"id\":1,\"op\":\"frobnicate\"}}";
  bad_to_server "missing op argument is bad"
    "{\"call\":{\"id\":1,\"op\":\"build\"}}";
  (* The argument written last, for an operation whose others are shared with
     the rest of the merlin family, so a family member that read only the
     shared ones would pass here. *)
  bad_to_server "a search with no limit is bad"
    "{\"call\":{\"id\":1,\"op\":\"search\",\"path\":\"a.ml\",\"source\":\"\",\"query\":\"int\"}}";
  bad_to_server "missing id is bad" "{\"call\":{\"op\":\"test\"}}";
  bad_to_server "non-integer id is bad"
    "{\"call\":{\"id\":1.5,\"op\":\"test\"}}";
  bad_to_server "an id past the bound is bad"
    "{\"call\":{\"id\":1e300,\"op\":\"test\"}}";
  bad_to_server "a column past the bound is bad"
    "{\"call\":{\"id\":1,\"op\":\"type_at\",\"path\":\"a.ml\",\"source\":\"\",\"line\":1,\"col\":1e300}}";
  bad_to_server "a raw invalid byte on the wire is bad"
    "{\"call\":{\"id\":1,\"op\":\"bash\",\"command\":\"a\xffb\"}}";
  bad_to_server "a client message is bad on the server side"
    "{\"result\":{\"id\":1,\"output\":\"ok\"}}";
  bad_to_client "missing hello member is bad"
    "{\"hello\":{\"status\":\"x\",\"dune\":true}}";
  bad_to_client "wrong hello type is bad"
    "{\"hello\":{\"status\":\"x\",\"dune\":\"yes\",\"merlin\":true}}";
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
        Proto.Trace { id = Some 1; line = "dune: build ." };
        Proto.Result { id = 1; output = "build ok" };
      ]
  in
  let r = Eio.Buf_read.of_string two in
  check "first of two messages"
    (Proto.read_to_client r
    = `Msg (Proto.Trace { id = Some 1; line = "dune: build ." }));
  check "second of two messages"
    (Proto.read_to_client r
    = `Msg (Proto.Result { id = 1; output = "build ok" }));
  check "eof after two messages" (Proto.read_to_client r = `Eof)

(* Bytes that are not valid UTF-8. Command output and file contents carry them,
   so the message must survive rather than fault. *)

let () =
  let msg = Proto.Call { id = 1; op = Proto.Bash { command = "a\xffb" } } in
  let read =
    Proto.read_to_server (Eio.Buf_read.of_string (to_server_bytes msg))
  in
  check "an invalid byte survives as U+FFFD"
    (read
    = `Msg
        (Proto.Call { id = 1; op = Proto.Bash { command = "a\xef\xbf\xbdb" } })
    );
  check "valid utf-8 is untouched"
    (Proto.read_to_client
       (Eio.Buf_read.of_string
          (to_client_bytes (Proto.Result { id = 1; output = "caf\xc3\xa9" })))
    = `Msg (Proto.Result { id = 1; output = "caf\xc3\xa9" }))

(* A line over the reader's buffer. Production readers are bounded, and a
   result carrying build output is the line that reaches the bound. *)

let () =
  let long = String.make 4096 'x' in
  let read =
    Eio_mock.Backend.run @@ fun () ->
    let src = Eio.Flow.string_source (long ^ "\n") in
    Proto.read_to_server (Eio.Buf_read.of_flow ~max_size:256 src)
  in
  check "an over-long line is bad"
    (match read with
    | `Bad line ->
        String.length line < 512
        && String.starts_with ~prefix:"xxx" line
        && String.ends_with ~suffix:"byte limit)" line
    | _ -> false);
  check "max_line is what a reader should be given"
    (Agentkit.Line.max_line = 16 * 1024 * 1024)

let () =
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
