(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The two halves of [humpty expect] that decide what a transcript says: the
   script reader and the scrubber.

   What matters in the reader is that a line it will not run is refused by
   number rather than skipped, since a script whose middle line was dropped
   runs to a transcript that looks right. What matters in the scrubber is that
   it removes everything that varies between two runs of the same script, and
   nothing else, since either failure shows up as a cram test that cannot be
   trusted. *)

module Expect = Humpty_cmd.Expect

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let holds s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let calls items =
  List.filter_map
    (function Expect.Call c -> Some c | Expect.Prompt _ -> None)
    items

(* The script reader. *)

let () =
  let script =
    "# a comment\n\nproject {}\n  build {\"targets\":[\"lib\"]}  \n\n"
  in
  match Expect.parse script with
  | Error e ->
      print_endline e;
      check "reads a script" false
  | Ok items ->
      check "reads every call" (List.length (calls items) = 2);
      (* Against the whole list, since [calls] drops prompts and would hide a
         comment or a blank that had become an item of its own. *)
      check "a comment is not an item" (List.length items = 2);
      let a = List.nth (calls items) 0 and b = List.nth (calls items) 1 in
      check "first call" (a.tool = "project" && a.arguments = "{}");
      (* The line is the script's own, counting the blanks and comments, since
         it is what a person edits. *)
      check "first line is the script's" (a.line = 3);
      check "second call"
        (b.tool = "build" && b.arguments = "{\"targets\":[\"lib\"]}");
      check "second line" (b.line = 4)

let () =
  (* Arguments are kept as written, so that the tool sees what a model would
     have sent, spaces and all. *)
  match Expect.parse "write {\"cap\":\"\", \"path\":\"a b.ml\"}\n" with
  | Ok [ Expect.Call c ] ->
      check "arguments kept verbatim"
        (c.arguments = "{\"cap\":\"\", \"path\":\"a b.ml\"}")
  | _ -> check "arguments kept verbatim" false

let () =
  match Expect.parse "project {}\r\n" with
  | Ok [ Expect.Call c ] ->
      check "a carriage return is not part of the arguments" (c.arguments = "{}")
  | _ -> check "a carriage return is not part of the arguments" false

let () = check "an empty script has no calls" (Expect.parse "" = Ok [])

let refused name script named =
  match Expect.parse script with
  | Ok _ -> check name false
  | Error e -> check name (holds e named && holds e "build {}")

let () =
  refused "a bare tool name is refused, by line" "\nbuild\n" "line 2";
  refused "a target that is not JSON is refused" "build .\n" "line 1";
  refused "arguments alone are refused" "{}  x\n" "line 1";
  (* The refusal names the line the script has, not the calls before it. *)
  refused "a later bad line is named" "project {}\n# c\nbuild\n" "line 3"

(* Prompt lines. *)

let () =
  let script = "? describe this repository\nproject {}\n?   trimmed   \n" in
  match Expect.parse script with
  | Ok [ Expect.Prompt a; Expect.Call c; Expect.Prompt b ] ->
      check "a prompt keeps its text" (a.text = "describe this repository");
      check "a prompt keeps its line" (a.line = 1);
      check "order is the script's" (c.tool = "project" && c.line = 2);
      check "a prompt's text is trimmed" (b.text = "trimmed" && b.line = 3)
  | _ -> check "prompts and calls mix" false

let () =
  refused "a bare ? is refused, by line" "project {}\n?\n" "line 2";
  refused "a ? with only spaces is refused" "?   \n" "line 1"

(* The scrubber. *)

let () =
  let roots = Expect.roots [ "/tmp/ws/"; "/private/tmp/ws"; "."; "/" ] in
  check "roots keeps both absolute names"
    (roots = [ "/private/tmp/ws"; "/tmp/ws" ]);
  check "roots drops a relative name" (not (List.mem "." roots));
  check "roots merges duplicates" (Expect.roots [ "/a/b"; "/a/b/" ] = [ "/a/b" ]);
  check "roots puts the longest first"
    (Expect.roots [ "/a"; "/a/b/c" ] = [ "/a/b/c"; "/a" ])

let () =
  let roots = Expect.roots [ "/tmp/ws"; "/private/tmp/ws" ] in
  let scrub = Expect.scrub ~roots in
  check "the workspace becomes $WS"
    (scrub "File \"/tmp/ws/lib/x.ml\", line 1:"
    = "File \"$WS/lib/x.ml\", line 1:");
  check "the resolved name becomes $WS too"
    (scrub "/private/tmp/ws/lib/x.ml" = "$WS/lib/x.ml");
  check "both names in one line"
    (scrub "/tmp/ws and /private/tmp/ws" = "$WS and $WS");
  check "a path that is not the workspace is left alone"
    (scrub "/tmp/other/x.ml" = "/tmp/other/x.ml");
  check "durations are dropped" (scrub "dune: reply 0.4s" = "dune: reply");
  check "a whole-second wait is dropped"
    (scrub "dune: waiting for socket 5s" = "dune: waiting for socket");
  check "a word ending in s is kept"
    (scrub "dune: diagnostics" = "dune: diagnostics");
  check "a duration is only dropped from the end of a line"
    (scrub "dune: 0.4s reply" = "dune: 0.4s reply");
  check "a duration with nothing before it is kept" (scrub "5s" = "5s");
  check "every line is scrubbed"
    (scrub "dune: reply 0.4s\n/tmp/ws/x\n" = "dune: reply\n$WS/x\n");
  check "an empty root list changes nothing"
    (Expect.scrub ~roots:[] "/tmp/ws/x" = "/tmp/ws/x")

(* The status line, which has one line for a message that was written for
   several. *)

let () =
  check "a wrapped message keeps every sentence"
    (Expect.one_line
       "no dune server answers at /tmp/ws/_build/.rpc/dune,\n\
        and none could be started. Stop the server that owns the workspace, or\n\
        remove the socket if none is running."
    = "no dune server answers at /tmp/ws/_build/.rpc/dune, and none could be \
       started. Stop the server that owns the workspace, or remove the socket \
       if none is running.");
  check "one line is left alone"
    (Expect.one_line "no dune tools" = "no dune tools");
  check "blank lines do not become spaces" (Expect.one_line "a\n\nb" = "a b");
  (* The server's own output is dropped rather than flattened: it is several
     lines of another program's diagnostics and it names a pid. *)
  check "the server's last output is dropped"
    (Expect.one_line
       "the dune okit started in /tmp/ws exited without opening its\n\
        RPC socket.\n\
        The server's last output was:\n\
        Error: Another Dune instance is currently running. (pid 4242)"
    = "the dune okit started in /tmp/ws exited without opening its RPC socket."
    );
  check "a message with no tail keeps its last line"
    (Expect.one_line "one.\ntwo." = "one. two.")

let () =
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
