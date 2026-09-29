(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One end-to-end wake-up against the real model, driven through [numpty once]
   as a person would drive it.

   It fires a task, fetches a page from a server this test stands up on the
   loopback, and asks for what was read to be written into memory. Nothing here
   reaches the internet.

   What is checked is the account rather than the reply. The journal must hold
   every step, from the run_start through the wake, the brief, the prompt, the
   fetch and its result, to the handover and the run_stop, and the memory version
   must have advanced by exactly the number of memory_write records. A version
   that moved without a record, or a record with no version behind it, is the two
   stores disagreeing, which is what the write order exists to prevent.

   Skips unless DS4_LIVE is set, since it loads a multi-gigabyte model. The
   binary to drive is passed on the command line, as okit_server's is, so the
   test runs the one this build produced. *)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Store = Numpty_daemon.Store

let exe = if Array.length Sys.argv > 1 then Sys.argv.(1) else "numpty-cpu"

let live =
  match Sys.getenv_opt "DS4_LIVE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let page =
  "<html><head><title>The upstream tag</title></head><body><p>The upstream tag \
   moved to v0.9 on the eighth of August.</p></body></html>"

let serve flow =
  let r = Eio.Buf_read.of_flow flow ~max_size:0x10000 in
  let _request = Eio.Buf_read.line r in
  let rec drain () =
    match Eio.Buf_read.line r with "" -> () | _ -> drain ()
  in
  (try drain () with End_of_file -> ());
  let text =
    Printf.sprintf
      "HTTP/1.1 200 OK\r\n\
       Content-Type: text/html; charset=utf-8\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      (String.length page) page
  in
  try Eio.Flow.copy_string text flow with Eio.Io _ -> ()

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
            serve flow)
      done;
      `Stop_daemon);
  f (Printf.sprintf "http://127.0.0.1:%d/page.html" port)

let records dir =
  let seen = ref [] in
  Journal.iter dir (fun r -> seen := r :: !seen);
  List.rev !seen

let has records name =
  List.exists
    (fun (r : Journal.record) -> Journal.kind_name r.Journal.kind = name)
    records

let run env =
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-numpty-wake" "" in
  Sys.remove tmp;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
  @@ fun () ->
  with_site ~sw ~net:(Eio.Stdenv.net env) @@ fun url ->
  let prompt =
    Printf.sprintf
      "Fetch %s and read it. Then call memory_write once, with id \
       \"upstream-tag\", kind \"fact\", a title of your own, a body saying \
       what the page said about the upstream tag, and a why. Then stop. Do not \
       fetch anything else."
      url
  in
  let child =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      [ exe; "once"; "--store"; tmp; "--ctx"; "16384"; "--seed"; "1"; prompt ]
  in
  let status = Eio.Process.await child in
  check "the wake-up exited cleanly" (status = `Exited 0);
  let root = Eio.Path.(fs / tmp) in
  let journal = records (Store.journal_dir root) in
  List.iter
    (fun name ->
      check
        (Printf.sprintf "the journal holds a %s record" name)
        (has journal name))
    [
      "run_start";
      "wake";
      "brief";
      "prompt";
      "tool_call";
      "tool_result";
      "stats";
      "memory_write";
      "handover";
      "run_stop";
    ];
  check "the fetch was journalled before it was made"
    (List.exists
       (fun (r : Journal.record) ->
         match r.Journal.kind with
         | Journal.Tool_call tc -> tc.Journal.name = "fetch"
         | _ -> false)
       journal);
  let writes =
    List.length
      (List.filter
         (fun (r : Journal.record) ->
           match r.Journal.kind with
           | Journal.Memory_write _ -> true
           | _ -> false)
         journal)
  in
  let clock = Eio.Stdenv.clock env in
  let memory = Memory.create ~clock (Store.memory_dir root) in
  check "the memory version advanced by exactly the number of writes"
    (Memory.version memory = writes);
  check "and something was written down" (writes > 0);
  check "the entry the wake-up was asked for is there"
    (List.exists
       (fun (e : Memory.entry) -> e.Memory.id = "upstream-tag")
       (Memory.entries memory));
  (* The handover names the version the wake-up finished at, which is what the
     next brief is assembled from. *)
  check "the handover names the version memory ended at"
    (List.exists
       (fun (r : Journal.record) ->
         match r.Journal.kind with
         | Journal.Handover v -> v = Memory.version memory
         | _ -> false)
       journal)

let () =
  if not live then
    print_endline "live_numpty_wake: skipped (set DS4_LIVE=1 to run)"
  else begin
    Eio_main.run run;
    if !failures > 0 then begin
      Printf.printf "\n%d failure(s)\n" !failures;
      exit 1
    end
  end
