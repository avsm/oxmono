(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Live integration check with the real DeepSeek-V4 engine driving the agent loop,
   and the tools' I/O mocked. *)

module Agent = Ds4.Agent
module Tool = Ds4.Tool
module V4 = Ds4.V4

let sentinel = "PINEAPPLE-42"

(* A mock [read] tool: a scripted Eio_mock.Flow stands in for the file. The flow
   is re-armed on each call so repeated reads stay deterministic. *)
let mock_read () =
  let flow = Eio_mock.Flow.make "secret.txt" in
  let codec =
    let open Dsml.Codec in
    Invoke.map "read" Fun.id
    |> Invoke.param ~enc:Fun.id "path" string ~description:"path of the file"
    |> Invoke.seal
  in
  Tool.v ~description:"Read a UTF-8 text file and return its contents." codec
    (fun path ->
      Printf.eprintf "  (mock read path=%s)\n%!" path;
      Eio_mock.Flow.on_read flow [ `Return sentinel; `Raise End_of_file ];
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow flow ~max_size:4096))

(* A mock [write] tool: the bytes are copied into [buf] instead of the
   filesystem, so the test can assert what the model wrote. *)
let mock_write buf =
  let codec =
    let open Dsml.Codec in
    Invoke.map "write" (fun path content -> (path, content))
    |> Invoke.param ~enc:fst "path" string ~description:"destination path"
    |> Invoke.param ~enc:snd "content" string ~description:"contents to write"
    |> Invoke.seal
  in
  Tool.v ~description:"Create or overwrite a UTF-8 text file." codec
    (fun (path, content) ->
      Printf.eprintf "  (mock write path=%s bytes=%d)\n%!" path
        (String.length content);
      Eio.Flow.copy_string content (Eio.Flow.buffer_sink buf);
      Printf.sprintf "wrote %d bytes to %s" (String.length content) path)

(* A mock [dns] tool: canned addresses instead of a real getaddrinfo. *)
let mock_dns () =
  let codec =
    let open Dsml.Codec in
    Invoke.map "dns" Fun.id
    |> Invoke.param ~enc:Fun.id "host" string ~description:"hostname to resolve"
    |> Invoke.seal
  in
  Tool.v ~description:"Resolve a hostname to IP addresses." codec (fun host ->
      Printf.eprintf "  (mock dns host=%s)\n%!" host;
      "203.0.113.7")

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let run env xdg model_path =
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let cache = Xdge.cache_dir xdg in
  Printf.printf "loading %s …\n%!" (Filename.basename model_path);
  (* Exercise the worker-domain path: the engine runs off the main domain. *)
  let engine =
    V4.create ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~cache
      ~model:Eio.Path.(fs / model_path)
      ()
  in
  let written = Buffer.create 64 in
  let agent =
    Agent.create engine ~temperature:0.0 ~max_tokens:512
      ~system:
        "You are a tool-using assistant. To work with a file you MUST call the \
         `read` and `write` tools rather than guessing its contents. Report \
         exactly what the tools return."
      ~tools:[ mock_read (); mock_write written; mock_dns () ]
  in
  (* Collect events while echoing the conversation. *)
  let tool_calls = ref [] and tool_results = ref [] in
  let cut_offs = ref [] in
  let content = Buffer.create 256 in
  let on_event : Agent.event -> unit = function
    | Agent.Content c ->
        Buffer.add_string content c;
        print_string c;
        flush stdout
    | Agent.Reasoning _ -> ()
    | Agent.Tool_call tc ->
        tool_calls := tc.name :: !tool_calls;
        Printf.printf "\n[tool_call %s %s]\n%!" tc.name tc.arguments
    | Agent.Tool_result (n, r) ->
        tool_results := (n, r) :: !tool_results;
        Printf.printf "[tool_result %s -> %S]\n%!" n r
    | Agent.Expanded n -> Printf.printf "[context grew to %d]\n%!" n
    | Agent.Squeezed n ->
        Printf.printf "[context full, %d tokens to reply]\n%!" n
    | Agent.Cut_off c ->
        cut_offs := c :: !cut_offs;
        Printf.printf "[reply cut off at %d tokens, tool_call %b]\n%!"
          c.Agent.tokens c.Agent.tool_call
    | Agent.Stats _ -> () (* timings vary, so keep them out of the output *)
    | Agent.Compacted c ->
        Printf.printf "[compacted from %d to %d tokens]\n%!" c.Agent.before
          c.Agent.after
    | Agent.Done -> print_newline ()
  in
  Agent.send agent ~on_event
    "Read the file secret.txt, then write its exact contents to result.txt. \
     Tell me when you are done.";

  (* The deterministic part: the model called the mock tools and the mocked I/O
     flowed through the conversation. *)
  let failures = ref 0 in
  let check name cond =
    if cond then Printf.printf "ok   - %s\n" name
    else begin
      incr failures;
      Printf.printf "FAIL - %s\n" name
    end
  in
  check "a reply the model ended is not reported as cut off" (!cut_offs = []);
  check "model called the read tool" (List.mem "read" !tool_calls);
  check "mocked read result was fed back"
    (List.exists (fun (n, r) -> n = "read" && r = sentinel) !tool_results);
  check "model called the write tool" (List.mem "write" !tool_calls);
  (* Soft signals: did the model carry the mocked value through to its write and
     its final answer? (Model-dependent, so reported rather than asserted.) *)
  Printf.printf "%s - mock write received the read value\n"
    (if contains (Buffer.contents written) sentinel then "ok  " else "note");
  Printf.printf "%s - final answer repeats the mocked value\n"
    (if contains (Buffer.contents content) sentinel then "ok  " else "note");

  (* A second agent, whose ceiling is too low to write the call it is asked
     for. The exchange must not end quietly: a discarded call is work the model
     believes it did, and an agent that reports that as a finished turn is
     worse than one that stops. *)
  let starved =
    Agent.create engine ~temperature:0.0 ~max_tokens:24
      ~system:
        "You are a tool-using assistant. To work with a file you MUST call the \
         `read` and `write` tools rather than guessing its contents."
      ~tools:[ mock_read (); mock_write (Buffer.create 16); mock_dns () ]
  in
  let cut = ref [] and turns = ref 0 in
  let on_starved = function
    | Agent.Cut_off c -> cut := c :: !cut
    | Agent.Stats s -> turns := s.Agent.turns
    | _ -> ()
  in
  (match
     Agent.send starved ~on_event:on_starved
       "Write the whole text of the first chapter of Genesis to genesis.txt \
        with the write tool."
   with
  | () ->
      (* A ceiling this low may leave a plain refusal, or cut a call and see
         the model answer the note with a smaller call or with text, all of
         which end the exchange. What must not happen is a turn that ended in
         a discarded call being the last word: each such cut is reported and
         buys the model another turn, so the turns outnumber them. *)
      let discarded =
        List.length (List.filter (fun (c : Agent.cut) -> c.tool_call) !cut)
      in
      check "a discarded call is never the last turn" (!turns > discarded);
      Printf.printf "note - exchange ended itself after %d discarded call(s)\n"
        discarded
  | exception Agent.Tool_call_cut_off { tokens; attempts } ->
      check "a call the ceiling cut in half is raised rather than passed over"
        (tokens > 0 && attempts > 1);
      check "the turns that failed are on the record as cuts"
        (List.exists (fun (c : Agent.cut) -> c.tool_call) !cut));
  Agent.close starved;

  if !failures = 0 then print_string "\nLive mock test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  let backend =
    match V4.backend with `Metal -> "metal" | `Cuda -> "cuda" | `Cpu -> "cpu"
  in
  match Sys.getenv_opt "DS4_LIVE" with
  | None ->
      Printf.printf "SKIP - %s live agent test (set DS4_LIVE=1 to run)\n"
        backend
  | Some _ -> (
      Printf.printf "running %s live agent test\n%!" backend;
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Ds4_cli.Model.resolve ~dir:(Ds4_cli.Model.dir xdg) None with
      | Error e -> failwith e
      | Ok model_path -> run env xdg model_path)
