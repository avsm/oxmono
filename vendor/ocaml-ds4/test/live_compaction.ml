(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Compaction against a real model.

   A small [ctx_size] equal to [max_ctx_size] means the context can never
   grow, so the first turn that no longer fits must be rescued by compaction
   or not at all. The properties that matter: it does rescue the turn, the
   conversation really does shrink, the agent never reports a context larger
   than [ctx_size], and the exchange goes on working afterwards. What a
   summary chooses to keep is model-dependent and is reported rather than
   asserted, as the soft signals in live_agent_mock are. *)

module Agent = Ds4.Agent
module Tool = Ds4.Tool
module V4 = Ds4.V4
module Model = Ds4_cli.Model

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* A paragraph of filler long enough that a handful of them fill a small
   context, ending in an instruction any model follows without tools. *)
let filler n =
  String.concat " " (List.init n (fun i -> Printf.sprintf "word%04d" i))
  ^ ". Reply with just the word ok."

let ctx_size = 3072

(* Drive one exchange, watching for [Compacted] and the invariant that
   matters most: the context this agent reports never exceeds [ctx_size],
   compaction included. Returns the reply's text. *)
let send ~compactions ~max_ctx_used agent prompt =
  let reply = Buffer.create 64 in
  let on_event = function
    | Agent.Compacted c ->
        compactions := c :: !compactions;
        Printf.printf "  [compacted %d -> %d tokens]\n%!" c.Agent.before
          c.Agent.after
    | Agent.Stats s -> max_ctx_used := max !max_ctx_used s.Agent.ctx_used
    | Agent.Content c -> Buffer.add_string reply c
    | _ -> ()
  in
  Agent.send agent ~on_event prompt;
  Buffer.contents reply

(* A mock tool whose one call returns far more text than fits a fresh
   [ctx_size] conversation, so handling it must compact rather than send a
   result cut in the middle. *)
let mock_dump () =
  let codec =
    let open Dsml.Codec in
    Invoke.map "dump" Fun.id
    |> Invoke.param ~enc:Fun.id "topic" string
         ~description:"what to fetch reference data about"
    |> Invoke.seal
  in
  Tool.v ~description:"Fetch reference data. Call this before answering." codec
    (fun _topic ->
      String.concat " " (List.init 1200 (fun i -> Printf.sprintf "ref%04d" i)))

let run env xdg model_path =
  let fs = Eio.Stdenv.fs env in
  let cache = Xdge.cache_dir xdg in
  Printf.printf "loading %s …\n%!" (Filename.basename model_path);
  let engine = V4.create ~cache ~model:Eio.Path.(fs / model_path) () in

  (* ---- Growth-exhausted compaction, over plain prompts alone ---- *)
  let agent =
    Agent.create engine ~temperature:0.0 ~max_tokens:64 ~ctx_size
      ~max_ctx_size:ctx_size
      ~system:"Reply with just the word ok, unless asked something else."
  in
  let secret = "PLUM-7734" in
  let compactions = ref [] and max_ctx_used = ref 0 in
  ignore
    (send ~compactions ~max_ctx_used agent
       (Printf.sprintf
          "Remember this secret word for later, you will be asked for it: %s. \
           Reply with just the word ok."
          secret));
  let rec pad n =
    if n >= 8 || !compactions <> [] then n
    else begin
      ignore (send ~compactions ~max_ctx_used agent (filler 350));
      pad (n + 1)
    end
  in
  let rounds = pad 0 in
  Printf.printf "  filled the context over %d round(s)\n%!" rounds;
  check "a growth-exhausted conversation is compacted rather than failing"
    (!compactions <> []);
  check "every compaction shrinks the conversation"
    (List.for_all (fun c -> c.Agent.after < c.Agent.before) !compactions);
  check "every compaction leaves a non-empty summary"
    (List.for_all (fun c -> String.trim c.Agent.summary <> "") !compactions);
  check "the reported context never exceeds ctx_size" (!max_ctx_used <= ctx_size);

  (* The exchange must go on working: two more rounds of filler and then a
     plain question, none of which may raise. *)
  ignore (pad (rounds + 2));
  let recalled =
    send ~compactions ~max_ctx_used agent
      "What was the secret word? Reply with just the word, nothing else."
  in
  Printf.printf "%s - the summary kept the secret word through compaction\n%!"
    (if contains recalled secret then "ok  " else "note");
  Agent.close agent;

  (* ---- Compaction triggered by an oversized tool result ---- *)
  let compactions2 = ref [] and max_ctx_used2 = ref 0 in
  let dumped =
    Agent.create engine ~temperature:0.0 ~max_tokens:64 ~ctx_size
      ~max_ctx_size:ctx_size
      ~system:
        "Always call the dump tool first, on any request, before replying."
      ~tools:[ mock_dump () ]
  in
  (try
     ignore
       (send ~compactions:compactions2 ~max_ctx_used:max_ctx_used2 dumped
          "Call dump with topic \"reference\", then reply with just the word \
           ok.")
   with e ->
     Printf.printf "note - dump exchange raised %s\n%!" (Printexc.to_string e));
  Printf.printf
    "%s - an oversized tool result is compacted rather than refused outright\n\
     %!"
    (if !compactions2 <> [] then "ok  " else "note");
  check "the reported context never exceeds ctx_size for the dump agent too"
    (!max_ctx_used2 <= ctx_size);
  Agent.close dumped;

  if !failures = 0 then print_string "\nLive compaction test passed.\n"
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
      Printf.printf "SKIP - %s live compaction test (set DS4_LIVE=1 to run)\n"
        backend
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Model.resolve ~dir:(Model.dir xdg) None with
      | Error e -> failwith e
      | Ok model_path -> run env xdg model_path)
