(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
  ---------------------------------------------------------------------------*)

(* Lifetime and finalizer checks for the C bindings.

   Freeing a session reads through the engine it belongs to, and the garbage
   collector gives no ordering between their finalizers. Dropping both at once
   could once close the engine first and then free the session through it. The
   session now keeps its engine alive, and this test notices if that is ever
   lost.

   The fault it looks for is a use-after-free, so passing means only that it did
   not crash. Run it under a sanitiser for a stronger signal.

   It needs a real model, so it is gated on DS4_LIVE. Only one engine may exist
   per process, so this covers the worker-domain case alone. *)

module Agent = Ds4.Agent
module V4 = Ds4.V4

let check name cond failures =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

(* [closed_agent_refuses engine] closes an agent twice and is whether it then
   refuses to be used. The second close must not reach a cache the first one
   freed, and a call after it must say so rather than fault in the stubs. *)
let closed_agent_refuses engine =
  let agent = Agent.create engine ~ctx_size:512 in
  Agent.close agent;
  Agent.close agent;
  match Agent.stats agent with
  | exception Invalid_argument _ -> true
  | _ -> false

(* Build an engine and a session, then drop both. Kept in its own function so
   that they are genuinely unreachable afterwards. *)
let make_and_drop env xdg model_path ~with_worker =
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let cache = Xdge.cache_dir xdg in
  let engine =
    if with_worker then
      V4.create ~sw
        ~domain_mgr:(Eio.Stdenv.domain_mgr env)
        ~cache
        ~model:Eio.Path.(fs / model_path)
        ()
    else V4.create ~cache ~model:Eio.Path.(fs / model_path) ()
  in
  let session = V4.Session.create engine ~ctx_size:512 ~seed:1L in
  (* Use both so that neither is optimised away before the drop. *)
  let pos = V4.Session.pos session in
  let vocab = V4.vocab_size engine in
  (pos, vocab, closed_agent_refuses engine)

let run env xdg model_path =
  let failures = ref 0 in

  Printf.printf "loading %s …\n%!" (Filename.basename model_path);
  let pos, vocab, agent_refuses =
    make_and_drop env xdg model_path ~with_worker:true
  in
  check "session and engine usable before collection"
    (vocab > 0 && pos >= 0)
    failures;
  check "a closed agent releases its session and refuses to be used"
    agent_refuses failures;

  (* Both are unreachable now. The first collection finalizes the session and
     releases its hold on the engine, and the second finalizes the engine. A
     crash in either is the fault this test looks for. *)
  Gc.full_major ();
  Gc.full_major ();
  check "collecting session and engine together did not crash" true failures;

  (* Only one engine may exist per process, so the case where the finalizers run
     on the creating domain cannot also be covered here. Opening a second raises
     rather than racing on the environment, which is worth checking too. *)
  check "a second engine is refused rather than created"
    (match make_and_drop env xdg model_path ~with_worker:false with
    | exception Invalid_argument _ -> true
    | _ -> false)
    failures;

  if !failures = 0 then print_string "\nLive lifetime test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None -> print_string "SKIP - live lifetime test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match Ds4_cli.Model.resolve ~dir:(Ds4_cli.Model.dir xdg) None with
      | Error e -> failwith e
      | Ok model_path -> run env xdg model_path)
