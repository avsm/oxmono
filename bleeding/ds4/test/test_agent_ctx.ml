(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The window arithmetic behind the agent loop, which needs no engine.

   What matters here is the band just below the context bound, where the prompt
   fits but a reply does not. A turn that runs there is cut off in the middle,
   and a cut off tool call is never made, so the agent must refuse the turn and
   grow instead. *)

module Agent = Ds4.Agent

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let max_tokens = 2048
let ctx = 32768

let room ?(squeeze = false) needed =
  Agent.has_room ~squeeze ~max_tokens ~needed ~ctx

let grow ?(max_ctx_size = 262144) ?(ctx = ctx) needed =
  Agent.grow_to ~max_tokens ~max_ctx_size ~needed ~ctx

let () =
  (* A short conversation has room for a whole reply. *)
  check "a short prompt has room" (room 1000);
  (* The largest prompt that still leaves [max_tokens] and the token generation
     stops on, and the first one that does not. *)
  check "the last prompt with a full reply's room fits"
    (room (ctx - max_tokens - 1));
  check "one token more does not" (not (room (ctx - max_tokens)));
  (* The dead zone: the prompt fits, so the old test passed it, but the reply
     it leaves room for is shorter than the one the model was told to write. *)
  check "a prompt in the dead zone is refused" (not (room (ctx - 1000)));
  check "a prompt one token short of the bound is refused"
    (not (room (ctx - 1)));
  check "a prompt that fills the window is refused" (not (room ctx));

  (* Squeezed, at the ceiling, the same turns run. Refusing them would fail a
     conversation the agent can still answer, just not at full length. *)
  check "the dead zone runs when squeezed" (room ~squeeze:true (ctx - 1000));
  check "the last token of the window runs when squeezed"
    (room ~squeeze:true (ctx - 1));
  (* A prompt that does not fit is exhaustion however the turn is run. *)
  check "a prompt that fills the window is refused when squeezed"
    (not (room ~squeeze:true ctx));
  check "a prompt past the window is refused when squeezed"
    (not (room ~squeeze:true (ctx + 1)));

  (* Growth doubles where doubling is enough, and jumps straight to what the
     turn needs where it is not. *)
  check "the dead zone grows by doubling" (grow (ctx - 1000) = Some (2 * ctx));
  check "a prompt beyond twice the window grows to fit it"
    (grow 100_000 = Some (100_000 + max_tokens + 1));

  (* At the ceiling there is nothing to grow to, and the turn is squeezed
     rather than refused. *)
  check "a window at the ceiling does not grow"
    (grow ~max_ctx_size:ctx (ctx - 1000) = None);
  (* The ceiling is close enough to reach but too close to reply in. Growing
     there would cost a prefill of the whole conversation and leave the turn
     raising again at the larger size. *)
  check "growth that would still leave no reply's room is refused"
    (grow ~max_ctx_size:(ctx + 100) (ctx - 1000) = None);
  check "growth to exactly a full reply's room is taken"
    (grow ~max_ctx_size:(ctx + 1049) (ctx - 1000) = Some (ctx + 1049));

  (* The property the retry depends on: whatever [grow_to] accepts, the turn
     that follows it does not raise at once. *)
  let retries_cleanly =
    List.for_all
      (fun (max_ctx_size, ctx, needed) ->
        match Agent.grow_to ~max_tokens ~max_ctx_size ~needed ~ctx with
        | None -> true
        | Some ctx_size ->
            ctx_size > ctx
            && Agent.has_room ~squeeze:false ~max_tokens ~needed ~ctx:ctx_size)
      (List.concat_map
         (fun max_ctx_size ->
           List.concat_map
             (fun ctx ->
               List.map
                 (fun needed -> (max_ctx_size, ctx, needed))
                 [ 0; 1; 100; ctx - max_tokens; ctx - 1; ctx; ctx + 5000 ])
             [ 512; 4096; 32768 ])
         [ 512; 4097; 33000; 40000; 262144 ])
  in
  check "an accepted growth always leaves a full reply's room" retries_cleanly;

  if !failures = 0 then Printf.printf "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d test(s) failed.\n" !failures;
    exit 1
  end
