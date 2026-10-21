(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
  ---------------------------------------------------------------------------*)

(* Speculative decoding against a model that carries a draft head.

   Two properties matter. Greedy decoding must commit the same tokens whether
   the draft head is used or not, since acceptance is meant to change the cost
   and never the text. And a session that committed a drafted token the caller
   does not keep must be rewound without losing the rest of its cache, since
   the agent does exactly that at the end of most turns and a full refill of
   the conversation each time would cost more than speculation saves.

   Only GLM 5.3 and Qwen3.8 Flash Next carry a draft head, and it needs a GPU,
   so this runs on the Metal build alone. [DS4_SPEC_MODEL] names the model, and
   otherwise the first of those downloaded is used. *)

module V4 = Ds4.V4
module Model = Ds4_cli.Model

let check name cond failures =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let steps = 96

(* A prompt long enough that prefilling it again is plainly slower than
   prefilling one token. *)
let prompt engine ~ctx_size =
  let t = V4.Transcript.create engine in
  let filler =
    String.concat " "
      (List.init 300 (fun i -> Printf.sprintf "Line %d of the preamble." i))
  in
  V4.Transcript.append_message t ~role:"user"
    (filler
   ^ "\n\n\
      Ignore the preamble. Count from one to forty in words, separated by \
      commas.");
  V4.Transcript.append_assistant_prefix t ~think:`None ~ctx_size;
  V4.Transcript.tokens t

let greedy s = V4.Session.sample s ~temperature:0.0 ~top_p:1.0 ~min_p:0.0

(* One token a step. *)
let plain engine tokens ~ctx_size =
  let s = V4.Session.create engine ~ctx_size ~seed:1L in
  V4.Session.sync s tokens;
  let rec go acc n =
    if n >= steps then List.rev acc
    else
      let tok = greedy s in
      if V4.token_is_stop engine tok then List.rev acc
      else begin
        V4.Session.eval s tok;
        go (tok :: acc) (n + 1)
      end
  in
  let out = go [] 0 in
  V4.Session.close s;
  out

(* As many tokens a step as the draft head gets accepted. *)
let speculative engine tokens ~ctx_size =
  let s = V4.Session.create engine ~ctx_size ~seed:1L in
  V4.Session.sync s tokens;
  let drafted = ref 0 in
  let rec go acc n =
    if n >= steps then List.rev acc
    else
      let tok = greedy s in
      if V4.token_is_stop engine tok then List.rev acc
      else
        let committed =
          V4.Session.eval_speculative s tok ~max:(steps - n) ~temperature:0.0
            ~top_p:1.0 ~min_p:0.0
        in
        drafted := !drafted + Array.length committed - 1;
        let rec take acc n i =
          if i >= Array.length committed then go acc n
          else if V4.token_is_stop engine committed.(i) then List.rev acc
          else take (committed.(i) :: acc) (n + 1) (i + 1)
        in
        take acc n 0
  in
  let out = go [] 0 in
  V4.Session.close s;
  (out, !drafted)

let time f =
  let t0 = Unix.gettimeofday () in
  let v = f () in
  (v, Unix.gettimeofday () -. t0)

(* A speculative rewind must retain usable state and the matching prefix,
   so restoring its last token cannot require replaying the whole prompt. *)
let rewind engine tokens ~ctx_size ~expected failures =
  let s = V4.Session.create engine ~ctx_size ~seed:1L in
  let (), full = time (fun () -> V4.Session.sync s tokens) in
  let rec until_drafted kept =
    if List.length kept >= steps then None
    else
      let tok = greedy s in
      let committed =
        V4.Session.eval_speculative s tok ~max:4 ~temperature:0.0 ~top_p:1.0
          ~min_p:0.0
      in
      let kept = kept @ Array.to_list committed in
      if Array.length committed > 1 then Some kept else until_drafted kept
  in
  (match until_drafted [] with
  | None -> check "a speculative step committed a drafted token" false failures
  | Some kept ->
      let pos = V4.Session.pos s in
      V4.Session.rewind s (pos - 1);
      check "a rewind drops the last token"
        (V4.Session.pos s = pos - 1)
        failures;
      check "a speculative rewind preserves usable cached state"
        (V4.Session.checkpoint_valid s)
        failures;
      let restored = Array.append tokens (Array.of_list kept) in
      check "rewind retains the matching cached prefix"
        (V4.Session.common_prefix s restored = pos - 1)
        failures;
      let (), again = time (fun () -> V4.Session.sync s restored) in
      Printf.printf
        "  prompt of %d tokens: %.3fs, one token after rewind: %.3fs\n%!"
        (Array.length tokens) full again;
      let next = greedy s in
      let n = List.length kept in
      check "the rewound session continues the greedy text"
        (List.length expected > n && List.nth expected n = next)
        failures);
  V4.Session.close s;
  check "checkpoint queries refuse a closed session"
    (match V4.Session.checkpoint_valid s with
    | exception Failure _ -> true
    | _ -> false)
    failures

let run env xdg model_path =
  let failures = ref 0 in
  Eio.Switch.run @@ fun _sw ->
  let fs = Eio.Stdenv.fs env in
  let engine =
    V4.create ~mtp:true ~cache:(Xdge.cache_dir xdg)
      ~model:Eio.Path.(fs / model_path)
      ()
  in
  let ctx_size = 8192 in
  check "the model's draft head is armed" (V4.draft_tokens engine > 0) failures;
  let tokens = prompt engine ~ctx_size in
  let expected = plain engine tokens ~ctx_size in
  let got, drafted = speculative engine tokens ~ctx_size in
  Printf.printf "  %d tokens, %d of them drafted\n%!" (List.length got) drafted;
  check "greedy decoding commits the same tokens with the draft head"
    (got = expected) failures;
  check "the draft head proposed tokens the model accepted" (drafted > 0)
    failures;
  rewind engine tokens ~ctx_size ~expected failures;
  if !failures = 0 then print_string "\nLive speculative test passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end

let model xdg =
  match Sys.getenv_opt "DS4_SPEC_MODEL" with
  | Some m -> Some m
  | None ->
      let dir = Model.dir xdg in
      List.find_map
        (fun name ->
          match Model.find name with
          | Some ({ files = [ f ]; _ } as m) when Model.present ~dir m ->
              Some (Filename.concat dir f)
          | _ -> None)
        [ "qwen38-q2"; "qwen38-q4"; "glm53-q2"; "glm53-q4" ]

let () =
  match Sys.getenv_opt "DS4_LIVE" with
  | None ->
      print_string "SKIP - live speculative test (set DS4_LIVE=1 to run)\n"
  | Some _ -> (
      Eio_main.run @@ fun env ->
      let xdg = Xdge.create (Eio.Stdenv.fs env) "ds4" in
      match model xdg with
      | None ->
          failwith
            "no GLM 5.3 or Qwen3.8 model is downloaded; set DS4_SPEC_MODEL"
      | Some path -> run env xdg path)
