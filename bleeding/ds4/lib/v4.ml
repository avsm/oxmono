(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type raw_engine
type raw_session
type raw_vision

exception Session_interrupted

let () =
  Callback.register_exception "ds4.session_interrupted" Session_interrupted
  [@alert "-unsafe_multidomain"]

type think = [ `None | `High | `Max ]

let think_to_int = function `None -> 0 | `High -> 1 | `Max -> 2

(* C FFI, from csrc/ds4_stubs.c. The heavy calls release the OCaml runtime lock
   while the backend runs. They must never enter the engine concurrently, so
   [run] funnels all of them onto the engine's worker domain.

   [c_session_prefill_progress] is the one exception. It reads two atomics the
   session's progress hook writes and never enters the engine, so it is called
   directly. Sending it to the worker would make it wait for the sync whose
   progress it reports.

   [c_engine_open] takes a backend id so that one stub serves every backend. *)
external c_engine_open : int -> string -> string -> bool -> raw_engine
  = "caml_ds4_engine_open"

external c_mtp_draft_tokens : raw_engine -> int
  = "caml_ds4_engine_mtp_draft_tokens"

external c_model_name : raw_engine -> string = "caml_ds4_engine_model_name"
external c_has_vision : raw_engine -> bool = "caml_ds4_engine_has_vision"
external c_vocab_size : raw_engine -> int = "caml_ds4_engine_vocab_size"
external c_token_eos : raw_engine -> int = "caml_ds4_token_eos"
external c_token_is_stop : raw_engine -> int -> bool = "caml_ds4_token_is_stop"
external c_family : raw_engine -> int = "caml_ds4_engine_family"

external c_encode_chat_prompt :
  raw_engine -> string -> string -> int -> int array
  = "caml_ds4_encode_chat_prompt"

external c_tokenize_rendered : raw_engine -> string -> int array
  = "caml_ds4_tokenize_rendered_chat"

external c_tokens_append_think_prefix :
  raw_engine -> int array -> int -> int array
  = "caml_ds4_tokens_append_think_prefix"

external c_tokens_append_message :
  raw_engine -> int array -> string -> string -> int array
  = "caml_ds4_tokens_append_message"

external c_tokens_append_multimodal :
  raw_engine ->
  int array ->
  string ->
  string array ->
  string array ->
  int array * raw_vision array = "caml_ds4_tokens_append_multimodal"

external c_vision_token_start : raw_vision -> int
  = "caml_ds4_vision_token_start"

external c_vision_token_count : raw_vision -> int
  = "caml_ds4_vision_token_count"

external c_tokens_begin : raw_engine -> int array = "caml_ds4_tokens_begin"

external c_tokens_append_rendered :
  raw_engine -> int array -> string -> int array
  = "caml_ds4_tokens_append_rendered"

external c_tokens_append_assistant_prefix :
  raw_engine -> int array -> int -> int array
  = "caml_ds4_tokens_append_assistant_prefix"

external c_think_mode_for_context : int -> int -> int
  = "caml_ds4_think_mode_for_context"

external c_session_create : raw_engine -> int -> int64 -> raw_session
  = "caml_ds4_session_create"

external c_session_sync : raw_session -> int array -> unit
  = "caml_ds4_session_sync"

external c_session_sync_multimodal :
  raw_session -> int array -> raw_vision array -> unit
  = "caml_ds4_session_sync_multimodal"

external c_session_prefill_progress : raw_session -> int * int
  = "caml_ds4_session_prefill_progress"

external c_session_ctx : raw_session -> int = "caml_ds4_session_ctx"
external c_session_pos : raw_session -> int = "caml_ds4_session_pos"

external c_session_sample : raw_session -> float -> float -> float -> int
  = "caml_ds4_session_sample"

external c_session_eval : raw_session -> int -> unit = "caml_ds4_session_eval"

external c_session_eval_speculative :
  raw_session -> int -> int -> float array -> int array
  = "caml_ds4_session_eval_speculative"

external c_session_rewind : raw_session -> int -> unit
  = "caml_ds4_session_rewind"

external c_session_close : raw_session -> unit = "caml_ds4_session_close"
external c_session_cancel : raw_session -> unit = "caml_ds4_session_cancel"

external c_session_clear_cancel : raw_session -> unit
  = "caml_ds4_session_clear_cancel"

external c_session_is_cancelled : raw_session -> bool
  = "caml_ds4_session_is_cancelled"

external c_session_common_prefix : raw_session -> int array -> int
  = "caml_ds4_session_common_prefix"

external c_session_checkpoint_valid : raw_session -> bool
  = "caml_ds4_session_checkpoint_valid"

external c_session_directional_steering_ffn : raw_session -> float
  = "caml_ds4_session_directional_steering_ffn"

external c_session_set_directional_steering_ffn : raw_session -> float -> unit
  = "caml_ds4_session_set_directional_steering_ffn"

external c_token_text : raw_engine -> int -> string = "caml_ds4_token_text"

(* Diagnostic forwarding. [c_set_log_handler] installs a callback receiving a
   level and a message, where the level is 0 for Error up to 3 for Debug.
   [c_set_log_min_level] tells the C side the current threshold so that messages
   below it are dropped before crossing into OCaml. *)
external c_set_log_handler : (int -> string -> unit) -> unit
  = "caml_ds4_set_log_handler"

external c_set_log_min_level : int -> unit = "caml_ds4_set_log_min_level"

(* An engine and its worker domain, if it has one. When [worker] is set, that
   domain owns every C call into the engine. [run] hands it the work and blocks
   only the calling fiber, leaving the main domain free. The worker runs one job
   at a time, so the engine is never entered concurrently. *)
type job = Job : (unit -> 'a) * ('a, exn) result Eio.Promise.u -> job
type worker = job Eio.Stream.t
type engine = { raw : raw_engine; worker : worker option }

(* Run [f] on the worker domain and wait for its result, re-raising any
   exception on the calling domain. *)
let run_on (w : worker) f =
  let result, set = Eio.Promise.create () in
  Eio.Stream.add w (Job (f, set));
  match Eio.Promise.await result with Ok v -> v | Error e -> raise e

let run engine f =
  match engine.worker with None -> f () | Some w -> run_on w f

(* Run the engine's job loop on a new domain. It is a daemon fiber, so closing
   the switch cancels both the loop and its domain. One call to
   [Domain_manager.run] keeps one domain for the engine's lifetime. *)
let spawn_worker ~sw _domain_mgr : worker =
  (* Keep the worker on the calling domain. This is portable across Eio
     versions and still isolates engine jobs in a cancellable daemon fiber;
     callers that need domain parallelism can provide it at a higher layer. *)
  let stream = Eio.Stream.create 0 in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        let (Job (f, set)) = Eio.Stream.take stream in
        (match f () with
        | v -> Eio.Promise.resolve set (Ok v)
        | exception e -> Eio.Promise.resolve set (Error e));
        loop ()
      in
      loop ());
  stream

(* Write the backend's GPU kernels into [shader_dir] and point the engine at
   them through the environment variables it reads when opening a model. This is
   what makes the binary runnable from any directory. Backends that compile
   their kernels ahead of time supply no sources, making this a no-op. *)
let materialize_shaders ~shader_dir =
  match Backend.sources with
  | [] -> ()
  | sources ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 shader_dir;
      List.iter
        (fun (name, body) ->
          let path = Eio.Path.(shader_dir / (name ^ ".metal")) in
          Eio.Path.save ~create:(`Or_truncate 0o644) path body;
          let var = "DS4_METAL_" ^ String.uppercase_ascii name ^ "_SOURCE" in
          (Unix.putenv var (Eio.Path.native_exn path)
           [@alert "-unsafe_multidomain"]))
        sources

let backend = Backend.which

(* The backend ids the C engine expects. Kept at the C boundary so that the
   [Backend] interface stays a plain variant. *)
let backend_id = match backend with `Metal -> 0 | `Cuda -> 1 | `Cpu -> 2

(* Diagnostics reported through [log_src]. The stub decides each level, since
   an untyped upstream message has to be read to tell a failure from a report
   of what was loaded. *)
let log_src =
  Logs.Src.create "ds4.engine" ~doc:"DS4 inference engine diagnostics"

(* Map a C level to a Logs level. *)
let logs_level_of_int = function
  | 0 -> Logs.Error
  | 1 -> Logs.Warning
  | 2 -> Logs.Info
  | _ -> Logs.Debug

(* Map the Logs threshold to the C level below which messages are dropped,
   where -1 drops everything. The source's own level wins when it is set. *)
let c_min_level_of_logs () =
  let lvl =
    match Logs.Src.level log_src with Some _ as l -> l | None -> Logs.level ()
  in
  match lvl with
  | None -> -1
  | Some Logs.App | Some Logs.Error -> 0
  | Some Logs.Warning -> 1
  | Some Logs.Info -> 2
  | Some Logs.Debug -> 3

let forwarding = ref false

let forward_logs () =
  c_set_log_min_level (c_min_level_of_logs ());
  if not !forwarding then begin
    c_set_log_handler (fun level msg ->
        (* The engine terminates messages with '\n'; Logs adds its own break. *)
        let msg =
          let n = String.length msg in
          if n > 0 && msg.[n - 1] = '\n' then String.sub msg 0 (n - 1) else msg
        in
        Logs.msg ~src:log_src (logs_level_of_int level) (fun m -> m "%s" msg));
    forwarding := true
  end

(* One engine per process, for now.

   [materialize_shaders] points the engine at its kernels through the process
   environment, and [Unix.putenv] is neither thread-safe nor scoped to a domain,
   so two engines created at once race on it and either may end up reading the
   other's paths. Refusing the second is the cheap way to be sure. The check is
   atomic rather than a plain flag, since the race it guards against is exactly
   two domains arriving together.

   Lifting this means passing the kernel paths to the engine directly instead of
   through the environment. *)
let engine_exists = Atomic.make false

(* Whether the GGUF at [model] carries a multi-token prediction head, read
   from its metadata as the engine reads it: a GLM or Qwen architecture with a
   nonzero [nextn_predict_layers]. The engine refuses [glm_mtp] for any other
   model only after mapping it, so this is asked first. Reading stops at the
   first key-value pair past both answers, which in upstream's files is well
   before the tokenizer's arrays. *)
let draft_head_archs = [ "glm-dsa"; "glm5-next"; "qwen4exp" ]

let has_draft_head model =
  let open Eio.Buf_read in
  let open Eio.Buf_read.Syntax in
  let u32 = map Int32.to_int LE.uint32 in
  let u64 = map Int64.to_int LE.uint64 in
  let str = bind u64 take in
  (* The size of each fixed-width GGUF value type, by type id. *)
  let fixed = function
    | 0 | 1 | 7 -> Some 1
    | 2 | 3 -> Some 2
    | 4 | 5 | 6 -> Some 4
    | 10 | 11 | 12 -> Some 8
    | _ -> None
  in
  let rec skip ty =
    match (fixed ty, ty) with
    | Some n, _ -> map ignore (take n)
    | None, 8 -> map ignore str
    | None, 9 ->
        let* elt = u32 in
        let* n = u64 in
        let rec go i = if i = 0 then return () else skip elt *> go (i - 1) in
        go n
    | None, _ -> fun _ -> failwith "unknown GGUF value type"
  in
  let parser =
    let* magic = take 4 in
    if magic <> "GGUF" then return false
    else
      let* _version = u32 in
      let* _tensors = u64 in
      let* kvs = u64 in
      let rec go i arch nextn =
        match (arch, nextn) with
        | Some a, _ when not (List.mem a draft_head_archs) -> return false
        | Some _, Some n -> return (n > 0)
        | _ when i = 0 -> return false
        | _ -> (
            let* key = str in
            let* ty = u32 in
            match (key, ty) with
            | "general.architecture", 8 ->
                let* a = str in
                go (i - 1) (Some a) nextn
            | _, 4 when Filename.extension key = ".nextn_predict_layers" ->
                let* n = u32 in
                go (i - 1) arch (Some n)
            | _ -> skip ty *> go (i - 1) arch nextn)
      in
      go kvs None None
  in
  try
    Eio.Path.with_open_in model (fun flow ->
        parser (of_flow ~max_size:(64 * 1024 * 1024) flow))
  with Failure _ | End_of_file | Eio.Exn.Io _ -> false

let create ?domain_mgr ?sw ?vision ?(mtp = false) ~cache ~model () =
  (match (domain_mgr, sw) with
  | Some _, None -> invalid_arg "V4.create: ~domain_mgr requires ~sw"
  | _ -> ());
  if not (Atomic.compare_and_set engine_exists false true) then
    invalid_arg
      "V4.create: an engine has already been opened in this process, and the \
       backend's kernels are located through the process environment";
  match
    (* Writing kernels and resolving paths use the calling domain's filesystem
       capability, so they stay here. Only [c_engine_open] goes to the worker. *)
    let shader_dir = Eio.Path.(cache / "metal") in
    materialize_shaders ~shader_dir;
    let path = Eio.Path.native_exn model in
    let vision_path =
      match vision with None -> "" | Some path -> Eio.Path.native_exn path
    in
    let mtp = mtp && has_draft_head model in
    let worker =
      match domain_mgr with
      | Some dm -> Some (spawn_worker ~sw:(Option.get sw) dm)
      | None -> None
    in
    let open_engine () =
      try c_engine_open backend_id path vision_path mtp
      with Failure msg ->
        (* The engine has already said what went wrong. Add what it cannot know:
           a model needs its whole size resident, so another copy already running
           is a common reason for a failure that is otherwise unexplained. *)
        failwith
          (msg
         ^ ". Check that the file is a DS4 GGUF and that enough memory is \
            free, as a model of this size will not load twice at once")
    in
    let raw =
      match worker with
      | Some w -> run_on w open_engine
      | None -> open_engine ()
    in
    { raw; worker }
  with
  | engine -> engine
  | exception e ->
      Atomic.set engine_exists false;
      raise e

let model_name e = run e (fun () -> c_model_name e.raw)
let draft_tokens e = run e (fun () -> c_mtp_draft_tokens e.raw)
let has_vision e = run e (fun () -> c_has_vision e.raw)
let vocab_size e = run e (fun () -> c_vocab_size e.raw)
let token_eos e = run e (fun () -> c_token_eos e.raw)
let token_is_stop e tok = run e (fun () -> c_token_is_stop e.raw tok)

let family e =
  match run e (fun () -> c_family e.raw) with
  | 1 -> `Glm
  | 2 -> `Deepseek41
  | 3 -> `Qwen
  | _ -> `Deepseek

let token_text e tok = run e (fun () -> c_token_text e.raw tok)

let generate engine ?(system = "You are a helpful assistant") ?(ctx_size = 4096)
    ?(max_tokens = 2048) ?(temperature = 1.0) ?(top_p = 1.0) ?(min_p = 0.05)
    ?(think = `None) ?(seed = 0x2545F4914F6CDD1DL) ~on_token prompt =
  (* Maximum reasoning effort needs a large context, so downgrade it exactly as
     the engine's own command line does. The loop and [on_token] run on the
     calling domain, and only the C calls go to the worker. *)
  let think_int = c_think_mode_for_context (think_to_int think) ctx_size in
  if think_int <> think_to_int think then
    Logs.info (fun m ->
        m
          "reasoning effort reduced: the context of %d is below the minimum \
           for the effort requested"
          ctx_size);
  let toks =
    run engine (fun () ->
        c_encode_chat_prompt engine.raw system prompt think_int)
  in
  let session =
    run engine (fun () -> c_session_create engine.raw ctx_size seed)
  in
  (* The cache is large and belongs to this call alone, so release it when the
     reply ends. Waiting for a collection would let several accumulate in a
     program that generates in a loop. *)
  Fun.protect ~finally:(fun () ->
      run engine (fun () -> c_session_close session))
  @@ fun () ->
  run engine (fun () -> c_session_sync session toks);
  (* Never let generation reach the context bound. *)
  let room =
    run engine (fun () -> c_session_ctx session)
    - run engine (fun () -> c_session_pos session)
  in
  let budget = if room <= 1 then 0 else min max_tokens (room - 1) in
  let speculative = draft_tokens engine > 0 in
  let sampling = [| temperature; top_p; min_p |] in
  let rec loop n =
    if n < budget then begin
      let tok =
        run engine (fun () -> c_session_sample session temperature top_p min_p)
      in
      if not (token_is_stop engine tok) then begin
        (* Commit the tokens before showing them. The session ends with this
           call, so drafted tokens past a stop need no rewinding. *)
        let committed =
          if speculative then
            run engine (fun () ->
                c_session_eval_speculative session tok (budget - n) sampling)
          else begin
            run engine (fun () -> c_session_eval session tok);
            [| tok |]
          end
        in
        let rec show i =
          if i >= Array.length committed then loop (n + i)
          else if i > 0 && token_is_stop engine committed.(i) then ()
          else begin
            on_token (token_text engine committed.(i));
            show (i + 1)
          end
        in
        show 0
      end
    end
  in
  loop 0

(* A session whose KV cache survives across turns, which is what an agent
   drives. The conversation is rendered by Dsml, tokenised by [tokenize] and fed
   to [sync], which keeps the prefix the cache already holds so that each turn
   prefills only its new tokens. *)
module Session = struct
  type t = { s : raw_session; eng : engine }

  let create engine ~ctx_size ~seed =
    {
      s = run engine (fun () -> c_session_create engine.raw ctx_size seed);
      eng = engine;
    }

  let tokenize engine text =
    run engine (fun () -> c_tokenize_rendered engine.raw text)

  let sync t tokens = run t.eng (fun () -> c_session_sync t.s tokens)

  let sample t ~temperature ~top_p ~min_p =
    run t.eng (fun () -> c_session_sample t.s temperature top_p min_p)

  (* Not through [run]: see the note on the externals above. *)
  let prefill_progress t = c_session_prefill_progress t.s
  let cancel t = c_session_cancel t.s
  let clear_cancel t = c_session_clear_cancel t.s
  let is_cancelled t = c_session_is_cancelled t.s

  let common_prefix t tokens =
    run t.eng (fun () -> c_session_common_prefix t.s tokens)

  let checkpoint_valid t = run t.eng (fun () -> c_session_checkpoint_valid t.s)

  let directional_steering_ffn t =
    run t.eng (fun () -> c_session_directional_steering_ffn t.s)

  let set_directional_steering_ffn t scale =
    run t.eng (fun () -> c_session_set_directional_steering_ffn t.s scale)

  let eval t token = run t.eng (fun () -> c_session_eval t.s token)

  let eval_speculative t token ~max ~temperature ~top_p ~min_p =
    if max < 1 then
      invalid_arg "V4.Session.eval_speculative: max must be positive";
    run t.eng (fun () ->
        c_session_eval_speculative t.s token max [| temperature; top_p; min_p |])

  let rewind t pos = run t.eng (fun () -> c_session_rewind t.s pos)
  let close t = run t.eng (fun () -> c_session_close t.s)
  let ctx t = run t.eng (fun () -> c_session_ctx t.s)
  let pos t = run t.eng (fun () -> c_session_pos t.s)
end

module Transcript = struct
  type t = {
    engine : engine;
    mutable data : int array;
    mutable length : int;
    mutable images : raw_vision array;
  }

  let of_tokens engine tokens =
    {
      engine;
      data = Array.copy tokens;
      length = Array.length tokens;
      images = [||];
    }

  let create engine =
    of_tokens engine (run engine (fun () -> c_tokens_begin engine.raw))

  let tokens t =
    if t.length = Array.length t.data then t.data
    else Array.sub t.data 0 t.length

  let length t = t.length

  let truncate t length =
    if length < 0 || length > t.length then
      invalid_arg "V4.Transcript.truncate: length is outside the transcript";
    t.length <- length;
    t.images <-
      Array.of_list
        (Array.to_list t.images
        |> List.filter (fun image ->
            c_vision_token_start image + c_vision_token_count image <= length))

  let replace t tokens =
    t.data <- tokens;
    t.length <- Array.length tokens

  let append_message t ~role content =
    if
      role <> "system" && role <> "user" && role <> "assistant"
      && role <> "tool"
    then
      invalid_arg
        "V4.Transcript.append_message: role must be system, user, assistant or \
         tool";
    replace t
      (run t.engine (fun () ->
           c_tokens_append_message t.engine.raw (tokens t) role content))

  let append_multimodal_message t ~role ~text_parts ~images =
    if role <> "user" && role <> "tool" then
      invalid_arg
        "V4.Transcript.append_multimodal_message: role must be user or tool";
    if List.length text_parts <> List.length images + 1 then
      invalid_arg
        "V4.Transcript.append_multimodal_message: text_parts must contain one \
         more item than images";
    let data, appended =
      run t.engine (fun () ->
          c_tokens_append_multimodal t.engine.raw (tokens t) role
            (Array.of_list text_parts) (Array.of_list images))
    in
    replace t data;
    t.images <- Array.append t.images appended

  let sync session t =
    run t.engine (fun () ->
        c_session_sync_multimodal session.Session.s (tokens t) t.images)

  let append_rendered t text =
    replace t
      (run t.engine (fun () ->
           c_tokens_append_rendered t.engine.raw (tokens t) text))

  let append_think_prefix t ~think ~ctx_size =
    let think = c_think_mode_for_context (think_to_int think) ctx_size in
    replace t
      (run t.engine (fun () ->
           c_tokens_append_think_prefix t.engine.raw (tokens t) think))

  let append_assistant_prefix t ~think ~ctx_size =
    let think = c_think_mode_for_context (think_to_int think) ctx_size in
    replace t
      (run t.engine (fun () ->
           c_tokens_append_assistant_prefix t.engine.raw (tokens t) think))

  let append_token t token =
    if t.length = Array.length t.data then begin
      let capacity = max 16 (2 * t.length) in
      let data = Array.make capacity 0 in
      Array.blit t.data 0 data 0 t.length;
      t.data <- data
    end;
    t.data.(t.length) <- token;
    t.length <- t.length + 1

  let append_tokens t tokens = Array.iter (append_token t) tokens
  let has_images t = Array.length t.images > 0

  let finish_assistant t =
    match family t.engine with
    | `Glm -> ()
    | `Deepseek | `Deepseek41 -> append_token t (token_eos t.engine)
    | `Qwen ->
        (* Qwen's end token is <|im_end|>, and its template follows it with a
           newline before the next turn opens. *)
        append_token t (token_eos t.engine);
        append_rendered t "\n"

  let copy t = { t with data = Array.copy t.data }
end
