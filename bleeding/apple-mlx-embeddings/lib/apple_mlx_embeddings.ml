module Model = Model
module Vision = Vision

type raw
type t = { raw : raw; lock : Eio.Mutex.t }

type vectors =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

let bridge_version = "mlx-swift-lm-3.32.3/qwen3-v1"

type memory_stats = {
  active_bytes : int;
  cache_bytes : int;
  peak_bytes : int;
  cache_limit_bytes : int;
}

external raw_memory_stats : unit -> memory_stats = "caml_ame_memory_stats"

let memory_stats () =
  Eio_unix.run_in_systhread ~label:"mlx.memory_stats" raw_memory_stats

external available : unit -> bool = "caml_ame_available"
external raw_load : string -> int -> raw = "caml_ame_load"
external raw_embed : raw -> string array -> vectors -> unit = "caml_ame_embed"
external raw_close : raw -> unit = "caml_ame_close"

let string label value =
  if String.contains value '\000' then
    invalid_arg (label ^ " contains a NUL byte")

let load ~model_dir ~max_tokens =
  if model_dir = "" then invalid_arg "Empty MLX model directory";
  string "MLX model directory" model_dir;
  if max_tokens < 32 || max_tokens > 32768 then
    invalid_arg "MLX max_tokens must be between 32 and 32768";
  let raw =
    Eio_unix.run_in_systhread ~label:"mlx.load" (fun () ->
        raw_load model_dir max_tokens)
  in
  { raw; lock = Eio.Mutex.create () }

let embed model ~dimensions inputs =
  let inputs = Array.copy inputs in
  let count = Array.length inputs in
  if count < 1 || count > 16 then
    invalid_arg "MLX embedding batches contain 1 to 16 inputs";
  if dimensions < 32 || dimensions > 4096 then
    invalid_arg "MLX embedding dimensions must be between 32 and 4096";
  Array.iter
    (fun input ->
      string "MLX embedding input" input;
      if String.length input > 65536 then
        invalid_arg "MLX embedding input exceeds 65536 bytes")
    inputs;
  Eio.Mutex.use_ro model.lock (fun () ->
      let output =
        Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout count
          dimensions
      in
      Eio_unix.run_in_systhread ~label:"mlx.embed" (fun () ->
          raw_embed model.raw inputs output);
      Eio.Fiber.check ();
      output)

let close model =
  Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_ro model.lock (fun () ->
          Eio_unix.run_in_systhread ~label:"mlx.close" (fun () ->
              raw_close model.raw)))
