type raw
type t = { raw : raw; lock : Eio.Mutex.t }

type vectors =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

let bridge_version = "mlx-swift-lm-3.32.3/qwen3-vl-v1"

external raw_load : string -> int -> raw = "caml_ame_vision_load"

external raw_embed : raw -> bool -> string -> string array -> vectors -> unit
  = "caml_ame_vision_embed"

external raw_close : raw -> unit = "caml_ame_vision_close"

let string label value =
  if String.contains value '\000' then
    invalid_arg (label ^ " contains a NUL byte")

let load ~model_dir ~max_tokens =
  if model_dir = "" then invalid_arg "Empty MLX vision model directory";
  string "MLX vision model directory" model_dir;
  if max_tokens < 32 || max_tokens > 32768 then
    invalid_arg "MLX max_tokens must be between 32 and 32768";
  let raw =
    Eio_unix.run_in_systhread ~label:"mlx.vision.load" (fun () ->
        raw_load model_dir max_tokens)
  in
  { raw; lock = Eio.Mutex.create () }

let embed images model ~instruction ~dimensions inputs =
  let inputs = Array.copy inputs in
  let count = Array.length inputs in
  if count < 1 || count > 2 then
    invalid_arg "MLX vision embedding batches contain 1 to 2 inputs";
  if dimensions < 64 || dimensions > 4096 then
    invalid_arg "MLX vision embedding dimensions must be between 64 and 4096";
  string "MLX vision instruction" instruction;
  if String.length instruction > 8192 then
    invalid_arg "MLX vision instruction exceeds 8192 bytes";
  Array.iter
    (fun input ->
      string "MLX vision input" input;
      if String.length input > 65536 then
        invalid_arg "MLX vision input exceeds 65536 bytes";
      if images && input = "" then invalid_arg "Empty MLX image path")
    inputs;
  Eio.Mutex.use_ro model.lock (fun () ->
      let output =
        Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout count
          dimensions
      in
      Eio_unix.run_in_systhread ~label:"mlx.vision.embed" (fun () ->
          raw_embed model.raw images instruction inputs output);
      Eio.Fiber.check ();
      output)

let embed_text model = embed false model
let embed_images model = embed true model

let close model =
  Eio.Cancel.protect (fun () ->
      Eio.Mutex.use_ro model.lock (fun () ->
          Eio_unix.run_in_systhread ~label:"mlx.vision.close" (fun () ->
              raw_close model.raw)))
