(** Native Qwen3-VL text and image embeddings in a shared vector space. These
    vectors are separate from the text-only Qwen3 embedding space. *)

type t

type vectors =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

val bridge_version : string
(** [bridge_version] identifies the vision architecture and preprocessing. *)

val load : model_dir:string -> max_tokens:int -> t
(** [load ~model_dir ~max_tokens] loads local Qwen3-VL embedding weights,
    tokenizer, chat template and image processor configuration. [max_tokens]
    must be between 32 and 32768. This function requires an Eio scheduler. *)

val embed_text :
  t -> instruction:string -> dimensions:int -> string array -> vectors
(** [embed_text model ~instruction ~dimensions inputs] embeds one or two UTF-8
    strings in the model's multimodal space. Each input is at most 65536 bytes.
    [instruction] is at most 8192 bytes. An empty instruction selects
    [Represent the user's input.]. Text is truncated to [max_tokens].
    [dimensions] is between 64 and the model output width. The result contains
    normalized float32 vectors. This function requires an Eio scheduler. *)

val embed_images :
  t -> instruction:string -> dimensions:int -> string array -> vectors
(** [embed_images model ~instruction ~dimensions paths] embeds one or two local
    image files. Each file must be regular, at most 64 MiB, and at most 100
    million decoded pixels with neither edge exceeding 16384 pixels. Images are
    resized within 4096 to 1843200 pixels for inference. A token limit that
    cannot accommodate an image fails rather than truncating its patch tokens.
    Instruction and dimension constraints match {!embed_text}. Calls on one
    model are serialized. Cancellation waits for an active batch to finish. This
    function requires an Eio scheduler. *)

val close : t -> unit
(** [close model] releases the model after active inference finishes. Repeated
    calls have no effect. This function requires an Eio scheduler. *)
