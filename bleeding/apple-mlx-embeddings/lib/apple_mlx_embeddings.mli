(** Native MLX inference for local Qwen3 embedding models on Apple silicon.
    Inference requires macOS 26 or later. Other platforms expose an unavailable
    backend. {!Model} selects and downloads supported checkpoints. *)

module Model = Model
module Vision = Vision

type t

type vectors =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

val bridge_version : string
(** [bridge_version] identifies the inference and preprocessing implementation. *)

val available : unit -> bool
(** [available ()] reports whether this build has an Apple GPU backend. *)

type memory_stats = {
  active_bytes : int;
  cache_bytes : int;
  peak_bytes : int;
  cache_limit_bytes : int;
}

val memory_stats : unit -> memory_stats
(** [memory_stats ()] samples process-wide MLX allocator usage. Active memory
    holds live tensors, cache memory holds reusable free buffers, and peak
    memory is the highest active usage since process startup. These counters
    exclude other process memory and are not an atomic snapshot. The first load
    or statistics call initializes the native runtime and caps its free-buffer
    cache at 256 MiB without raising a smaller existing limit. This is not an
    active-memory or process-memory limit. A released buffer can temporarily
    exceed the cache limit until the next allocation. This function requires an
    Eio scheduler and fails when the native backend is unavailable. *)

val load : model_dir:string -> max_tokens:int -> t
(** [load ~model_dir ~max_tokens] loads a local Qwen3 embedding model.
    [max_tokens] must be between 32 and 32768. The directory must contain model
    weights, [config.json], [tokenizer.json] and [tokenizer_config.json]. This
    function requires an Eio scheduler. *)

val embed : t -> dimensions:int -> string array -> vectors
(** [embed model ~dimensions inputs] returns one normalized float32 vector per
    input. Inputs are truncated at the model's configured token limit.
    [dimensions] must be between 32 and the model's output width. A batch
    contains 1 to 16 UTF-8 strings, each at most 65536 bytes. Callers supply any
    query instruction as part of the input. Calls on one model are serialized.
    An in-flight batch finishes before cancellation takes effect. This function
    requires an Eio scheduler. *)

val close : t -> unit
(** [close model] releases the model after any active inference finishes.
    Repeated calls have no effect. This function requires an Eio scheduler. *)
