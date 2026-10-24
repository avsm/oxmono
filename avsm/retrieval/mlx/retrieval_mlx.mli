val create :
  model_dir:string ->
  dimensions:int ->
  max_tokens:int ->
  query_instruction:string ->
  check:(unit -> unit) ->
  Retrieval.Embedding.provider
(** [create ~model_dir ~dimensions ~max_tokens ~query_instruction ~check]
    fingerprints local model files without loading GPU weights. The identity
    includes their contents, tokenizer files, inference settings and bridge
    version. Documents are embedded unchanged. Queries use Qwen3's instruction
    prefix. An empty instruction disables the query prefix. Instructions are
    limited to 8192 bytes and must not contain NUL. Batches contain at most four
    inputs.

    The process retains one lazily loaded model, serializing inference and
    replacing it when the model identity changes. File changes between provider
    creation and inference reject the request. Recreate the provider to use the
    new revision. File hashing runs outside the Eio domain. *)

val release : unit -> unit
(** [release ()] closes the retained model after any active inference completes.
    Existing providers reload it on their next request. *)

module Vision : sig
  val create :
    model_dir:string -> dimensions:int -> max_tokens:int ->
    query_instruction:string -> check:(unit -> unit) ->
    Retrieval.Embedding.provider
  (** [create ...] makes an independent Qwen3-VL provider. Queries are text.
      Document inputs are JSON arrays containing the local image path and its
      SHA256 hex digest. Pixel content is checked before and after inference.
      The model loads lazily, independently of the text provider. Batches have
      at most two inputs. Its identity includes preprocessing bridge version,
      model contents, instruction and dimensions. *)

  val release : unit -> unit
  (** [release ()] closes the retained vision model after active inference. *)
end
