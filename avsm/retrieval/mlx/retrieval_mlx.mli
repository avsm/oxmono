module Runtime : sig
  type t

  val create : sw:Eio.Switch.t -> t
  (** [create ~sw] owns the lazily loaded text and vision models until [sw]
      finishes. Providers cannot be used after the switch closes. *)

  val release : t -> unit
  (** [release t] releases retained weights after active inference completes.
      Existing providers reload on their next request. Cleanup also waits in a
      cancelled context. *)
end

val create :
  runtime:Runtime.t ->
  model_dir:string ->
  dimensions:int ->
  max_tokens:int ->
  query_instruction:string ->
  check:(unit -> unit) ->
  Retrieval.Embedding.provider
(** [create ~runtime ~model_dir ~dimensions ~max_tokens ~query_instruction ~check]
    fingerprints local model files without loading GPU weights. The identity
    includes their contents, tokenizer files, inference settings and bridge
    version. Documents are embedded unchanged. Queries use Qwen3's instruction
    prefix. An empty instruction disables the query prefix. Instructions are
    limited to 8192 bytes and must not contain NUL. Batches contain at most four
    inputs.

    The runtime retains one lazily loaded model, serializing inference and
    replacing it when the model identity changes. File changes between provider
    creation and inference reject the request. Recreate the provider to use the
    new revision. File hashing runs outside the Eio domain. *)

module Vision : sig
  val create :
    runtime:Runtime.t ->
    model_dir:string ->
    dimensions:int ->
    max_tokens:int ->
    query_instruction:string ->
    check:(unit -> unit) ->
    Retrieval.Embedding.provider
  (** [create ...] makes an independent Qwen3-VL provider. Queries are text.
      Document inputs are JSON arrays containing the local image path and its
      SHA256 hex digest. Pixel content is checked before and after inference.
      The model loads lazily, independently of the text provider. Batches have
      at most two inputs. Its identity includes preprocessing bridge version,
      model contents, instruction and dimensions. *)
end
