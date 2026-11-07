(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Merging the GGUF split files of a DeepSeek V4.1 model into the one
    page-aligned file the engine loads.

    Hugging Face publishes the larger V4.1 builds as standard GGUF split files,
    each with its own header. The engine reads the Engram tables of a V4.1 model
    straight from disk and needs one file whose tensors are aligned to 16 KiB,
    which is how the model was built before it was split. *)

val merge :
  ?progress:(written:int -> total:int -> unit) ->
  out:string ->
  expected_bytes:int ->
  sha256:string ->
  string list ->
  unit
(** [merge ~out ~expected_bytes ~sha256 shards] writes the model held by the
    split files [shards], given in order, to [out]. Every tensor and metadata
    entry is kept, the split metadata is dropped and the tensor data is
    realigned to 16 KiB, so the result is the original file, whose length is
    [expected_bytes] and whose SHA-256 is [sha256] in lowercase hexadecimal. The
    shards are left in place.

    The output is assembled at [out ^ ".assembling"] and published only after it
    verifies, so a merge that is interrupted resumes where it stopped when run
    again with the same shards. The file is never truncated to fit: a partial
    assembly that differs from these shards is refused. [progress] is called
    with the bytes written so far as the merge proceeds.

    It raises [Failure] when the shards are not a complete DeepSeek V4.1 split
    set in order, when a partial assembly was not written from them, or when the
    result does not verify. The partial file is then left for inspection and
    must be moved aside before trying again. *)

val verify : path:string -> expected_bytes:int -> sha256:string -> unit
(** [verify ~path ~expected_bytes ~sha256] raises [Failure] unless the file at
    [path] is [expected_bytes] long and has the SHA-256 digest [sha256], given
    in lowercase hexadecimal. *)
