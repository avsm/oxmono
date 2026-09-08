(** Replaceable media retrieval with cache integration.

    This layer deliberately sits beside {!Media}: the transport-only media
    operations do not depend on [Media_store]. *)

type t
(** A network media fetch capability. The callback returns final bytes. For an
    encrypted request it must authenticate and decrypt the downloaded ciphertext
    before returning successfully. *)

val create :
  (Client.t -> Media.request -> (string, Media.encrypted_error) result) -> t

val default : t
(** The normal authenticated/legacy [Media.download] implementation. Encrypted
    requests use {!Media.download_encrypted}. *)

val get_content :
  ?use_cache:bool ->
  ?store:Media_store.t ->
  ?fetcher:t ->
  Client.t ->
  Media.request ->
  (string, Media.encrypted_error) result
(** [get_content client request] reads local content or an optional cache before
    invoking the fetcher. Local send-queue MXCs never invoke the fetcher. A
    successful encrypted request returns authenticated plaintext. *)
