type reader
(** Private bearer connections, with separate configuration namespaces. *)

type writer

val read_configuration : Tool_config.t
val write_configuration : Tool_config.t

val initialize_reader :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  Jsont.json ->
  reader

val initialize_writer :
  sw:Eio.Switch.t ->
  fetch:_ Fetch.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  Jsont.json ->
  writer

val reader : reader -> Email_client.reader

val writer : writer -> Email_client.writer
(** [reader source] and [writer source] lazily connect with their own token. No
    filesystem or general network capability is exposed to the model. *)
