(** Cambridge Core identifier resolution and PDF downloads. *)
type t
val origin : string
val identifier_url : doi:Taposaur.Doi.t -> string
val content_url : id:string -> string
val id_of_url : string -> string option
(** Accepts canonical Cambridge journal article URLs only. *)
val client : _ Fetch.t -> t
val locate : sw:Eio.Switch.t -> t -> Taposaur.Doi.t -> Fetch.response
val pdf : sw:Eio.Switch.t -> t -> string -> Fetch.response
