(** Resource-free text extraction from HTML mail. *)

val render : string -> string
(** [render html] preserves paragraph boundaries, image alt text and explicit
    HTTP or mailto link targets. Scripts, styles and hidden elements are
    excluded. No resources are fetched. Callers must sanitize terminal controls
    before displaying the returned source text. *)
