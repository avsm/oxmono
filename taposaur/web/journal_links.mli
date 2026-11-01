(** Local OpenAlex source links, loaded independently of the paper catalogue. *)
type t
val empty : t
val of_jsonl : string -> (t, string) result
(** [of_jsonl data] reads [source_id] with [journal], [doi], or both.
    Conflicting source IDs for a name are left unlinked. *)
val find : t -> string -> string option
(** [find links journal] returns an OpenAlex source URL for an unambiguous
    name, ignoring ASCII case and repeated whitespace. *)
val source : t -> string -> string option
(** [source links doi] returns the unambiguous OpenAlex source for a DOI. *)
val known_doi : t -> string -> bool
(** [known_doi links doi] checks DOI identity even if its source is ambiguous. *)
