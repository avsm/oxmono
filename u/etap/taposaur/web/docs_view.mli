(** Reference pages. Static HTML built from the documented schema. *)
val index : string
(** [index] lists the documentation pages. *)
val corpus : string
(** [corpus] documents every column of a corpus export row and its Parquet
    type, the provenance and acquisition records and the frozen labels. *)
val corpus_columns : string list
(** [corpus_columns] names the documented columns, in page order. *)
