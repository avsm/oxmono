(** Validated, recoverable replacement of an archived GROBID conversion.
    Callers must exclude other archive writers during publication. *)
val validate : Taposaur.Uid.t -> string -> Taposaur.Bibliography.t
(** [validate uid xml] requires well-formed TEI, the exact header DOI,
    a publisher, and evidence of Glutton consolidation. *)
val publish : ?audit:Taposaur_audit.session -> root:string -> work:string -> Taposaur.Uid.t ->
  input:string -> xml:string -> unit
(** [publish ~root ~work uid ~input ~xml] preserves the old artifact,
    manifest and event log in [work], then publishes the validated TEI,
    conversion events and manifest. Repeating an interrupted publication
    completes its durable intent. Unexpected source changes are refused.
    [work] must be a dedicated, private directory for this paper. *)
