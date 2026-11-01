(** Executes review definitions using local catalogue reads or recorded inputs.
    No network or archive access. *)
module R = Taposaur_review
module S = Taposaur_review_store
type binding = { key : string; bytes : string; partial : bool }
val execute : ?max_members:int ->
  S.write S.t -> Taposaur_catalog.read Taposaur_catalog.t ->
  S.revision -> actor:string -> binding list -> int
(** [execute store catalog revision ~actor bindings] executes local DOI
    searches and reads fixed lists. Recorded searches require supplied
    results. Invalid lines require an explicitly partial binding. Exceeding
    the result budget aborts without publishing a run. *)
val snapshot : ?publishers:Taposaur.Publisher_evidence.t list -> ?max_members:int -> ?max_bytes:int -> ?capture_seconds:float ->
  S.write S.t -> Taposaur_catalog.read Taposaur_catalog.t ->
  run_id:int -> parent:int option -> label:string -> actor:string -> int
(** [snapshot store catalog ...] captures metadata in one catalogue read
    transaction for fixed run membership. It never changes that membership. *)
