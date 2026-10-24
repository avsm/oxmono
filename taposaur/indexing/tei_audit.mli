(** Read-only validation and regeneration evidence for listed TEI files. *)

type t = {
  uid : string; file : string; converter : string option;
  status : string; detail : string; sha256 : string;
  observed_bytes : int; source_pdf : string option; pdf_status : string;
  source_stamp : string; source_changed : bool; action : string;
}

val paper : Taposaur_archive.t -> Taposaur.Uid.t -> t list
(** [paper archive uid] validates all TEI artifacts, recognises GROBID error
    text and checks the recorded PDF input's signature. Each TEI is bounded
    to 16 MiB. PDF signature checks are not full PDF validation. Source stamps
    and TEI digests allow later regeneration to recheck its preconditions.
    No source file or catalogue row is modified. Missing manifests raise. *)

val to_json : t -> string
(** [to_json report] encodes one report as a JSON object. *)
