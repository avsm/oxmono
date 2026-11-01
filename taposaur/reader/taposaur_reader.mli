(** Compose archive access, format readers and pure paper resolution. *)

val load :
  ?index:Taposaur.Index.t -> Taposaur_archive.paper -> Taposaur_paper.t
(** [load paper] reads every TEI, publisher XML and S2ORC JSON artifact
    listed by the manifest. XML headers are limited to 1 MiB and JSON to
    16 MiB. Failed or unsupported documents remain as errors in the
    resolved paper. The manifest is the snapshot already held by [paper].
    Cancellation propagates. An index for another UID is rejected. *)
