(** Manifest-listed HTTP artifact responses. *)
val send :
  Taposaur_archive.paper -> Taposaur.Manifest.Artifact.t ->
  Proffer.Req.t @ local -> Proffer.Resp.respond @ local -> unit
(** [send paper artifact request respond] streams the file with HEAD and
    single byte-range support, checking that it has not changed since stat. *)
