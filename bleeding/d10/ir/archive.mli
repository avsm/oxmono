(** A pre-resolved source archive supplied by the recipe producer.

    The archive expands to the package's source tree with all extra sources
    placed, all patches applied, and all opam substitutions performed. The d10ir
    executor unpacks it; it does not fetch, patch, or substitute. *)

type t = {
  path : string;
      (** Path to the archive, relative to {!Plan.t.archive_root}. *)
  sha256 : string;  (** Verified before unpack. *)
  strip_components : int;  (** Passed to [tar --strip-components]. Default 1. *)
}

val pp : t Fmt.t
(** [pp] renders [path] and the short sha for log lines. *)
