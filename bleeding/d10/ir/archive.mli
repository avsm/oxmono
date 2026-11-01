(** Prepared source input supplied by a recipe producer.

    For {!Direct}, the archive contains sources with extra files and patches
    already applied. The executor verifies and unpacks it, then applies the
    node's [substs] using [subst_vars]. It does not fetch sources or patches.

    {!Makefile} instead consumes unpacked sources under [sources/<sha256>/]. Its
    source identifiers need not be archive checksums. Such exported plans cannot
    be replayed by Direct without first supplying actual archives. *)

type t = {
  path : string;
      (** Path to the archive, relative to the plan's [archive_root]. *)
  sha256 : string;
      (** SHA-256 of the archive. Direct skips verification when empty. *)
  strip_components : int;  (** Passed to [tar --strip-components]. Default 1. *)
}

val pp : t Fmt.t
(** [pp] renders [path] and the short sha for log lines. *)
