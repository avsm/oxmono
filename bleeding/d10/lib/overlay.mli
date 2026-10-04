(** Reporepo overlay context: a (handle, version) pair identifying which overlay
    snapshot contributed an opam file or attributed an audit event.

    Cross-cutting type used by {!Layer}, {!Index}, and the [oi] modules
    {!Oi.Plan}, {!Oi.Origin}, {!Oi.Audit}. The pair is either fully present or
    absent — callers wrap in [option] at the boundaries that may not have one.
*)

type t = { handle : string; version : string }

val pp : t Fmt.t
(** [pp ppf t] prints [t] as [handle@version]. *)

(** {1 Codec} *)

val codec : t Jsont.t
(** JSON codec ([{"handle": ..., "version": ...}]). *)
