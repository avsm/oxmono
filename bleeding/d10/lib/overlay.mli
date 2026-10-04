(** Optional source attribution for layer metadata and index queries.

    The handle identifies an overlay and the version identifies its snapshot.
    Neither field requires a registry. Callers use [t option] for untagged
    packages. *)

type t = { handle : string; version : string }

val pp : t Fmt.t
(** [pp ppf t] prints [t] as [handle@version]. *)

(** {1 Codec} *)

val codec : t Jsont.t
(** JSON codec ([{"handle": ..., "version": ...}]). *)
