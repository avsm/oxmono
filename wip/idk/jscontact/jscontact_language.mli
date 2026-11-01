@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Language tags.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html} RFC 9553} types the
    [language] of a Card, the keys of its [localizations] and the [language] of
    a LanguagePref as language tags, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc5646.html} RFC 5646}.

    @canonical Jscontact.Language *)

val validate : string -> string Jscontact_valid.t
(** [validate s] is [Ok s] if [s] is a well formed language tag, that is hyphen
    separated subtags of 1 to 8 alphanumerics each. The check is syntactic. It
    does not consult the IANA subtag registry, so it accepts a well formed tag
    that names no language. *)
