@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ordered component lists.

    A Name and an Address each hold their parts as a list of components under
    the same rules, stated by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.1} RFC 9553
     Section 2.2.1} and
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.5.1} Section 2.5.1}.
*)

val validate :
  is_separator:('c -> bool) ->
  has_phonetic:('c -> bool) ->
  is_ordered:bool ->
  default_separator:string option ->
  has_phonetic_system:bool ->
  'c list option ->
  unit Jscontact_valid.t
(** [validate ~is_separator ~has_phonetic ~is_ordered ~default_separator
     ~has_phonetic_system components] checks the rules the two sections share.
    At least one component is not a separator. No two separators are
    consecutive. A separator and [default_separator] require [is_ordered].
    [default_separator] requires [components]. A component with a phonetic
    requires [has_phonetic_system], which says whether the holder sets a
    phonetic script or system. *)
