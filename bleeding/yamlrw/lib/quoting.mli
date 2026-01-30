(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML scalar quoting detection *)

val needs_quoting : string -> bool
(** Check if a string value needs quoting in YAML output. Returns true if the
    string:
    - Is empty
    - Starts with an indicator character
    - Is a reserved word (null, true, false, yes, no, etc.)
    - Contains characters that would be ambiguous
    - Looks like a number *)

val needs_double_quotes : string -> bool
(** Check if a string requires double quotes (vs single quotes). Returns true if
    the string contains characters that need escape sequences. *)

val choose_style : string -> [> `Plain | `Single_quoted | `Double_quoted ]
(** Choose the appropriate quoting style for a string value *)
