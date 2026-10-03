(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared theme resolution for bordered widgets. *)

val border :
  ?theme:Theme.t -> ?border:Border.t -> default:Border.t -> unit -> Border.t
(** [border] resolves an explicit border, then a theme border, then [default].
*)

val animated : Theme.t option -> bool
(** [animated theme] reports whether the selected theme animates widgets. *)
