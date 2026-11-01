(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Entry point for the Eio property suite. *)

val suite : string * Alcobar.test_case list
(** [suite] is the Eio property suite run by this executable. *)
