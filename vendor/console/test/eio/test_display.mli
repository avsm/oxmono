(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Eio driver behind {!Console_eio.run}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_display}. *)

val report_domains_suite : string * unit Alcotest.test_case list
(** Worker reporting retains display ownership and scope lifetime. *)
