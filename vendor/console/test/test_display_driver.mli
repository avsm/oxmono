(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for how a driver runs a {!Console.Display}: its scope and its [Logs]
    messages. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for the display's driver. *)
