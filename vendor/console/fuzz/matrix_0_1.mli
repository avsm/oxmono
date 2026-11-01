(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The samples that matrix 0.1.0's emulator misreads.

    Built against matrix 0.1.0, the properties that read the test terminal
    discard the samples that meet two of its defects, both fixed upstream, and
    the characters too new for its Unicode 16.0 tables. This module is
    temporary: remove it and its calls when console moves to matrix 0.2.0, so
    that those properties take every sample again, keeping
    {!discard_newer_unicode} only if that matrix still reads an older Unicode
    than uucp. Built against another matrix, its functions do nothing. *)

val discard_stray_vs16 : string -> unit
(** [discard_stray_vs16 s] discards the sample being tested, with
    {!Alcobar.bad_test}, when [s] holds a grapheme cluster with a VS16 (U+FE0F)
    that console does not count two cells wide, as after a character with no
    emoji presentation, or with a VS16 after a character that is no emoji.
    matrix 0.1.0 draws such a cluster two cells wide, and the cell of a spacing
    mark that a VS16 follows, which the test terminal draws apart, two cells
    wide too (invariant-hq/mosaic#89). *)

val discard_dropped_mark : string -> unit
(** [discard_dropped_mark s] discards the sample being tested when [s] holds a
    character of no width, such as a combining mark or a ZWJ, right after a
    printable ASCII character. matrix 0.1.0 drops it from the screen
    (invariant-hq/mosaic@65da7988, #79). *)

val discard_newer_unicode : string -> unit
(** [discard_newer_unicode s] discards the sample being tested when [s] holds a
    character assigned after Unicode 16.0. matrix 0.1.0's emulator reads Unicode
    16.0, where such a character is unassigned and takes one cell, while console
    measures it with uucp's Unicode. *)

val discard_wide_after_first : string -> unit
(** [discard_wide_after_first s] discards the sample being tested when [s] holds
    a grapheme cluster with a character wider than its first character with any
    width, such as U+2764 U+200D U+1F469, a ZWJ sequence. console counts the
    cells of the widest character, two there, while matrix 0.1.0's emulator
    draws the cluster in the cells of the first, one there. *)
