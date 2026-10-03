(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Matrix ASCII digital-rain frame.

    Internal: {!Panel} and {!Table} draw their animated borders through this. *)

val frame :
  ?char_width:(Uchar.t -> int) -> string list -> elapsed:float -> string
(** [frame ?char_width lines ~elapsed] surrounds the content [lines] (already
    rendered, padded to a common width) with a Matrix-style ASCII rain border
    [elapsed] seconds in: a bold green head sweeps clockwise round the box,
    trailing dimmer green, every cell flickering through an ASCII glyph.
    Deterministic in [elapsed]. *)
