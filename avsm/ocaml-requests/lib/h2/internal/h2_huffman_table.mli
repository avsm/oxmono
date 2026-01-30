(*---------------------------------------------------------------------------
  Copyright (c) 2019 Antonio Nuno Monteiro.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** {b Internal module - not part of the public API.}

    HPACK Huffman encoding/decoding tables per RFC 7541 Appendix B.
    These tables are used internally by {!H2_hpack}. Direct use is
    not recommended and the API may change without notice. *)

val encode_table : (int * int) array
(** [encode_table.(i)] returns [(code, code_length_in_bits)] for byte value [i].
    Used by the Huffman encoder. *)

val decode_table : (int * bool * char) array
(** State machine table for Huffman decoding.
    Each entry is [(next_state, is_accepting, output_char)]. *)
