(* scan.portable.ml - byte-class scans on platforms without [ocaml_simd].

   Selected by dune when [ocaml_simd] is unavailable; see [scan.simd.ml] for
   the SSE2 implementation. *)

include Scan_portable
