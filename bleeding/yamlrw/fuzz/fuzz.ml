(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Main entry point for fuzz tests.

    Run without arguments for Crowbar's default mode (quick check):
    {[
      dune exec fuzz/fuzz.exe
    ]}

    Run with AFL for thorough fuzzing:
    {[
      mkdir -p fuzz/input
      echo -n "" > fuzz/input/empty
      echo "key: value" > fuzz/input/simple
      echo -e "- a\n- b\n- c" > fuzz/input/list
      afl-fuzz -m none -i fuzz/input -o _fuzz -- _build/default/fuzz/fuzz.exe @@
    ]}

    For AFL mode, build with afl-instrument:
    {[
      opam install crowbar afl-persistent
      dune build fuzz/fuzz.exe
    ]} *)

(* Force linking of all fuzz test modules via side effects *)
let () =
  Fuzz_common.run ();
  Fuzz_encoding.run ();
  Fuzz_chomping.run ();
  Fuzz_tag.run ();
  Fuzz_value.run ();
  Fuzz_yamlrw.run ();
  Fuzz_emitter.run ()
