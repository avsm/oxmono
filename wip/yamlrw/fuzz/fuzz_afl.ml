(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** AFL-specific fuzzer for yamlrw parser.

    This is a standalone AFL fuzzer that reads input from a file or stdin
    and exercises the parser. Build with afl-instrument for best results.

    Usage:
    {[
      # Build with AFL instrumentation
      opam switch create . ocaml-variants.5.2.0+options ocaml-option-afl
      dune build fuzz/fuzz_afl.exe

      # Create seed corpus
      mkdir -p fuzz/input
      echo -n "" > fuzz/input/empty
      echo "null" > fuzz/input/null
      echo "true" > fuzz/input/bool
      echo "42" > fuzz/input/int
      echo "3.14" > fuzz/input/float
      echo "hello" > fuzz/input/string
      echo "key: value" > fuzz/input/mapping
      echo -e "- a\n- b" > fuzz/input/sequence
      echo -e "---\nfoo\n..." > fuzz/input/document
      echo "&anchor value" > fuzz/input/anchor
      echo "!tag value" > fuzz/input/tag
      echo -e "|\n  literal\n  block" > fuzz/input/literal
      echo -e ">\n  folded\n  block" > fuzz/input/folded
      echo "'single quoted'" > fuzz/input/single
      echo '"double quoted"' > fuzz/input/double

      # Run AFL
      afl-fuzz -m none -i fuzz/input -o _fuzz -- _build/default/fuzz/fuzz_afl.exe @@
    ]} *)

(** Read entire file as string *)
let read_file filename =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Read from stdin until EOF *)
let read_stdin () =
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_channel buf stdin 1024
    done;
    assert false
  with End_of_file -> Buffer.contents buf

(** Fuzz target: exercises all major parsing paths *)
let fuzz_target input =
  (* Test value parsing *)
  (try
     let v = Yamlrw.of_string input in
     (* Exercise serialization *)
     let _ = Yamlrw.to_string v in
     (* Exercise different styles *)
     let _ = Yamlrw.to_string ~layout_style:`Block v in
     let _ = Yamlrw.to_string ~layout_style:`Flow v in
     (* Exercise pp *)
     let _ = Format.asprintf "%a" Yamlrw.pp v in
     ()
   with Yamlrw.Yamlrw_error _ -> ());

  (* Test yaml parsing (with alias resolution) *)
  (try
     let y = Yamlrw.yaml_of_string ~resolve_aliases:true input in
     let _ = Yamlrw.yaml_to_string y in
     ()
   with Yamlrw.Yamlrw_error _ -> ());

  (* Test yaml parsing (without alias resolution) *)
  (try
     let y = Yamlrw.yaml_of_string ~resolve_aliases:false input in
     let _ = Yamlrw.yaml_to_string y in
     ()
   with Yamlrw.Yamlrw_error _ -> ());

  (* Test document parsing *)
  (try
     let docs = Yamlrw.documents_of_string input in
     let _ = Yamlrw.documents_to_string docs in
     ()
   with Yamlrw.Yamlrw_error _ -> ());

  (* Test encoding detection *)
  let enc, _ = Yamlrw.Encoding.detect input in
  let _ = Yamlrw.Encoding.to_string enc in

  (* Test streaming parser *)
  (try
     let parser = Yamlrw.Stream.parser input in
     Yamlrw.Stream.iter (fun _ _ _ -> ()) parser
   with Yamlrw.Yamlrw_error _ -> ());

  (* Test scanner directly *)
  (try
     let scanner = Yamlrw.Scanner.of_string input in
     let _ = Yamlrw.Scanner.to_list scanner in
     ()
   with Yamlrw.Yamlrw_error _ -> ())

let () =
  let input =
    if Array.length Sys.argv > 1 then read_file Sys.argv.(1) else read_stdin ()
  in
  fuzz_target input
