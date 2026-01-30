"Ppx_embed_file"
================

`ppx_embed_file` is a simple PPX that allows embedding files directly into executables as strings or
bytes so that we can embed files into OCaml programs.

## How to use

First, in your `dune` file, specify `preprocess` to include `ppx_embed_file` and
`preprocessor_deps` to include any files to be included:

```sh
$ cat test/dune
(library (
  (name ppx_embed_test)
  (libraries (core))
  (preprocessor_deps (hello_world.txt))
  (preprocess (pps (ppx_jane ppx_embed_file)))))
```

Then, write the appropriate extension nodes in your `.ml` files:

```ocaml
open! Core

let hello_world_string = [%embed_file_as_string "hello_world.txt"]

let hello_world_string_with_filename =
  [%embed_file_as_string_with_filename "hello_world.txt"]
;;

let%expect_test "string test" =
  Core.print_string hello_world_string;
  [%expect {xxx| Hello world! |xxx}]
;;

let%expect_test "string with filename test" =
  [%sexp_of: string * string] hello_world_string_with_filename |> Core.print_s;
  [%expect {xxx| (hello_world.txt "Hello world!\n") |xxx}]
;;
```
