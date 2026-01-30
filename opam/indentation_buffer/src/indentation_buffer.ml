open! Core

type t =
  { mutable indent : int
  ; mutable at_start_of_line : bool
  ; buffer : Buffer.t
  }

let string t s =
  if t.at_start_of_line
  then
    for _ = 0 to (t.indent * 2) - 1 do
      Buffer.add_char t.buffer ' '
    done;
  t.at_start_of_line <- false;
  Buffer.add_string t.buffer s
;;

let newline t =
  Buffer.add_string t.buffer "\n";
  t.at_start_of_line <- true
;;

let indent t = t.indent <- t.indent + 1
let dedent t = t.indent <- t.indent - 1

let create capacity =
  { indent = 0; at_start_of_line = true; buffer = Buffer.create capacity }
;;

let contents t = Buffer.contents t.buffer
