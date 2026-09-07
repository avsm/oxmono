module Iarray = Stdlib_stable.Iarray

let default = "application/octet-stream"

(* Compare the extension in place, folding only ASCII uppercase bytes. The
   table keys are lowercase; neither a substring nor a folded copy is needed. *)
let[@zero_alloc] rec compare_extension (name : string @ local) start len key i =
  if i = len || i = String.length key
  then len - String.length key
  else (
    let c = Char.lowercase_ascii name.[start + i] in
    let diff = Char.code c - Char.code key.[i] in
    if diff = 0 then compare_extension name start len key (i + 1) else diff)
;;

let[@zero_alloc] rec lookup (name : string @ local) start len low high =
  if low >= high
  then default
  else (
    let mid = low + ((high - low) / 2) in
    let key, content_type = Iarray.get Mime_data.extensions mid in
    let diff = compare_extension name start len key 0 in
    if diff = 0
    then content_type
    else if diff < 0
    then lookup name start len low mid
    else lookup name start len (mid + 1) high)
;;

(* The last dot after the last slash starts the extension. A leading dot in
   the final segment alone denotes a dotfile, which has no extension. *)
let[@zero_alloc] rec scan (name : string @ local) len i =
  if i <= 0
  then default
  else (
    match name.[i] with
    | '/' -> default
    | '.' ->
      if name.[i - 1] = '/' || i + 1 = len
      then default
      else lookup name (i + 1) (len - i - 1) 0 (Iarray.length Mime_data.extensions)
    | _ -> scan name len (i - 1))
;;

let[@zero_alloc] of_path (name : string @ local) =
  let len = String.length name in
  scan name len (len - 1)
;;
