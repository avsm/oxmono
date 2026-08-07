(* Percent-decoding, written once and shared by path segments, query strings
   and form bodies. An invalid escape decodes to the bytes as written rather
   than raising, so a malformed target still reaches a handler that can answer
   404 instead of failing the connection. *)

let hex c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let decode ~plus s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if plus && c = '+' then (
      Buffer.add_char b ' ';
      incr i)
    else if c = '%' && !i + 2 < n then
      match (hex s.[!i + 1], hex s.[!i + 2]) with
      | Some hi, Some lo ->
          Buffer.add_char b (Char.chr ((hi * 16) + lo));
          i := !i + 3
      | _ ->
          Buffer.add_char b c;
          incr i
    else (
      Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

(* [split c s] is [s] cut at every occurrence of [c], keeping empty pieces. *)
let split c s =
  let rec go acc start i =
    if i >= String.length s then
      List.rev (String.sub s start (i - start) :: acc)
    else if s.[i] = c then
      go (String.sub s start (i - start) :: acc) (i + 1) (i + 1)
    else go acc start (i + 1)
  in
  go [] 0 0

(* [segments path] is [path] split on '/', with empty pieces dropped and each
   piece percent-decoded. '+' is literal in a path. *)
let segments path =
  List.filter_map
    (fun p -> if p = "" then None else Some (decode ~plus:false p))
    (split '/' path)

(* [pairs s] is an application/x-www-form-urlencoded string decoded into an
   association list. '+' means space. A piece with no '=' has an empty value. *)
let pairs s =
  List.filter_map
    (fun piece ->
      if piece = "" then None
      else
        match String.index_opt piece '=' with
        | None -> Some (decode ~plus:true piece, "")
        | Some i ->
            let k = String.sub piece 0 i in
            let v = String.sub piece (i + 1) (String.length piece - i - 1) in
            Some (decode ~plus:true k, decode ~plus:true v))
    (split '&' s)
