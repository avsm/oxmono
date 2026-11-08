(* SPDX-License-Identifier: ISC *)

(* OBuilder announces each operation on a line of its own, such as
   [/src: (run (shell "opam install $DEPS"))], in colour. *)
let strip_ansi s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      if s.[i] = '\027' then
        let rec skip j =
          if j >= String.length s then j
          else
            match s.[j] with
            | 'A' .. 'Z' | 'a' .. 'z' -> j + 1
            | _ -> skip (j + 1)
        in
        go (skip (i + 1))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Only OBuilder's own forms count: [(from IMAGE)] alone, or a working
   directory, a colon and the operation, so build output that happens to start
   with [(run] is left as output. *)
let operation line =
  let plain = strip_ansi line in
  let is op k = String.starts_with ~prefix:("(" ^ k ^ " ") op in
  if is plain "from" then Some plain
  else
    match String.index_opt plain ':' with
    | Some i when String.length plain > 0 && plain.[0] = '/' ->
        let dir = String.sub plain 0 i in
        let op =
          String.trim (String.sub plain (i + 1) (String.length plain - i - 1))
        in
        if
          (not (String.contains dir ' '))
          && List.exists (is op)
               [ "run"; "copy"; "workdir"; "env"; "user"; "shell"; "comment" ]
        then Some op
        else None
    | _ -> None

(* A quoted atom is read as [Spec.atom] wrote it. *)
let unquote s =
  let n = String.length s in
  if n < 2 || s.[0] <> '"' || s.[n - 1] <> '"' then s
  else
    let b = Buffer.create n in
    let rec go i =
      if i < n - 1 then
        if s.[i] = '\\' && i + 1 < n - 1 then (
          Buffer.add_char b (if s.[i + 1] = 'n' then '\n' else s.[i + 1]);
          go (i + 2))
        else (
          Buffer.add_char b s.[i];
          go (i + 1))
    in
    go 1;
    Buffer.contents b

let phase op =
  let prefix = "(comment " in
  if String.starts_with ~prefix op && String.ends_with ~suffix:")" op then
    let n = String.length prefix in
    Some (unquote (String.trim (String.sub op n (String.length op - n - 1))))
  else None

(* A step's name is its shell command, or the operation, kept short. *)
let step_name op =
  let text =
    match String.index_opt op '"' with
    | Some i when String.starts_with ~prefix:"(run" op -> (
        match String.rindex_opt op '"' with
        | Some j when j > i -> String.sub op (i + 1) (j - i - 1)
        | _ -> op)
    | _ -> op
  in
  if String.length text > 72 then String.sub text 0 69 ^ "..." else text

let depth op =
  let d = ref 0 and quoted = ref false and escaped = ref false in
  String.iter
    (fun c ->
      if !escaped then escaped := false
      else if !quoted then (
        if c = '\\' then escaped := true else if c = '"' then quoted := false)
      else
        match c with
        | '"' -> quoted := true
        | '(' -> incr d
        | ')' -> decr d
        | _ -> ())
    op;
  !d
