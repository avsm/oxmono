(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let root = Filename.concat Filename.parent_dir_name Filename.parent_dir_name
let read path = In_channel.with_open_bin path In_channel.input_all

(* The references of a line of INVARIANTS.md: each `file` "name". *)
let references line =
  let n = String.length line in
  let rec go i acc =
    match String.index_from_opt line i '`' with
    | None -> List.rev acc
    | Some a -> (
        match String.index_from_opt line (a + 1) '`' with
        | None -> List.rev acc
        | Some b ->
            let file = String.sub line (a + 1) (b - a - 1) in
            if b + 2 < n && line.[b + 1] = ' ' && line.[b + 2] = '"' then
              match String.index_from_opt line (b + 3) '"' with
              | None -> List.rev acc
              | Some c ->
                  go (c + 1) ((file, String.sub line (b + 3) (c - b - 3)) :: acc)
            else go (b + 1) acc)
  in
  go 0 []

(* The test cases of an OCaml file: the string after each [test_case]. *)
let test_cases source =
  let n = String.length source and key = "test_case" in
  let rec go i acc =
    if i >= n then acc
    else
      match String.index_from_opt source i 't' with
      | None -> acc
      | Some j
        when j + String.length key <= n
             && String.sub source j (String.length key) = key ->
          let k = ref (j + String.length key) in
          while !k < n && (source.[!k] = ' ' || source.[!k] = '\n') do
            incr k
          done;
          if !k < n && source.[!k] = '"' then
            match String.index_from_opt source (!k + 1) '"' with
            | None -> acc
            | Some e -> go e (String.sub source (!k + 1) (e - !k - 1) :: acc)
          else go !k acc
      | Some j -> go (j + 1) acc
  in
  go 0 []

let is_infix ~affix s =
  let n = String.length affix and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = affix || go (i + 1)) in
  go 0

(* A test exists when an OCaml file has a test case of that name, or a cram
   file holds that phrase. *)
let exists (file, name) =
  let path = Filename.concat root file in
  Sys.file_exists path
  &&
  let source = read path in
  if Filename.check_suffix file ".ml" then List.mem name (test_cases source)
  else is_infix ~affix:name source

let rows () =
  read (Filename.concat root "INVARIANTS.md")
  |> String.split_on_char '\n'
  |> List.filter (fun l ->
      String.starts_with ~prefix:"| " l
      && not
           (String.starts_with ~prefix:"| Invariant |" l
           || String.starts_with ~prefix:"| Interface |" l))

let test_named_tests_exist () =
  let rows = rows () in
  Alcotest.(check bool) "INVARIANTS.md has rows" true (List.length rows > 20);
  List.iter
    (fun row ->
      match references row with
      | [] -> Alcotest.failf "a row names no test: %s" row
      | refs ->
          List.iter
            (fun ((file, name) as r) ->
              if not (exists r) then
                Alcotest.failf "%s has no test %S, which INVARIANTS.md names"
                  file name)
            refs)
    rows

let test_stdlib_utf8_boundaries () =
  let source =
    read (Filename.concat root "lib/input/width.ml")
    |> String.lowercase_ascii |> String.to_seq
    |> Seq.filter (fun c -> not (List.mem c [ ' '; '\t'; '\r'; '\n' ]))
    |> String.of_seq
  in
  Alcotest.(check bool)
    "UTF-8 boundaries do not scan continuation bytes with a bit mask" false
    (is_infix ~affix:"land0xc0=0x80" source)

let () =
  Alcotest.run "invariants"
    [
      ( "invariants",
        [
          Alcotest.test_case "UTF-8 boundaries use the Stdlib decoder" `Quick
            test_stdlib_utf8_boundaries;
          Alcotest.test_case "every named test exists" `Quick
            test_named_tests_exist;
        ] );
    ]
