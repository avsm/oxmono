(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What a workspace leaves for an agent, against a real temporary directory.

   The property that matters is what happens to a file too long to join every
   prompt. It is cut, and a cut that says nothing leaves the model reading a
   sentence that stops for no reason and unaware that the rest of it is on disk
   and reachable with the tools it holds. *)

module Instructions = Agentkit.Instructions

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let index s sub =
  let n = String.length s and m = String.length sub in
  let rec at i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else at (i + 1)
  in
  at 0

let save ws text =
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(ws / "AGENTS.md") text

let run env =
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-instructions" "" in
  Sys.remove tmp;
  let ws = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 ws;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () ->
      check "a workspace with no AGENTS.md leaves nothing"
        (Instructions.load ws = None);

      save ws "  \n\n";
      check "a file holding only whitespace leaves nothing"
        (Instructions.load ws = None);

      save ws "Build with dune.\n";
      check "a short file is passed on as it stands"
        (Instructions.load ws = Some "Build with dune.\n");

      (* Longer than what joins every prompt, in lines long enough that a cut
         made without regard for them would land inside one. *)
      let line i = Printf.sprintf "rule %d: %s\n" i (String.make 40 'x') in
      let long = String.concat "" (List.init 600 (fun i -> line (i + 1))) in
      save ws long;
      match Instructions.load ws with
      | None -> check "a long file is still loaded" false
      | Some text ->
          check "a long file is cut" (String.length text < String.length long);
          check "a cut file says which file the rest of it is in"
            (contains text "AGENTS.md" && contains text "for the rest");
          let body =
            match index text "\n\n[AGENTS.md" with
            | None -> text
            | Some i -> String.sub text 0 i
          in
          check "what is passed on stays within what a prompt carries"
            (String.length body <= 8000);
          (* A cut inside a line quotes half a rule as if it were the whole of
             it, which is worse than saying less. *)
          let last =
            List.filter
              (fun l -> String.trim l <> "")
              (String.split_on_char '\n' body)
            |> List.rev |> List.hd
          in
          check "the cut falls at a line boundary"
            (String.ends_with ~suffix:(String.make 40 'x') last))

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
