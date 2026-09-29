(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The filesystem scanning tools, against a real temporary tree.

   The property that matters most is a negative one. Given a capability from
   [Eio.Path.with_subtree], nothing these tools are asked to do may reach
   outside that subtree. That is worth testing on a real filesystem rather than
   a mock, since Eio's path resolution is what enforces it. *)

module Tool = Ds4.Tool
module Toolbox = Ds4.Toolbox

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

(* Drive a tool as the agent does, with JSON arguments through the codec. *)
let call tool args =
  Tool.invoke tool { Dsml.name = Tool.name tool; arguments = args; id = None }

let populate root =
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "top.txt")
    "alpha\nbeta\ngamma\ndelta\nepsilon\n";
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "sub");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "sub" / "nested.ml")
    "let needle = 1\nlet other = 2\n  (* an indented needle *)\n";
  (* A file the scanner should skip rather than emit noise from. *)
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "sub" / "blob.bin")
    "\000\000needle\000\000";
  (* A passage that occurs twice, which an edit must refuse rather than pick
     the first of. *)
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "twice.txt")
    "let x = 1\nlet y = 2\nlet x = 1\n";
  (* Longer than one result holds, so that paging can be walked end to end. *)
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "long.txt")
    (String.concat ""
       (List.init 400 (fun i ->
            Printf.sprintf "line %d: %s\n" (i + 1) (String.make 40 'x'))));
  (* A build directory, which mirrors the source and should not be walked. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(root / "_build");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(root / "_build" / "copy.ml")
    "let needle = 1\n"

let run env =
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-scan" "" in
  Sys.remove tmp;
  let root_path = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 root_path;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () ->
      populate root_path;
      (* Everything below reaches the tree only through this capability. *)
      Eio.Path.with_subtree root_path @@ fun dir ->
      Eio.Switch.run @@ fun sw ->
      (* Record what was asked for, so the test can assert on approval. *)
      let requested = ref [] in
      let approve p =
        requested := p :: !requested;
        true
      in
      let caps = Toolbox.Caps.create ~sw ~fs ~approve dir in
      let list = Toolbox.list ~caps in
      let read = Toolbox.read ~caps in
      let read_lines = Toolbox.read_lines ~caps in
      let find = Toolbox.find ~caps in
      let grep = Toolbox.grep ~caps in
      let stat = Toolbox.stat ~caps in
      let tree = Toolbox.tree ~caps in
      let open_dir = Toolbox.open_dir ~caps in

      let out = call list {|{"cap":"","path":"."}|} in
      check "list reports a regular file with its size"
        (contains out "file" && contains out "top.txt");
      check "list distinguishes directories"
        (contains out "dir" && contains out "sub");

      let out =
        call read_lines {|{"cap":"","path":"top.txt","start":2,"count":2}|}
      in
      check "read_lines returns just the requested window"
        (contains out "beta" && contains out "gamma"
        && (not (contains out "alpha"))
        && not (contains out "delta"));
      check "read_lines numbers the lines it returns" (contains out "2  beta");

      check "a window short of the file's end says where it sits in the file"
        (contains out "lines 2-3 of 5");
      check "a window the caller's own count ended asks for nothing more"
        (not (contains out "Read from line"));

      let out =
        call read_lines {|{"cap":"","path":"top.txt","start":4,"count":0}|}
      in
      check "read_lines with count<=0 runs to end of file"
        (contains out "delta" && contains out "epsilon"
        && not (contains out "gamma"));
      check
        "a window that reached the end of the file says so by saying nothing"
        ((not (contains out "lines 4-5 of 5"))
        && not (contains out "Read from line"));

      (* A file longer than one result holds must still be reachable whole.
         What is elided from a result cannot be asked for again, since nothing
         in it says which lines went, so each of these tools stops short of
         that and names the line to continue from. The property that matters is
         that following those instructions terminates and leaves no line
         unread. *)
      let after out marker =
        let n = String.length out and m = String.length marker in
        let rec at i =
          if i + m > n then None
          else if String.sub out i m = marker then Some (i + m)
          else at (i + 1)
        in
        at 0
      in
      let next_line out =
        match after out "Read from line " with
        | None -> None
        | Some i ->
            let j = ref i in
            while
              !j < String.length out && out.[!j] >= '0' && out.[!j] <= '9'
            do
              incr j
            done;
            int_of_string_opt (String.sub out i (!j - i))
      in
      let out = call read {|{"cap":"","path":"long.txt"}|} in
      check "read holds back a file too long for one result"
        (contains out "line 1: " && not (contains out "line 400: "));
      check "a paged read says how much of the file it holds"
        (contains out "of 400.");
      check "a paged read names the line to read on from, and the tool for it"
        (next_line out <> None && contains out "read_lines");
      (* The line named must be the one after the last shown, or the model
         would read past what it was given or over it again. *)
      check "a paged read resumes exactly where it stopped"
        (match next_line out with
        | None -> false
        | Some n ->
            contains out (Printf.sprintf "line %d: " (n - 1))
            && not (contains out (Printf.sprintf "line %d: " n)));

      (* The line numbers a page carries. A note has no number in that column,
         so it drops out of the count rather than being read as a line. *)
      let numbers page =
        String.split_on_char '\n' page
        |> List.filter_map (fun line ->
            if String.length line < 6 then None
            else int_of_string_opt (String.trim (String.sub line 0 6)))
      in
      let seen = Hashtbl.create 512 in
      let elided = ref false in
      let rec walk start pages =
        let out =
          call read_lines
            (Printf.sprintf
               {|{"cap":"","path":"long.txt","start":%d,"count":0}|} start)
        in
        (* 4000 characters is the agent's default limit on a tool result. A
           page over it reaches the model with its middle removed, which is the
           wall this walk is here to prove is gone. *)
        if String.length out > 4000 then elided := true;
        List.iter (fun n -> Hashtbl.replace seen n ()) (numbers out);
        if pages > 20 then false
        else
          match next_line out with
          | Some n when n > start -> walk n (pages + 1)
          | Some _ -> false
          | None -> true
      in
      check "paging a long file reaches its end in bounded steps" (walk 1 0);
      check "paging a long file leaves no line unread"
        (Hashtbl.length seen = 400);
      check "a page arrives whole rather than with its middle removed"
        (not !elided);

      let out = call find {|{"cap":"","path":".","substring":"nested"}|} in
      check "find walks into subdirectories" (contains out "sub/nested.ml");

      let out = call grep {|{"cap":"","path":".","substring":"needle"}|} in
      check "grep reports path:line: for a match"
        (contains out "sub/nested.ml:1:");
      check "grep skips binary files" (not (contains out "blob.bin"));

      check "grep quotes a matching line as it is, indentation and all"
        (contains out "sub/nested.ml:3:   (* an indented needle *)");
      check "grep says how many files it did not search, and why"
        (contains out "Not searched: 1 binary");

      let out = call grep {|{"cap":"","path":".","substring":""}|} in
      check "grep refuses an empty pattern" (contains out "refusing");

      (* A search of a path that is not there must not read as a search that
         found nothing, which would send the model elsewhere to look. *)
      let out = call grep {|{"cap":"","path":"nowhere","substring":"x"}|} in
      check "grep of a missing path is an error, not an empty result"
        (not (contains out "no matches"));

      let out = call stat {|{"cap":"","path":"sub"}|} in
      check "stat identifies a directory" (contains out "kind=dir");

      (* An edit that names one place changes that place and nothing else. The
         lines around it are what proves the rest of the file survived, since a
         tool that rewrote the whole file would pass a check on the new text
         alone. *)
      let edit = Toolbox.edit ~caps in
      let out =
        call edit {|{"cap":"","path":"top.txt","old":"gamma","new":"GAMMA"}|}
      in
      check "edit reports the file it changed" (contains out "top.txt");
      check "edit shows the changed line numbered, with the lines around it"
        (contains out "     3  GAMMA"
        && contains out "     1  alpha"
        && contains out "     5  epsilon");
      check "an edit that keeps the line count says nothing moved"
        (not (contains out "moved"));
      let out =
        call edit
          {|{"cap":"","path":"top.txt","old":"beta\n","new":"beta\nbeta2\nbeta3\n"}|}
      in
      check "an edit that adds lines says how far the rest moved"
        (contains out "moved by +2");
      ignore
        (call edit
           {|{"cap":"","path":"top.txt","old":"beta\nbeta2\nbeta3\n","new":"beta\n"}|});
      let out = call read {|{"cap":"","path":"top.txt"}|} in
      check "edit replaces the passage"
        (contains out "GAMMA" && not (contains out "gamma"));
      check "edit leaves the rest of the file alone"
        (out = "alpha\nbeta\nGAMMA\ndelta\nepsilon\n");

      (* The refusals matter more than the success. Each must leave the file as
         it was, since an edit that reports a failure and changes something
         anyway is worse than one that cannot run at all. *)
      let out =
        call edit {|{"cap":"","path":"top.txt","old":"zeta","new":"ZETA"}|}
      in
      check "edit refuses a passage that is not there"
        (contains out "is not in" && contains out "top.txt");
      check "a refused edit leaves the file as it was"
        (call read {|{"cap":"","path":"top.txt"}|}
        = "alpha\nbeta\nGAMMA\ndelta\nepsilon\n");

      let out =
        call edit
          {|{"cap":"","path":"twice.txt","old":"let x = 1","new":"let x = 3"}|}
      in
      check "edit refuses a passage that names two places"
        (contains out "appears 2 times");
      check "edit says how to narrow an ambiguous passage"
        (contains out "more of the lines");
      check "an ambiguous edit leaves the file as it was"
        (call read {|{"cap":"","path":"twice.txt"}|}
        = "let x = 1\nlet y = 2\nlet x = 1\n");

      (* Narrowed by the line above it, the same passage names one place. *)
      let out =
        call edit
          {|{"cap":"","path":"twice.txt","old":"let y = 2\nlet x = 1","new":"let y = 2\nlet x = 3"}|}
      in
      check "an edit narrowed by its surroundings goes through"
        (contains out "twice.txt" && not (contains out "appears"));
      check "the narrowed edit changed the second occurrence"
        (call read {|{"cap":"","path":"twice.txt"}|}
        = "let x = 1\nlet y = 2\nlet x = 3\n");

      let out =
        call edit {|{"cap":"","path":"top.txt","old":"","new":"nothing"}|}
      in
      check "edit refuses the empty passage" (contains out "refusing");

      (* A write replaces the file whole by renaming a new one over it. What
         that must not change is the mode of the file it replaces, and it must
         leave nothing of its own behind. *)
      let write = Toolbox.write ~caps in
      Unix.chmod (Filename.concat tmp "top.txt") 0o755;
      let out =
        call write {|{"cap":"","path":"top.txt","content":"replaced\n"}|}
      in
      check "write reports what it wrote" (contains out "wrote 9 bytes");
      check "write keeps the mode of the file it replaces"
        ((Unix.stat (Filename.concat tmp "top.txt")).Unix.st_perm = 0o755);
      check "write leaves no temporary file behind"
        (not (Array.exists (fun f -> contains f "ds4-tmp") (Sys.readdir tmp)));
      ignore
        (call write
           {|{"cap":"","path":"top.txt","content":"alpha\nbeta\nGAMMA\ndelta\nepsilon\n"}|});

      (* A file too long to send in one reply is written in parts, which is
         only possible if a part can be added to what is already there. The
         property that matters is that nothing is lost between the parts. *)
      let append = Toolbox.append ~caps in
      let out =
        call append {|{"cap":"","path":"grown.txt","content":"one\n"}|}
      in
      check "append creates a file that is not there"
        (contains out "grown.txt" && contains out "4");
      let out =
        call append {|{"cap":"","path":"grown.txt","content":"two\n"}|}
      in
      check "append says how large the file has grown" (contains out "8");
      check "append adds to what was there, losing nothing"
        (call read {|{"cap":"","path":"grown.txt"}|} = "one\ntwo\n");
      let escaped =
        call append {|{"cap":"","path":"../escaped.txt","content":"x"}|}
      in
      check "append cannot escape the subtree with .."
        (not (contains escaped "appended"));
      let escaped =
        call append {|{"cap":"","path":"/tmp/ds4-escaped.txt","content":"x"}|}
      in
      check "append refuses an absolute path with what to do instead"
        (contains escaped "outside this capability"
        && contains escaped "open_dir");

      (* An edit that climbs out is refused by the capability resolving it, as
         a read that climbs out is. What matters is that it fails and says so,
         rather than reporting the edit of a file it never reached. *)
      let out =
        call edit
          {|{"cap":"","path":"../../etc/passwd","old":"root","new":"toor"}|}
      in
      check "edit cannot escape the subtree with .."
        (not (contains out "edited"));
      let out =
        call edit {|{"cap":"","path":"/etc/passwd","old":"root","new":"toor"}|}
      in
      check "edit refuses an absolute path with what to do instead"
        (contains out "outside this capability" && contains out "open_dir");

      (* The point of the subtree capability. Eio resolves these. *)
      let escaped =
        call read_lines
          {|{"cap":"","path":"../../etc/passwd","start":1,"count":1}|}
      in
      check "read_lines cannot escape the subtree with .."
        (not (contains escaped "root:"));
      let escaped = call list {|{"cap":"","path":".."}|} in
      check "list cannot escape the subtree with .."
        (not (contains escaped "etc"));
      let escaped = call stat {|{"cap":"","path":"/etc/passwd"}|} in
      check "stat cannot reach an absolute path outside the subtree"
        (not (contains escaped "kind=file"));

      (* One call in place of walking the tree with repeated lists. *)
      let out = call tree {|{"cap":"","path":".","depth":0}|} in
      check "tree reports nested entries indented"
        (contains out "sub/" && contains out "  nested.ml");
      check "tree gives each directory its entry count"
        (contains out "sub/ (2)");

      (* At the depth limit a directory is listed but not descended into. Its
         count is what distinguishes that from an empty one. *)
      let out = call tree {|{"cap":"","path":".","depth":1}|} in
      check "tree stops at the requested depth" (not (contains out "nested.ml"));
      check "a directory cut off by depth still reports what it holds"
        (contains out "sub/ (2)");

      (* Minting narrows reach. The new capability sees sub/, not the root. *)
      let minted = String.trim (call open_dir {|{"cap":"","path":"sub"}|}) in
      check "open_dir returns a capability name" (minted <> "");
      let out = call list (Printf.sprintf {|{"cap":%S,"path":"."}|} minted) in
      check "a minted capability sees its own directory"
        (contains out "nested.ml");
      check "a minted capability does not see its parent"
        (not (contains out "top.txt"));
      let out =
        call read_lines
          (Printf.sprintf {|{"cap":%S,"path":"../top.txt","start":1,"count":1}|}
             minted)
      in
      check "a minted capability cannot escape upwards to its parent"
        (not (contains out "alpha"));

      let out = call list {|{"cap":"nope","path":"."}|} in
      check "an unknown capability is reported rather than silently allowed"
        (contains out "unknown capability");

      (* A build tree is reported but not walked, so it cannot bury the rest. *)
      let out = call tree {|{"cap":"","path":".","depth":3}|} in
      check "tree lists a build directory with its count"
        (contains out "_build/ (1)");
      check "tree does not descend into a build directory"
        (not (contains out "copy.ml"));
      (* Source first, machinery last, so the entry limit cuts the right end. *)
      let index sub =
        let n = String.length out and m = String.length sub in
        let rec at i =
          if i + m > n then max_int
          else if String.sub out i m = sub then i
          else at (i + 1)
        in
        at 0
      in
      check "tree orders underscore names after the rest"
        (index "sub/" < index "_build/" && index "top.txt" < index "_build/");
      let out = call grep {|{"cap":"","path":".","substring":"needle"}|} in
      check "grep does not search inside a build directory"
        (not (contains out "_build/copy.ml"));

      (* Which directories already have a capability should be plain to see. *)
      let out = call tree {|{"cap":"","path":".","depth":2}|} in
      check "tree marks a directory that has a capability"
        (contains out ("sub/ (2) " ^ minted));
      let out = call list {|{"cap":"","path":"."}|} in
      check "list marks a directory that has a capability" (contains out minted);

      (* A tool given a path outside its capability must say so. Reporting an
         empty directory instead is what sent a model looking for a shell. *)
      let out = call tree {|{"cap":"","path":"/etc","depth":2}|} in
      check "tree refuses an absolute path rather than reporting it empty"
        (contains out "outside this capability");
      let out = call list {|{"cap":"","path":"/etc"}|} in
      check "list refuses an absolute path"
        (contains out "outside this capability");

      (* Asking for somewhere outside goes through approval and then works. *)
      let granted = String.trim (call open_dir {|{"cap":"","path":"/etc"}|}) in
      check "open_dir consults approval for a path outside" (!requested <> []);
      check "open_dir returns a capability for an approved path"
        (granted <> "" && not (contains granted "outside"));
      let out = call list (Printf.sprintf {|{"cap":%S,"path":"."}|} granted) in
      check "the granted capability can be listed" (String.length out > 0))

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
