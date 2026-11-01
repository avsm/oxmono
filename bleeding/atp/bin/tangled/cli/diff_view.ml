(* SPDX-License-Identifier: ISC *)
(* A knot's diff, printed as a unified diff. The knot names the novel lines
   of each side, not their text, so the text comes from the two blobs. *)

module Api = Tangled.Api
module Defs = Api.Lex.Git.Defs

type file = {
  lhs : Defs.diff_src;
  rhs : Defs.diff_src;
  hunks : Defs.diff_hunk list;
}

let file_of json =
  let d = Api.decode Defs.file_diff_jsont json in
  {
    lhs = Api.decode Defs.diff_src_jsont d.lhs_src;
    rhs = Api.decode Defs.diff_src_jsont d.rhs_src;
    hunks = List.map (Api.decode Defs.diff_hunk_jsont) d.hunks;
  }

(* An absent side has an all-zero object ID. *)
let absent (src : Defs.diff_src) =
  src.oid = "" || String.for_all (( = ) '0') src.oid

let counts f =
  List.fold_left
    (fun (a, d) (h : Defs.diff_hunk) ->
      (a + List.length h.novel_rhs, d + List.length h.novel_lhs))
    (0, 0) f.hunks

let name f = if absent f.rhs then f.lhs.path else f.rhs.path

let lines text =
  let l = String.split_on_char '\n' text in
  (* A final newline ends the last line rather than starting another. *)
  let l =
    match List.rev l with "" :: rest when rest <> [] -> List.rev rest | _ -> l
  in
  Array.of_list l

let side api repository ~rev (src : Defs.diff_src) =
  match src.content with
  | Some text -> lines text
  | None when absent src || src.is_binary || src.is_submodule -> [||]
  | None -> lines (Api.blob api repository ~ref_:rev ~path:src.path ())

let line_at a i = if i >= 0 && i < Array.length a then a.(i) else ""

(* The rendered lines of one file, each tagged with its kind. *)
let render_file f ~old ~new_ =
  let out = ref [] in
  let emit kind text = out := (kind, text) :: !out in
  emit `Header (Printf.sprintf "diff --git a/%s b/%s" f.lhs.path f.rhs.path);
  if absent f.lhs then emit `Header "new file"
  else if absent f.rhs then emit `Header "deleted file";
  if f.lhs.is_binary || f.rhs.is_binary then emit `Header "Binary files differ"
  else if f.lhs.is_submodule || f.rhs.is_submodule then
    emit `Header
      (Printf.sprintf "Submodule %s -> %s" (Ui.short_sha f.lhs.oid)
         (Ui.short_sha f.rhs.oid))
  else begin
    emit `Header
      ("--- " ^ if absent f.lhs then "/dev/null" else "a/" ^ f.lhs.path);
    emit `Header
      ("+++ " ^ if absent f.rhs then "/dev/null" else "b/" ^ f.rhs.path);
    List.iter
      (fun (h : Defs.diff_hunk) ->
        let pairs = List.map (Api.decode Defs.line_pair_jsont) h.lines in
        let first f = List.find_map f pairs in
        let start side = Option.fold ~none:0 ~some:(fun n -> n + 1) side in
        let count f = List.length (List.filter_map f pairs) in
        emit `Hunk
          (Printf.sprintf "@@ -%d,%d +%d,%d @@"
             (start (first (fun (p : Defs.line_pair) -> p.lhs)))
             (count (fun (p : Defs.line_pair) -> p.lhs))
             (start (first (fun (p : Defs.line_pair) -> p.rhs)))
             (count (fun (p : Defs.line_pair) -> p.rhs)));
        (* Removed lines print before the added lines they pair with. *)
        let minus = ref [] and plus = ref [] in
        let flush () =
          List.iter (emit `Removed) (List.rev !minus);
          List.iter (emit `Added) (List.rev !plus);
          minus := [];
          plus := []
        in
        List.iter
          (fun (p : Defs.line_pair) ->
            let novel_l =
              Option.fold ~none:false
                ~some:(fun l -> List.mem l h.novel_lhs)
                p.lhs
            and novel_r =
              Option.fold ~none:false
                ~some:(fun r -> List.mem r h.novel_rhs)
                p.rhs
            in
            match (p.lhs, p.rhs) with
            | Some _, Some r when not (novel_l || novel_r) ->
                flush ();
                emit `Context (line_at new_ r)
            | _ ->
                Option.iter
                  (fun l ->
                    if novel_l || p.rhs = None then
                      minus := line_at old l :: !minus)
                  p.lhs;
                Option.iter
                  (fun r ->
                    if novel_r || p.lhs = None then
                      plus := line_at new_ r :: !plus)
                  p.rhs)
          pairs;
        flush ())
      f.hunks
  end;
  List.rev !out

let print_line (kind, text) =
  if Ui.tty () then
    let view =
      match kind with
      | `Header -> Ui.bold text
      | `Hunk -> Ui.V.role `Accent (Ui.text text)
      | `Removed -> Ui.V.role `Failed (Ui.text ("-" ^ text))
      | `Added -> Ui.V.role `Succeeded (Ui.text ("+" ^ text))
      | `Context -> Ui.text (" " ^ text)
    in
    Ui.print view
  else
    print_endline
      (match kind with
      | `Header | `Hunk -> text
      | `Removed -> "-" ^ text
      | `Added -> "+" ^ text
      | `Context -> " " ^ text)

let stat files (stats : Defs.diff_stat option) =
  let width =
    List.fold_left (fun w f -> max w (String.length (name f))) 0 files
  in
  List.iter
    (fun f ->
      let added, removed = counts f in
      let path = name f ^ String.make (width - String.length (name f)) ' ' in
      if Ui.tty () then
        Ui.print
          (Ui.V.concat
             [
               Ui.text (" " ^ path ^ " | ");
               Ui.V.role `Succeeded (Ui.text (Printf.sprintf "+%d" added));
               Ui.text " ";
               Ui.V.role `Failed (Ui.text (Printf.sprintf "-%d" removed));
             ])
      else Printf.printf " %s | +%d -%d\n" path added removed)
    files;
  Option.iter
    (fun (s : Defs.diff_stat) ->
      let line =
        Printf.sprintf " %d files changed, %d insertions(+), %d deletions(-)"
          s.files_changed s.insertions s.deletions
      in
      if Ui.tty () then Ui.print (Ui.muted line) else print_endline line)
    stats

let show api repository ~base ~head ~stat:only_stat =
  let d = Api.diff api repository ~base ~head () in
  let files = List.map file_of d.diffs in
  let stats = Option.map (Api.decode Defs.diff_stat_jsont) d.stats in
  if only_stat then stat files stats
  else
    let rendered =
      Ui.busy "Reading files" (fun () ->
          Eio.Fiber.List.map ~max_fibers:8
            (fun f ->
              render_file f
                ~old:(side api repository ~rev:base f.lhs)
                ~new_:(side api repository ~rev:head f.rhs))
            files)
    in
    List.iter (List.iter print_line) rendered
