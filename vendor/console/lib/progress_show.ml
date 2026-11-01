(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Layout = Progress_layout
module V = View_core
open Task_core

let tasks _ = V.tasks
let lines vs = V.concat ~sep:(V.text "\n") vs

let row ?(layout = Layout.default) r =
  V.deferred ~kind:(Layout.reads layout) (fun c ->
      let look = Ctx.look c and width = Ctx.width c in
      if Progress_tree.draws_label layout r then widen_label r;
      let columns = Progress_row.columns look r in
      if is_running r then
        Progress_row.live look ~time:(Ctx.time c) ?width ~columns layout r
      else lines (Progress_row.ended look ?width ~columns ~whole:false r))

let tree ?(layout = Layout.default) r = Progress_tree.view ~layout r

let summary root =
  V.deferred ~kind:tasks (fun c ->
      Option.value ~default:V.empty
        (Progress_row.summary (Ctx.look c) ?width:(Ctx.width c) (root_of root)))

let verdict root =
  V.deferred ~kind:tasks (fun c ->
      lines
        (Progress_row.verdict (Ctx.look c) ?width:(Ctx.width c) (root_of root)))

(* On a wide terminal, a count or a timer at the right edge is far from its
   label: the rows keep to 100 columns. *)
let view ?(layout = Layout.default) root =
  let root = root_of root in
  V.deferred ~kind:(Layout.reads layout) (fun c ->
      let width = Option.map (min 100) (Ctx.width c) in
      let look = Ctx.look c in
      let below =
        Option.to_list (Progress_row.summary look ?width root)
        @ Progress_row.verdict look ?width root
      in
      let height = Option.map (fun h -> h - List.length below) (Ctx.height c) in
      let tree =
        match height with
        | Some h when h <= 0 -> V.empty
        | _ -> V.resolve (Ctx.with_room ~width ~height c) (tree ~layout root)
      in
      V.vcat (tree :: below))

type form = Live | Plain | Quiet | Json

(* The size in which the rows are redrawn, if they are, and the lines
   printed otherwise. *)
let shape ?size = function
  | `Auto -> (size, if Option.is_some size then Live else Plain)
  | `Tty -> (Some (Option.value size ~default:(fun () -> (80, 24))), Live)
  | `Plain -> (None, Plain)
  | `Quiet -> (None, Quiet)
  | `Json -> (None, Json)

let print d vs = Display_core.print_lines d vs

(* A top task that ends, unless the ended tasks are kept, leaves the region
   for the scrollback as [commit] draws it, its tree by default: laid out
   first, then forgotten, then printed, so it never shows twice. *)
let moved d ~layout ~commit r =
  let look = Display_core.look d in
  let width = min (Display_core.width d) 100 in
  let committed =
    match commit with None -> Some (tree ~layout r) | Some f -> f r
  in
  let committed = Option.map (V.resolve (Ctx.v ~width look)) committed in
  forget r;
  match committed with
  | Some v when not (V.is_empty v) -> Display_core.print d v
  | _ -> Display_core.touch d

(* A task that ended moves once no task above it runs, the root aside: a
   top task as it ends, and one started under a task that moved. *)
let rec running_above r =
  match parent r with
  | None -> false
  | Some p -> (is_running p && not (is_root p)) || running_above p

let on_live d ~layout ~keep_ended ~commit = function
  | Started _ | Progressed _ | Renamed _ -> Display_core.touch d
  | Line (r, line) ->
      let look = Display_core.look d and width = Display_core.width d in
      if is_root r then print d (Progress_row.message look ~width line)
      else if is_running r then Display_core.touch d
      else print d (Progress_row.labelled look ~width:(min width 100) r line)
  | Ended r ->
      if (not (is_root r)) && (not keep_ended) && not (running_above r) then
        moved d ~layout ~commit r
      else Display_core.touch d

let on_plain d ~keep_ended = function
  | Started r ->
      if not (transient r) then
        print d [ Progress_row.started (Display_core.look d) r ]
  | Progressed _ | Renamed _ -> ()
  | Line (r, line) ->
      let look = Display_core.look d in
      if is_root r then print d (Progress_row.message look line)
      else print d (Progress_row.labelled look r line)
  | Ended r ->
      if not (is_root r) then begin
        if not (transient r && outcome r = Some `Succeeded) then
          print d [ Progress_row.appended (Display_core.look d) r ];
        if not keep_ended then forget r
      end

(* The program's lines for the changes of tasks, in place of console's. *)
let on_own d ~keep_ended root plain = function
  | (Line (r, _) | Ended r) as change when is_root r ->
      on_plain d ~keep_ended change
  | change -> (
      (match event change ~time:(since_start (work root)) with
      | Some e -> Option.iter (Display_core.print d) (plain e)
      | None -> ());
      match change with Ended r when not keep_ended -> forget r | _ -> ())

(* The output a task kept, under its line. *)
let kept look r =
  let all = Queue.fold (fun acc l -> l :: acc) [] (output r) in
  (if dropped r > 0 then [ Progress_row.dropped look (dropped r) ] else [])
  @ List.concat_map (Progress_row.under look) (List.rev all)

let on_quiet d = function
  | Started _ | Progressed _ | Renamed _ -> ()
  | Line (r, line) ->
      let look = Display_core.look d in
      if is_root r then print d (Progress_row.message look line)
      else if not (is_running r) then
        print d (Progress_row.labelled look r line)
  | Ended r ->
      if not (is_root r) then begin
        (match outcome r with
        | Some (`Failed | `Cancelled) ->
            let look = Display_core.look d in
            print d (Progress_row.appended look r :: kept look r)
        | _ -> ());
        forget r
      end

let on_json d root change =
  (match event change ~time:(since_start (work root)) with
  | Some e -> Display_core.write d (Event.to_string e ^ "\n")
  | None -> ());
  match change with Ended r when not (is_root r) -> forget r | _ -> ()

(* What a display shows of a root in each form: the tree and the summary,
   the message the work ended with alone when only failures print, and
   nothing as events. *)
let view_of_form ~layout root = function
  | Live | Plain -> view ~layout root
  | Quiet -> verdict root
  | Json -> V.empty

let show_with ?write_error ?(progress = `Auto) ?(layout = Layout.default) ?view
    ?(keep_ended = false) ?commit ?plain ?delay ?size ppf root =
  let root = root_of root in
  let shown_size, form = shape ?size progress in
  let default = view_of_form ~layout root form in
  let shown =
    match form with
    | Live | Plain -> Option.value view ~default
    | Quiet | Json -> default
  in
  let once_width =
    match (form, size) with
    | Plain, Some given -> Some (fun () -> fst (given ()))
    | _ -> None
  in
  let d =
    Display_core.v ?size:shown_size ?once_width ?write_error ?delay
      ~fallback:default
      ~now:(Work.now (work root))
      ppf shown
  in
  Display_core.told d;
  Display_core.on_close d (fun () -> if is_running root then cancel root);
  (match form with
  | Live -> watch (work root) (on_live d ~layout ~keep_ended ~commit)
  | Plain -> (
      match plain with
      | None -> watch (work root) (on_plain d ~keep_ended)
      | Some plain -> watch (work root) (on_own d ~keep_ended root plain))
  | Quiet -> watch (work root) (on_quiet d)
  | Json ->
      Display_core.on_output d (fun s -> log root (Console_text.strip s));
      watch (work root) (on_json d root));
  d
