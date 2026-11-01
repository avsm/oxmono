(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Task_core
module V = View_core

let tasks _ = V.tasks

(* A label drawn where the tree lays it out: in its look, at its time, in
   the room its guides leave it. *)
let label f =
  V.deferred ~kind:tasks (fun c ->
      f (Ctx.look c) ~time:(Ctx.time c) (Ctx.width c))

(* A line of the tree that is not a task's, cut to its room. *)
let line f =
  label (fun look ~time:_ width ->
      let l = f look in
      match width with Some w -> Widget.truncate look w l | None -> l)

(* A transient task that succeeded has no line: the tasks under it take
   its place. *)
let shows r =
  is_running r || (not (transient r)) || outcome r <> Some `Succeeded

type node = { row : t; kids : node list }

(* The tasks [keep] keeps among [rows] and under them, each under the
   nearest of its parents kept. *)
let rec forest keep rows =
  List.concat_map
    (fun r ->
      let kids = forest keep (children r) in
      if keep r then [ { row = r; kids } ] else kids)
    rows

let rec size nodes = List.fold_left (fun n x -> n + 1 + size x.kids) 0 nodes

(* The tasks among [nodes] and under them that ended with [outcome]. *)
let rec ending outcome nodes =
  List.fold_left
    (fun n x ->
      n
      + (if Task_core.outcome x.row = Some outcome then 1 else 0)
      + ending outcome x.kids)
    0 nodes

let rec worst acc nodes =
  List.fold_left
    (fun acc n ->
      let acc : Theme.mark =
        match (outcome n.row, acc) with
        | Some `Failed, _ | _, `Failed -> `Failed
        | Some `Cancelled, _ | _, `Cancelled -> `Cancelled
        | _ -> acc
      in
      worst acc n.kids)
    acc nodes

(* How a tree is drawn: every task and window; the tasks that ended folded
   under each task; the running tasks alone; or as the scrollback keeps
   it. *)
type form = Full | Fold | Bare | Whole

(* A task keeps its row as it was last laid out, for the room, the look and
   the time, until it changes: a running row moves with the time only by
   its spinner and its timer. A top task whose tree ended keeps that tree.
   A layout with a piece of its own reads what it likes, and keeps
   nothing. *)

type key = {
  version : int;
  look : Look.t;
  layout : Progress_layout.t;
  width : int option;
  column : int;
  count_cell : int;
  rate_cell : int;
  spin : int;
  timer : string;
  form : form;
  cut : int;
  top : bool;
}

type Task_core.memo += Row of key * V.t | Tree of key * (int * int) list * V.t

let same a b =
  a.version = b.version && a.look == b.look && a.layout == b.layout
  && Option.equal Int.equal a.width b.width
  && a.column = b.column
  && a.count_cell = b.count_cell
  && a.rate_cell = b.rate_cell && a.spin = b.spin
  && String.equal a.timer b.timer
  && a.form = b.form && a.cut = b.cut && Bool.equal a.top b.top

let keeps layout look = not (Progress_layout.reads layout look).live

let key ~layout ~form ~cut ~top ~column ~time look width r =
  let running = is_running r in
  {
    version = version r;
    look;
    layout;
    width;
    column;
    count_cell = Work.count_cell (work r);
    rate_cell = Work.rate_cell (work r);
    spin = (if running then int_of_float (Float.floor (time *. 10.)) else 0);
    timer = (if running then Task_text.duration (elapsed r) else "");
    form;
    cut = (if running then cut else -1);
    top;
  }

(* What [r] keeps that [find] finds, or [lay ()], which [r] then keeps as
   [make] makes it, with one more of its kind at most: two views may show a
   task at once. *)
let kept r ~kind ~find ~make lay =
  match List.find_map find (memo r) with
  | Some v -> v
  | None ->
      let v = lay () in
      let rec one seen = function
        | [] -> []
        | m :: rest when kind m ->
            if seen then one seen rest else m :: one true rest
        | m :: rest -> m :: one seen rest
      in
      keep r (make v :: one false (memo r));
      v

let is_row = function Row _ -> true | _ -> false
let is_tree = function Tree _ -> true | _ -> false

(* With [fold], the tasks that ended among [nodes], apart. *)
let split ~fold nodes =
  if fold then List.partition (fun n -> not (is_running n.row)) nodes
  else ([], nodes)

(* The running tasks alone count no task dropped. *)
let shed form r = if form = Bare then 0 else shed r

(* The label of a node: a running task's row, with the lines of its window
   newer than [cut] in the full and folded forms, and [extra] lines; a task
   that ended, its line, with its output when whole. *)
(* The lines under a top task's row are two cells further in than the row,
   behind the tree's indent, so they have two cells less. *)
let own ~layout ~form ~cut ~top ~columns ?(extra = []) r =
  let under width =
    Option.map (fun w -> max 1 (w - if top then 2 else 0)) width
  in
  let row look ~time width lay =
    if extra <> [] || not (keeps layout look) then lay ()
    else
      let key =
        key ~layout ~form ~cut ~top ~column:columns.Progress_row.label ~time
          look width r
      in
      kept r ~kind:is_row
        ~find:(function Row (k, v) when same k key -> Some v | _ -> None)
        ~make:(fun v -> Row (key, v))
        lay
  in
  if is_running r then
    label (fun look ~time width ->
        row look ~time width @@ fun () ->
        let tail =
          match form with
          | Full | Fold ->
              List.filter_map
                (fun (seq, l) ->
                  if seq > cut then
                    Some
                      (match under width with
                      | Some w -> Widget.truncate look w l
                      | None -> l)
                  else None)
                (Progress_row.tail look r)
          | Bare | Whole -> []
        in
        V.concat ~sep:(V.text "\n")
          ((Progress_row.live look ~time ?width ~columns layout r :: tail)
          @ List.map
              (fun f ->
                let l = f look in
                match under width with
                | Some w -> Widget.truncate look w l
                | None -> l)
              extra))
  else
    label (fun look ~time width ->
        row look ~time width @@ fun () ->
        let whole = form = Whole in
        let lines = Progress_row.ended look ?width ~columns ~whole r in
        let output =
          if whole && outcome r <> Some `Succeeded then
            Progress_row.output look
              ~width:(Option.value (under width) ~default:max_int)
              r
          else []
        in
        V.concat ~sep:(V.text "\n") (lines @ output))

(* The trees of [nodes], under a task: a line that counts the tasks
   dropped, then the one that folds those that ended, come first. *)
let heads ~fold ~shed nodes =
  let ended, shown = split ~fold nodes in
  ( ((if shed > 0 then
        [ Tree.v (line (fun look -> Progress_row.shed look shed)) [] ]
      else [])
    @
    match ended with
    | [] -> []
    | ns ->
        [
          Tree.v
            (line (fun look ->
                 Progress_row.finished look ~failed:(ending `Failed ns)
                   ~cancelled:(ending `Cancelled ns) (size ns)
                   (worst `Succeeded ns)))
            [];
        ]),
    shown )

let rec block ~layout ~form ~fold ~shed ~cut ~cols ~depth nodes =
  let heads, shown = heads ~fold ~shed nodes in
  heads @ List.map (fun n -> node ~layout ~form ~cut ~cols ~depth n) shown

and node ?(top = false) ~layout ~form ~cut ~cols ~depth ?extra n =
  let r = n.row in
  Tree.v
    (own ~layout ~form ~cut ~top ~columns:(cols depth) ?extra r)
    (block ~layout ~form ~fold:(form = Fold) ~shed:(shed form r) ~cut ~cols
       ~depth:(depth + 1) n.kids)

(* The root draws no line: its tree is those of the top tasks, which fold as
   its children. A task's tree starts with its own line. *)
let tops keep r = forest keep (if is_root r then children r else [ r ])
let top_fold form r = form = Fold && is_root r
let top_shed form r = if is_root r then shed form r else 0
let indent = 2
let drawn tree = Tree.view ~indent tree

(* A running row draws a label if its layout has one, the line of a task
   that ended always. *)
let draws_label layout r =
  (not (is_running r))
  || Progress_layout.has_label (Progress_layout.for_task layout r)

let rec widen layout nodes =
  List.iter
    (fun n ->
      if draws_label layout n.row then widen_label n.row;
      widen layout n.kids)
    nodes

(* The columns of the rows of [r]'s tree, by the depth they are drawn at:
   the label column ends where the widest label drawn at any depth ends,
   behind the guides of its depth. *)
let columns ~layout look r nodes =
  widen layout nodes;
  let w = work r in
  let base = if is_root r then 1 else depth r in
  let guide = Widget.guide look None in
  let offset d =
    if d = 0 then 0
    else
      Widget.cells
        (String.make indent ' '
        ^ Widget.branch guide (List.init d (fun _ -> false)))
  in
  let offsets = Array.init (max 1 (Work.depths w - base)) offset in
  let widest = ref 0 in
  Array.iteri
    (fun d o -> widest := max !widest (o + Work.label_cell w (base + d)))
    offsets;
  let widest = !widest and lead = Progress_row.lead look in
  fun d ->
    let o = if d < Array.length offsets then offsets.(d) else offset d in
    { Progress_row.label = widest - o; lead }

let rec ended n = (not (is_running n.row)) && List.for_all ended n.kids

let rec versions acc n =
  List.fold_left versions ((id n.row, version n.row) :: acc) n.kids

(* A top task's tree, kept whole once every task in it ended. *)
let top_tree ~layout ~form ~cut ~cols n =
  let tree = drawn (node ~top:true ~layout ~form ~cut ~cols ~depth:0 n) in
  if not (ended n) then tree
  else
    let seen = versions [] n in
    V.deferred ~kind:(Progress_layout.reads layout) (fun c ->
        if not (keeps layout (Ctx.look c)) then V.resolve c tree
        else
          let key =
            key ~layout ~form ~cut ~top:true ~column:(cols 0).label ~time:0.
              (Ctx.look c) (Ctx.width c) n.row
          in
          kept n.row ~kind:is_tree
            ~find:(function
              | Tree (k, s, v) when same k key && s = seen -> Some v | _ -> None)
            ~make:(fun v -> Tree (key, seen, v))
            (fun () -> V.resolve c tree))

let trees ~layout ~form ~cut ~cols r nodes =
  let heads, shown =
    heads ~fold:(top_fold form r) ~shed:(top_shed form r) nodes
  in
  List.map drawn heads @ List.map (top_tree ~layout ~form ~cut ~cols) shown

(* The lines the trees take in the region, and how many are of windows,
   counted without drawing them. *)
let rec count_block ~form ~fold ~shed nodes =
  let ended, shown = split ~fold nodes in
  let own = (if shed > 0 then 1 else 0) + if ended = [] then 0 else 1 in
  List.fold_left
    (fun (lines, tails) n ->
      let l, w = count_node ~form n in
      (lines + l, tails + w))
    (own, 0) shown

and count_node ~form n =
  let r = n.row in
  let window = if is_running r then Queue.length (window r) else 0 in
  let lines, tails =
    count_block ~form ~fold:(form = Fold) ~shed:(shed form r) n.kids
  in
  (1 + window + lines, window + tails)

let count ~form r nodes =
  count_block ~form ~fold:(top_fold form r) ~shed:(top_shed form r) nodes

(* The seq of the newest of the [n] oldest lines of the windows of the
   running tasks, which give way. *)
let cut_tails n nodes =
  let rec seqs acc nodes =
    List.fold_left
      (fun acc x ->
        let acc =
          if is_running x.row then
            Queue.fold (fun acc (l : line) -> l.seq :: acc) acc (window x.row)
          else acc
        in
        seqs acc x.kids)
      acc nodes
  in
  match List.sort Int.compare (seqs [] nodes) with
  | [] -> -1
  | sorted ->
      let n = min n (List.length sorted) in
      if n <= 0 then -1 else List.nth sorted (n - 1)

let draw views = V.vcat views

let first c height v =
  let v = V.resolve c v in
  if V.height v <= height then v
  else
    V.concat ~sep:(V.text "\n")
      (List.filteri (fun i _ -> i < height) (V.lines v))

(* The latest running tasks that fit, after a line that counts the others,
   the tree's own task first. *)
let bare ~layout ~cols c ~height r =
  let rec collect acc nodes =
    List.fold_left
      (fun acc n ->
        let acc =
          if is_running n.row && n.row != r then n.row :: acc else acc
        in
        collect acc n.kids)
      acc nodes
  in
  let running =
    collect [] (forest shows (if is_root r then children r else [ r ]))
  in
  let top = if is_root r then 0 else 1 in
  let room = height - top and n = List.length running in
  let shown = Hashtbl.create 16 in
  let hidden =
    if n <= room then begin
      List.iter (fun x -> Hashtbl.replace shown (id x) ()) running;
      0
    end
    else
      let k = max 0 (room - 1) in
      List.sort (fun a b -> Int.compare (id b) (id a)) running
      |> List.iteri (fun i x -> if i < k then Hashtbl.replace shown (id x) ());
      n - k
  in
  let keep x = x == r || Hashtbl.mem shown (id x) in
  let nodes = tops keep r in
  let trees =
    if hidden = 0 then trees ~layout ~form:Bare ~cut:(-1) ~cols r nodes
    else if top = 1 then
      match nodes with
      | n :: rest ->
          drawn
            (node ~top:true ~layout ~form:Bare ~cut:(-1) ~cols ~depth:0
               ~extra:[ (fun look -> Progress_row.more look hidden) ]
               n)
          :: trees ~layout ~form:Bare ~cut:(-1) ~cols r rest
      | [] -> []
    else
      drawn
        (Tree.v
           (line (fun look -> V.(text "  " ++ Progress_row.more look hidden)))
           [])
      :: trees ~layout ~form:Bare ~cut:(-1) ~cols r nodes
  in
  first c height (draw trees)

(* The tree is counted before it is drawn, and drawn once, in the form that
   fits. *)
let region ~layout c ~height r =
  let width = Option.value (Ctx.width c) ~default:1 in
  if height <= 0 || width <= 0 || gone r then V.empty
  else
    let nodes = tops shows r in
    let cols = columns ~layout (Ctx.look c) r nodes in
    if fst (count ~form:Full r nodes) <= height then
      draw (trees ~layout ~form:Full ~cut:(-1) ~cols r nodes)
    else
      let n, tails = count ~form:Fold r nodes in
      if n - tails > height then bare ~layout ~cols c ~height r
      else
        let cut = cut_tails (n - height) nodes in
        draw (trees ~layout ~form:Fold ~cut ~cols r nodes)

let whole ~layout c r =
  if gone r then V.empty
  else
    let nodes = tops shows r in
    draw
      (trees ~layout ~form:Whole ~cut:(-1)
         ~cols:(columns ~layout (Ctx.look c) r nodes)
         r nodes)

let view ~layout r =
  V.deferred ~kind:(Progress_layout.reads layout) (fun c ->
      match Ctx.height c with
      | None -> whole ~layout c r
      | Some height -> region ~layout c ~height r)
