(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind =
  | Spinner
  | Mark
  | Label of Style.t
  | Bar of int option
  | Percent
  | Count
  | Rate
  | Eta
  | Elapsed
  | Spacer
  | Const of View_core.t
  | Custom of (Task_core.t -> View_core.t)
  | Choose of (Task_core.t -> piece list)

and piece = { kind : kind; stage : int option; blank : bool }

type t = piece list

let pieces t = t

let rec reads_in look k = function
  | [] -> k
  | { kind = Custom _ | Choose _; _ } :: rest ->
      reads_in look (View_core.join k View_core.moving) rest
  | { kind = Const v; _ } :: rest ->
      reads_in look (View_core.join k (View_core.kind v look)) rest
  | _ :: rest -> reads_in look k rest

(* A frame asks what its view reads: a layout whose pieces read nothing but
   the tasks, the default one, answers at once. *)
let reads t =
  let own p =
    match p.kind with
    | Custom _ | Choose _ -> true
    | Const v -> not (View_core.is_flat v)
    | _ -> false
  in
  if List.exists own t then fun look -> reads_in look View_core.tasks t
  else fun _ -> View_core.tasks

let one ?stage kind =
  let blank =
    match kind with
    | Const v -> String.trim (View_core.to_string v) = ""
    | _ -> false
  in
  [ { kind; stage; blank } ]

(* What gives way, and when: the rate and the time left first, then the
   bar and the percentage; the count gets shorter at the third stage and
   the elapsed time goes at the fourth. *)
let spinner = one Spinner
let label ?(style = Style.bold) () = one (Label style)

let bar ?width () =
  (match width with
  | Some n when n < 0 ->
      invalid_arg "Console.Progress.Layout.bar: negative width"
  | _ -> ());
  one ~stage:2 (Bar width)

let percent = one ~stage:2 Percent
let count = one Count
let rate = one ~stage:1 Rate
let eta = one ~stage:1 Eta
let elapsed = one ~stage:4 Elapsed
let spacer = one Spacer
let const v = one (Const v)
let custom f = one (Custom f)
let choose f = one (Choose f)

let give_way n t =
  if n < 1 then invalid_arg "Console.Progress.Layout.give_way: stage below 1";
  List.map (fun p -> { p with stage = Some n }) t

let is_label p = match p.kind with Label _ -> true | _ -> false
let has_label pieces = List.exists is_label pieces
let is_spacer p = match p.kind with Spacer -> true | _ -> false

(* A row has one flexible piece of each kind: the label that gives way and
   the spacer that takes the room left. *)
let checked t =
  let count is = List.length (List.filter is t) in
  if count is_label > 1 then
    invalid_arg "Console.Progress.Layout: more than one label";
  if count is_spacer > 1 then
    invalid_arg "Console.Progress.Layout: more than one spacer";
  t

let ( ++ ) a b = checked (a @ b)

(* A chosen layout takes the stage at which the choice gives way. *)
let rec for_task t r =
  checked
    (List.concat_map
       (fun p ->
         match p.kind with
         | Choose f ->
             let chosen = for_task (f r) r in
             if p.stage = None then chosen
             else List.map (fun q -> { q with stage = p.stage }) chosen
         | _ -> [ p ])
       t)

let list ?sep ts =
  let rec go = function
    | [] -> []
    | [ t ] -> t
    | t :: rest -> t @ Option.value sep ~default:[] @ go rest
  in
  checked (go ts)

let space = const (View_core.text " ")

let packed lead =
  list
    [
      lead;
      space;
      label ();
      space;
      bar ();
      space;
      count;
      space;
      rate;
      space;
      elapsed;
    ]

let default = packed spinner
let ended = packed (one Mark)

let pp_kind ppf = function
  | Spinner -> Fmt.string ppf "spinner"
  | Mark -> Fmt.string ppf "mark"
  | Label _ -> Fmt.string ppf "label"
  | Bar None -> Fmt.string ppf "bar"
  | Bar (Some n) -> Fmt.pf ppf "bar %d" n
  | Percent -> Fmt.string ppf "percent"
  | Count -> Fmt.string ppf "count"
  | Rate -> Fmt.string ppf "rate"
  | Eta -> Fmt.string ppf "eta"
  | Elapsed -> Fmt.string ppf "elapsed"
  | Spacer -> Fmt.string ppf "spacer"
  | Const v -> Fmt.pf ppf "const %S" (View_core.to_string v)
  | Custom _ -> Fmt.string ppf "custom"
  | Choose _ -> Fmt.string ppf "choose"

let pp = Fmt.Dump.list (fun ppf p -> pp_kind ppf p.kind)
