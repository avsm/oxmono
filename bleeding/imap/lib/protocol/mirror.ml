type scope = {
  endpoint:string; account:string; mailbox_key:string; raw_name:string;
  encoding:Mailbox_name.mode; mailbox_id:string option
}
type mode = Baseline | Condstore
type phase = New | Live
type restart_reason = Uidvalidity_changed | Modseq_regressed | Nomodseq
type error =
  | Invalid of string | Stale_revision | Wrong_action
  | Incomplete_coverage | Modseq_regression
type cursor = {
  schema_version:int; scope:scope; phase:phase;
  uidvalidity:Proto.Uidvalidity.t option; generation:int64; revision:int64;
  anchor:Proto.Modseq.t option; frontier:int64;
  inventory_ref:string option; mode:mode
}

let initial scope =
  if scope.endpoint="" || scope.account="" || scope.mailbox_key="" ||
     scope.raw_name="" then invalid_arg "Imap.Mirror.initial: empty scope";
  {schema_version=1;scope;phase=New;uidvalidity=None;generation=0L;
   revision=0L;anchor=None;frontier=0L;inventory_ref=None;mode=Baseline}

let restore ~schema_version ~scope ~phase ~uidvalidity ~generation ~revision
    ~anchor ~frontier ~inventory_ref ~mode =
  let invalid why = Error (Invalid ("persisted cursor: " ^ why)) in
  if schema_version <> 1 then invalid "unsupported schema version"
  else if scope.endpoint="" || scope.account="" || scope.mailbox_key="" ||
          scope.raw_name="" then invalid "empty scope component"
  else if generation < 0L || revision < 0L || frontier < 0L ||
          frontier > 4_294_967_295L then invalid "counter outside range"
  else if generation <> revision then invalid "generation/revision mismatch"
  else if mode=Baseline && anchor<>None then invalid "baseline has MODSEQ anchor"
  else if (phase=New && (uidvalidity<>None || generation<>0L ||
           frontier<>0L || anchor<>None || inventory_ref<>None)) ||
          (phase=Live && (uidvalidity=None || generation=0L ||
           inventory_ref=None)) then invalid "phase metadata mismatch"
  else if inventory_ref=Some "" then invalid "empty inventory reference"
  else Ok {schema_version;scope;phase;uidvalidity;generation;revision;anchor;
           frontier;inventory_ref;mode}

type selected = {
  uidvalidity:Proto.Uidvalidity.t; uidnext:int64;
  highestmodseq:Proto.Modseq.t option; nomodseq:bool
}
type action = {
  id:string; scope:scope; expected_revision:int64; expected_generation:int64;
  uidvalidity:Proto.Uidvalidity.t; upper_uid:int64;
  previous_anchor:Proto.Modseq.t option; mode:mode;
  restart:restart_reason option
}

let plan (cursor:cursor) ~stage_id selected =
  if stage_id="" then Error (Invalid "empty staging identifier")
  else if selected.uidnext < 1L || selected.uidnext > 4_294_967_296L
  then Error (Invalid "UIDNEXT outside protocol range")
  else if cursor.revision=Int64.max_int || cursor.generation=Int64.max_int
  then Error (Invalid "cursor counter exhausted")
  else
    let restart =
      match cursor.uidvalidity with
      | Some old when old <> selected.uidvalidity -> Some Uidvalidity_changed
      | _ when selected.nomodseq && cursor.anchor <> None -> Some Nomodseq
      | _ ->
          (match cursor.anchor,selected.highestmodseq with
           | Some old,Some now when Proto.Modseq.to_int64 now <
                                  Proto.Modseq.to_int64 old ->
               Some Modseq_regressed
           | _ -> None) in
    let mode =
      if selected.nomodseq || selected.highestmodseq=None then Baseline
      else Condstore in
    let previous_anchor =
      if restart<>None || mode=Baseline then None else cursor.anchor in
    Ok {id=stage_id;scope=cursor.scope;expected_revision=cursor.revision;
        expected_generation=cursor.generation;
        uidvalidity=selected.uidvalidity;
        upper_uid=Int64.pred selected.uidnext;previous_anchor;mode;restart}

type row = {
  uid:Proto.Uid.t; flags:Mail_flag.Imap_flag.t list;
  modseq:Proto.Modseq.t option
}
module Uid_map = Map.Make(Int64)
type snapshot = { validity:Proto.Uidvalidity.t; by_uid:row Uid_map.t }

let snapshot ~uidvalidity rows =
  let rec add map = function
    | [] -> Ok {validity=uidvalidity;by_uid=map}
    | row::rest ->
        let uid=Proto.Uid.to_int64 row.uid in
        if Uid_map.mem uid map then Error (Invalid "duplicate UID in inventory")
        else add (Uid_map.add uid row map) rest in
  add Uid_map.empty rows
let rows snap = Uid_map.bindings snap.by_uid |> List.map snd
let snapshot_uidvalidity snap = snap.validity

type completed = {
  action_id:string; uidvalidity:Proto.Uidvalidity.t;
  covered_upper:int64; inventory_complete:bool; commands_complete:bool;
  rows:row list; explicit_highestmodseq:Proto.Modseq.t option; nomodseq:bool
}
type staged = {
  action:action; replacement:snapshot; next_anchor:Proto.Modseq.t option;
  resolved_mode:mode; resolved_restart:restart_reason option
}

let max_observed_modseq rows =
  List.fold_left (fun acc row ->
    match acc,row.modseq with
    | None,x | x,None -> x
    | Some a,Some b ->
        Some (if Proto.Modseq.to_int64 a >= Proto.Modseq.to_int64 b
              then a else b)) None rows

let complete (cursor:cursor) (action:action) done_ =
  if cursor.scope<>action.scope then Error Wrong_action
  else if cursor.revision<>action.expected_revision ||
     cursor.generation<>action.expected_generation
  then Error Stale_revision
  else if done_.action_id<>action.id ||
          done_.uidvalidity<>action.uidvalidity
  then Error Wrong_action
  else if not done_.inventory_complete || not done_.commands_complete ||
          done_.covered_upper<>action.upper_uid
  then Error Incomplete_coverage
  else if List.exists (fun row ->
    Proto.Uid.to_int64 row.uid > action.upper_uid) done_.rows
  then Error (Invalid "inventory row above fixed upper UID bound")
  else
    match snapshot ~uidvalidity:action.uidvalidity done_.rows with
    | Error _ as e -> e
    | Ok replacement ->
        let resolved_mode=if done_.nomodseq then Baseline else action.mode in
        let resolved_restart=
          if done_.nomodseq && action.mode=Condstore then Some Nomodseq
          else action.restart in
        let next_anchor =
          if resolved_mode=Baseline then None
          else match done_.explicit_highestmodseq with
            | Some explicit -> Some explicit
            | None when List.for_all (fun row -> row.modseq<>None) done_.rows ->
                max_observed_modseq done_.rows
            | None -> None in
        (match action.previous_anchor,next_anchor with
         | Some old,Some now when Proto.Modseq.to_int64 now <
                                  Proto.Modseq.to_int64 old ->
             Error Modseq_regression
         | _ -> Ok {action;replacement;next_anchor;
                    resolved_mode;resolved_restart})

type flag_change = {before:row;after:row}
type transition = {
  cursor:cursor; snapshot:snapshot; added:row list;
  changed:flag_change list; removed:Proto.Uid.t list;
  invalidated_epoch:bool; restart:restart_reason option;
  stage_id:string; more:bool
}

let flags_equal a b =
  let normalize = List.sort_uniq Mail_flag.Imap_flag.compare in
  let a=normalize a and b=normalize b in
  List.length a=List.length b &&
  List.for_all2 Mail_flag.Imap_flag.equal a b

let publish (cursor:cursor) ~published staged =
  let action=staged.action in
  if cursor.scope<>action.scope then Error Wrong_action
  else if cursor.revision<>action.expected_revision ||
     cursor.generation<>action.expected_generation
  then Error Stale_revision
  else
    let old_validity =
      match published with None -> None | Some snap -> Some snap.validity in
    if old_validity<>cursor.uidvalidity then
      Error (Invalid "published snapshot does not match cursor epoch")
    else
      let epoch_changed =
        match old_validity with
        | Some old -> old<>action.uidvalidity
        | None -> false in
      let old =
        if epoch_changed then Uid_map.empty
        else match published with None -> Uid_map.empty
          | Some snap -> snap.by_uid in
      let added =
        Uid_map.fold (fun uid row acc ->
          if Uid_map.mem uid old then acc else row::acc)
          staged.replacement.by_uid [] |> List.rev in
      let changed =
        Uid_map.fold (fun uid row acc ->
          match Uid_map.find_opt uid old with
          | Some before when not (flags_equal before.flags row.flags) ->
              {before;after=row}::acc
          | _ -> acc) staged.replacement.by_uid [] |> List.rev in
      let removed =
        if epoch_changed then [] else
        Uid_map.fold (fun uid row acc ->
          if Uid_map.mem uid staged.replacement.by_uid then acc
          else row.uid::acc) old [] |> List.rev in
      let cursor =
        {cursor with phase=Live;uidvalidity=Some action.uidvalidity;
         generation=Int64.succ cursor.generation;
         revision=Int64.succ cursor.revision;
         anchor=staged.next_anchor;frontier=action.upper_uid;
         inventory_ref=Some action.id;mode=staged.resolved_mode} in
      Ok {cursor;snapshot=staged.replacement;added;changed;removed;
          invalidated_epoch=epoch_changed;restart=staged.resolved_restart;
          stage_id=action.id;more=false}
