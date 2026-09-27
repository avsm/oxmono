type scope = {
  endpoint:string; account:string; mailbox_key:string; raw_name:string;
  encoding:Mailbox_name.mode; mailbox_id:string option
}
type mode = Baseline | Condstore
type phase = New | Live
type restart_reason = Uidvalidity_changed | Modseq_regressed | Nomodseq
type error = Invalid of string
type cursor = {
  schema_version:int; scope:scope; phase:phase;
  uidvalidity:Uidvalidity.t option; generation:int64; revision:int64;
  anchor:Modseq.t option; frontier:int64;
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
  uidvalidity:Uidvalidity.t; uidnext:int64;
  highestmodseq:Modseq.t option; nomodseq:bool
}
type action = {
  id:string; scope:scope; expected_revision:int64; expected_generation:int64;
  uidvalidity:Uidvalidity.t; upper_uid:int64;
  previous_anchor:Modseq.t option; mode:mode;
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
           | Some old,Some now when Modseq.compare now old < 0 ->
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
  uid:Uid.t; flags:Mail_flag.Imap_flag.t list;
  modseq:Modseq.t option
}
module Uid_map = Map.Make(Uid)
type snapshot = { validity:Uidvalidity.t; by_uid:row Uid_map.t }

let snapshot ~uidvalidity rows =
  let rec add map = function
    | [] -> Ok {validity=uidvalidity;by_uid=map}
    | row::rest ->
        let uid=row.uid in
        if Uid_map.mem uid map then Error (Invalid "duplicate UID in inventory")
        else add (Uid_map.add uid row map) rest in
  add Uid_map.empty rows
let rows snap = Uid_map.bindings snap.by_uid |> List.map snd
let snapshot_uidvalidity snap = snap.validity
