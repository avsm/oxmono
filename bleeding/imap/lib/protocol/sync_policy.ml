module Flag = Mail_flag.Imap_flag
module Flags = Map.MakePortable (struct
  type t = Flag.t
  let compare = Flag.compare
end)

type flag_delta = { add : Flag.t list; remove : Flag.t list }
type flag_plan = {
  merged : Flag.t list;
  to_remote : flag_delta;
  to_local : flag_delta;
  deleted_held : bool;
}

let flag_map flags =
  Flags.of_list (List.map (fun flag -> flag, flag) (Flag.durable flags))

let prefer_right _ _ right = Some right

let delta ~held ~target ~merged keys =
  let add,remove = Flags.fold (fun key _ (add,remove) ->
    if held key then add,remove else
    match Flags.find_opt key target, Flags.find_opt key merged with
    | None, Some value -> value::add,remove
    | Some value, None -> add,value::remove
    | _ -> add,remove) keys ([],[]) in
  {add=List.rev add;remove=List.rev remove}

let reconcile_flags ?(propagate_deleted=false) ~base ~remote ~local () =
  let base=flag_map base and remote=flag_map remote and local=flag_map local in
  let keys=Flags.union prefer_right (Flags.union prefer_right base local)
    remote in
  let deleted=Flag.system Flag.Deleted in
  let deleted_held=not propagate_deleted &&
    Flags.mem deleted remote<>Flags.mem deleted local in
  let held key=deleted_held && Flag.equal key deleted in
  let merged=Flags.filter (fun key _ ->
    let b=Flags.mem key base in
    if held key then b
    else
      let r=Flags.mem key remote and l=Flags.mem key local in
      if b then r && l else r || l) keys in
  {merged=List.map snd (Flags.bindings merged);
   to_remote=delta ~held ~target:remote ~merged keys;
   to_local=delta ~held ~target:local ~merged keys;
   deleted_held}

type deletion_policy = Preserve | Propagate | Propagate_remote | Propagate_local
type deletion_hold =
  | Incomplete_inventory
  | Unpaired_identity
  | Survivor_changed
  | Preservation_policy
  | Direction_policy
  | Retention_policy
  | Unverified_absence
  | Grace_period
  | Missing_content_evidence
type deletion_plan =
  | No_deletion
  | Hold_deletion of deletion_hold
  | Delete_remote
  | Delete_local

let absence_mature ~last_present_generation ~current_generation
    ~first_generation ~min_scans =
  let superseded=match last_present_generation,first_generation with
    | Some last,Some first -> last>=first
    | _ -> false in
  if superseded then false
  else min_scans<=0 ||
    match first_generation with
    | Some first when first>=0L && current_generation>=first ->
        Int64.sub current_generation first>=Int64.of_int min_scans
    | _ -> false

type observation = { present : bool; complete : bool }

let plan_disappearance_with_grace ~absence_mature ~policy ~paired
    ~local_retained ~survivor_unchanged ~remote ~local =
  match remote.present,local.present with
  | true,true | false,false -> No_deletion
  | false,true | true,false ->
      let missing=if remote.present then local else remote in
      if not missing.complete then Hold_deletion Incomplete_inventory
      else if not paired then Hold_deletion Unpaired_identity
      else if not survivor_unchanged then Hold_deletion Survivor_changed
      else if remote.present && local_retained then
        Hold_deletion Retention_policy
      else let action=match policy,remote.present with
        | Preserve,_ -> Hold_deletion Preservation_policy
        | (Propagate | Propagate_remote),false -> Delete_local
        | (Propagate | Propagate_local),true -> Delete_remote
        | Propagate_remote,true | Propagate_local,false ->
            Hold_deletion Direction_policy in
      (match action with
       | Delete_local | Delete_remote when not absence_mature ->
           Hold_deletion Grace_period
       | _ -> action)
