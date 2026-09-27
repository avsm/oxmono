module Flag = Mail_flag.Imap_flag
module Flags = Map.Make (struct
  type t = Flag.t
  let compare = Flag.compare
end)

type flag_delta = { add : Flag.t list; remove : Flag.t list }
type flag_plan = {
  merged : Flag.t list;
  to_remote : flag_delta;
  to_local : flag_delta;
}
type error = Deleted_flag_requires_policy

let normalize flags =
  List.fold_left (fun map flag -> match flag with
    | Flag.Recent -> map
    | _ -> Flags.add flag flag map) Flags.empty flags

let overlay left right =
  Flags.fold (fun key value map -> Flags.add key value map) right left

let delta ~target ~merged keys =
  let add,remove = Flags.fold (fun key _ (add,remove) ->
    match Flags.find_opt key target, Flags.find_opt key merged with
    | None, Some value -> value::add,remove
    | Some value, None -> add,value::remove
    | _ -> add,remove) keys ([],[]) in
  {add=List.rev add;remove=List.rev remove}

let reconcile_flags ?(propagate_deleted=false) ~base ~remote ~local () =
  let base=normalize base and remote=normalize remote and local=normalize local in
  let keys=overlay (overlay base local) remote in
  let deleted=Flag.system Flag.Deleted in
  let changed key =
    let b=Flags.mem key base in
    Flags.mem key remote<>b || Flags.mem key local<>b in
  if not propagate_deleted && changed deleted then
    Error Deleted_flag_requires_policy
  else
    let merged=Flags.fold (fun key representative merged ->
      let b=Flags.mem key base in
      let r=Flags.mem key remote and l=Flags.mem key local in
      let present=if b then r && l else r || l in
      if not present then merged
      else
        let value=match Flags.find_opt key remote,
            Flags.find_opt key local,Flags.find_opt key base with
          | Some value,_,_ | None,Some value,_ | None,None,Some value -> value
          | None,None,None -> representative in
        Flags.add key value merged) keys Flags.empty in
    Ok {merged=List.map snd (Flags.bindings merged);
        to_remote=delta ~target:remote ~merged keys;
        to_local=delta ~target:local ~merged keys}

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
type deletion_plan =
  | No_deletion
  | Hold_deletion of deletion_hold
  | Delete_remote
  | Delete_local

let absence_mature ~last_present_generation ~current_generation
    ~first_generation ~min_scans =
  let superseded=match last_present_generation,first_generation with
    | Some last,Some first -> last>=first
    | Some _,None -> true
    | None,_ -> false in
  if superseded then false
  else min_scans<=0 ||
    match first_generation with
    | Some first when first>=0L && current_generation>=first ->
        Int64.sub current_generation first>=Int64.of_int min_scans
    | _ -> false

let plan_disappearance_with_grace ~absence_mature ~policy ~paired
    ~remote_present ~remote_complete
    ~local_present ~local_complete ~local_retained
    ~survivor_unchanged =
  match remote_present,local_present with
  | true,true | false,false -> No_deletion
  | false,true | true,false as presence ->
      let missing_complete = if remote_present then local_complete
        else remote_complete in
      if not missing_complete then Hold_deletion Incomplete_inventory
      else if not paired then Hold_deletion Unpaired_identity
      else if not survivor_unchanged then Hold_deletion Survivor_changed
      else if remote_present && local_retained then
        Hold_deletion Retention_policy
      else let action=match policy,presence with
        | Preserve,_ -> Hold_deletion Preservation_policy
        | (Propagate | Propagate_remote),(false,true) -> Delete_local
        | (Propagate | Propagate_local),(true,false) -> Delete_remote
        | (Propagate_remote | Propagate_local),_ ->
            Hold_deletion Direction_policy
        | _ -> assert false in
      (match action with
       | Delete_local | Delete_remote when not absence_mature ->
           Hold_deletion Grace_period
       | _ -> action)

let plan_disappearance ~policy ~paired ~remote_present ~remote_complete
    ~local_present ~local_complete ~local_retained ~survivor_unchanged =
  plan_disappearance_with_grace ~absence_mature:true ~policy ~paired
    ~remote_present ~remote_complete ~local_present ~local_complete
    ~local_retained ~survivor_unchanged
