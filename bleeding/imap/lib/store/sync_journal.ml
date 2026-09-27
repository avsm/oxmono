type t = Database.t
open Database
open Record_codec
module M = Imap.Mirror
module S = Sqlite3
module F = Mail_flag.Imap_flag

type tombstone_reason = Inventory_absence | Expunge_receipt
  | Local_absence | Explicit_delete | Retention
type tombstone = { reason:tombstone_reason; evidence:string;
  generation:int64 option }
type pair = { id:string; scope:M.scope;
  remote_uidvalidity:Imap.Uidvalidity.t option; remote_uid:Imap.Uid.t option;
  local_id:string option; content_sha256:string option;
  content_length:int64 option;
  internal_date:Imap.Internal_date.t option;
  common_flags:F.t list;
  remote_tombstone:tombstone option; local_tombstone:tombstone option;
  revision:int64 }
let reason = function
  | Inventory_absence -> "inventory_absence"
  | Expunge_receipt -> "expunge_receipt"
  | Local_absence -> "local_absence"
  | Explicit_delete -> "explicit_delete"
  | Retention -> "retention"
let dec_reason = function
  | "inventory_absence" -> Inventory_absence
  | "expunge_receipt" -> Expunge_receipt
  | "local_absence" -> Local_absence
  | "explicit_delete" -> Explicit_delete
  | "retention" -> Retention
  | _ -> fail "unknown sync tombstone reason"
let dec_tombstone kind evidence generation = match kind,evidence with
  | S.Data.NULL,S.Data.NULL -> None
  | S.Data.TEXT kind,S.Data.TEXT evidence ->
    Some {reason=dec_reason kind;evidence;generation=nullable_int generation}
  | _ -> fail "invalid sync tombstone"
let tombstone_columns = function
  | None -> [S.Data.NULL;S.Data.NULL;S.Data.NULL]
  | Some x -> [s (reason x.reason);s x.evidence;ni x.generation]
let valid_tombstone who side = function
  | None -> ()
  | Some x ->
    if x.evidence="" ||
       (match x.generation with Some n -> n<0L | None -> false) then
      invalid_arg (who ^ ": invalid tombstone evidence");
    (match side,x.reason with
     | `Remote,(Local_absence|Retention)
     | `Local,(Inventory_absence|Expunge_receipt) ->
       invalid_arg (who ^ ": tombstone side mismatch")
     | _ -> ())
let check_sync_flags where flags =
  if List.exists (function F.Recent -> true | _ -> false) flags then
    invalid_arg (where ^ ": \\Recent is ephemeral");
  let rec check = function
    | a::(b::_ as rest) ->
      if F.equal a b then invalid_arg (where ^ ": duplicate flag")
      else check rest
    | _ -> () in
  check (List.sort F.compare flags)
let validate_pair who x =
  if x.id="" || x.revision<0L then
    invalid_arg (who ^ ": invalid ID or revision");
  if Option.is_some x.remote_uidvalidity <> Option.is_some x.remote_uid ||
     (x.remote_uid=None && x.local_id=None) then
    invalid_arg (who ^ ": incomplete occurrence identity");
  Option.iter (fun id -> if id="" then
    invalid_arg (who ^ ": empty local ID")) x.local_id;
  if x.remote_tombstone<>None && x.remote_uid=None then
    invalid_arg (who ^ ": remote tombstone without UID");
  if x.local_tombstone<>None && x.local_id=None then
    invalid_arg (who ^ ": local tombstone without ID");
  if Option.is_some x.content_sha256<>Option.is_some x.content_length then
    invalid_arg (who ^ ": incomplete content evidence");
  Option.iter (fun hash -> if not (is_sha256_hex hash) then
      invalid_arg (who ^ ": invalid content digest"))
    x.content_sha256;
  Option.iter (fun length -> if length<0L then
    invalid_arg (who ^ ": negative content length"))
    x.content_length;
  valid_tombstone who `Remote x.remote_tombstone;
  valid_tombstone who `Local x.local_tombstone;
  check_sync_flags who x.common_flags
let decode_scope r start : M.scope =
  {endpoint=text r.(start);account=text r.(start+1);
   mailbox_key=text r.(start+2);raw_name=text r.(start+3);
   encoding=dec_enc (text r.(start+4));
   mailbox_id=nullable_text r.(start+5)}
let scope_where = "endpoint=? AND account=? AND mailbox_key=?"
let active_states = "state IN ('prepared','sent','ambiguous','observed')"
let page_where ?after where =
  where ^ (match after with None -> "" | Some _ -> " AND id>?") ^
  " ORDER BY id LIMIT ?"
let page_values ?after values limit =
  values @ (match after with None -> [] | Some id -> [s id]) @
  [i (Int64.of_int limit)]
let check_limit who limit =
  if limit<1 || limit>10_000 then
    invalid_arg (who ^ ": limit must be 1..10000")
let insert_flags t table id flags =
  with_stmt t ("INSERT INTO " ^ table ^ " VALUES (?,?,?)") (fun stmt ->
    List.iteri (fun ord flag -> run_prepared t stmt
      [s id;i (Int64.of_int ord);s (F.to_wire flag)]) flags)
let read_flags t what sql id =
  rows t sql [s id]
  |> List.map (fun r -> of_checked what F.of_wire (text r.(0)))

(* Pair and operation rows are read joined with their flag rows, so one
   statement decodes a whole page. The flag column follows the record. *)
let pair_columns =
  "id,endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,\
   remote_epoch,remote_uid,local_id,revision,remote_tombstone_kind,\
   remote_tombstone_evidence,remote_tombstone_generation,\
   local_tombstone_kind,local_tombstone_evidence,\
   local_tombstone_generation,content_sha256,content_length,internal_date"
let decode_pair (r,common_flags) =
  {id=text r.(0);scope=decode_scope r 1;
   remote_uidvalidity=Option.map validity (nullable_int r.(7));
   remote_uid=Option.map uid (nullable_int r.(8));
   local_id=nullable_text r.(9);revision=int r.(10);common_flags;
   content_sha256=nullable_text r.(17);
   content_length=nullable_int r.(18);
   internal_date=Option.map (of_checked "pair INTERNALDATE"
     Imap.Internal_date.of_string) (nullable_text r.(19));
   remote_tombstone=dec_tombstone r.(11) r.(12) r.(13);
   local_tombstone=dec_tombstone r.(14) r.(15) r.(16)}
let select_pairs t where values =
  rows t ("SELECT p.*,f.flag FROM (SELECT " ^ pair_columns ^
    " FROM sync_pairs WHERE " ^ where ^ ") AS p \
    LEFT JOIN sync_pair_flags AS f ON f.pair_id=p.id ORDER BY p.id,f.ord")
    values
  |> group_flags "sync flag" ~flag:20 |> List.map decode_pair
let scoped_pairs t (scope:M.scope) where values =
  select_pairs t (scope_where ^ where) (scope_key scope @ values)
  |> List.map (fun pair ->
    if pair.scope<>scope then fail "sync pair scope mismatch";
    pair)
let find_pair_unlocked t ~id =
  match select_pairs t "id=?" [s id] with [] -> None | x :: _ -> Some x
let find_pair t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> find_pair_unlocked t ~id)
let published_state t scope =
  match rows t "SELECT generation,inventory_ref,uidvalidity FROM mailboxes \
    WHERE endpoint=? AND account=? AND mailbox_key=?" (scope_key scope) with
  | [] -> None
  | r :: _ -> Some (int r.(0),nullable_text r.(1),nullable_int r.(2))
let in_snapshot t scope ~epoch ~uid =
  rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? \
    AND mailbox_key=? AND uidvalidity=? AND uid=?"
    (scope_key scope @ [i (Imap.Uidvalidity.to_int64 epoch);
      i (Imap.Uid.to_int64 uid)])<>[]
let side_name = function `Remote -> "remote" | `Local -> "local"
let last_presence_generation t ~pair_id ~side =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT generation FROM sync_pair_presence \
      WHERE pair_id=? AND side=?" [s pair_id;s (side_name side)] with
    | [] -> None
    | r :: _ -> Some (int r.(0)))
let note_presence t ~pair ~side ~generation =
  let who="Imap_store.Journal.note_presence" in
  if generation<0L then invalid_arg (who ^ ": negative generation");
  let remote=match side,pair.remote_uidvalidity,pair.remote_uid,
      pair.local_id with
    | `Remote,Some epoch,Some uid,_ -> Some (epoch,uid)
    | `Local,_,_,Some _ -> None
    | _ -> invalid_arg (who ^ ": pair has no occurrence on that side") in
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        (match published_state t pair.scope with
         | Some (published,Some _,_) when published>generation ->
             `Stale_revision
         | published ->
             let observed=match published with
               | Some (published,Some _,validity) ->
                   published=generation && (match remote with
                     | None -> true
                     | Some (epoch,_) ->
                         validity=Some (Imap.Uidvalidity.to_int64 epoch))
               | _ -> false in
             if not observed then
               invalid_arg (who ^ ": unpublished generation");
             Option.iter (fun (epoch,uid) ->
               if not (in_snapshot t pair.scope ~epoch ~uid) then
                 invalid_arg (who ^ ": remote UID absent")) remote;
             run t "INSERT INTO sync_pair_presence(pair_id,side,generation) \
               VALUES (?,?,?) ON CONFLICT(pair_id,side) DO UPDATE SET \
               generation=MAX(generation,excluded.generation)"
               [s pair.id;s (side_name side);i generation];
             `Recorded)
    | _ -> `Stale_revision)
let reactivate_local t ~pair ~generation =
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let allowed=match pair.local_tombstone with
          | Some {reason=Local_absence;generation=first;_} ->
              (match rows t "SELECT generation FROM sync_pair_presence \
                WHERE pair_id=? AND side='local'" [s pair.id] with
               | r :: _ -> let seen=int r.(0) in
                   seen=generation &&
                   (match first with Some first -> seen>=first | None -> true)
               | [] -> false)
          | _ -> false in
        let published=match published_state t pair.scope with
          | Some (published,Some _,_) -> published=generation
          | _ -> false in
        if not allowed || not published then
          invalid_arg
            "Imap_store.Journal.reactivate_local: unverified presence";
        run t "UPDATE sync_pairs SET local_tombstone_kind=NULL,\
          local_tombstone_evidence=NULL,local_tombstone_generation=NULL,\
          revision=? WHERE id=?" [i (Int64.succ pair.revision);s pair.id];
        `Reactivated {pair with local_tombstone=None;
          revision=Int64.succ pair.revision}
    | _ -> `Stale_revision)
let find_by t ~scope clause values =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match scoped_pairs t scope (" AND " ^ clause) values with
    | [] -> None
    | x :: _ -> Some x)
let find_remote t ~scope ~uidvalidity ~uid =
  find_by t ~scope "remote_epoch=? AND remote_uid=?"
    [i (Imap.Uidvalidity.to_int64 uidvalidity);i (Imap.Uid.to_int64 uid)]
let find_local t ~scope ~local_id =
  find_by t ~scope "local_id=?" [s local_id]
let pairs_page t ~scope ?after ~limit () =
  check_limit "Imap_store.Journal.pairs_page" limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    scoped_pairs t scope (page_where ?after "") (page_values ?after [] limit))
let check_inventory_tombstone t who x =
  match x.remote_tombstone,x.remote_uidvalidity,x.remote_uid with
  | Some {reason=Inventory_absence;evidence;generation=Some generation},
    Some epoch,Some uid ->
    (match published_state t x.scope with
     | Some (published,Some reference,validity)
       when validity=Some (Imap.Uidvalidity.to_int64 epoch) &&
            published=generation && reference=evidence -> ()
     | _ -> invalid_arg (who ^ ": unverified inventory tombstone"));
    if in_snapshot t x.scope ~epoch ~uid then
      invalid_arg (who ^ ": UID still in published inventory")
  | Some {reason=Inventory_absence;generation=None;_},_,_ ->
    invalid_arg (who ^ ": missing inventory generation")
  | _ -> ()
(* Absence tombstones are renewed when the side vanishes again. Allowing
   only the same or a more permanent reason keeps a deletion tombstone from
   being rewritten into a Local_absence one that reactivate_local clears. *)
let permanence = function
  | Inventory_absence | Local_absence -> 0
  | Expunge_receipt | Retention -> 1
  | Explicit_delete -> 2
let replace_tombstone who old next =
  match old,next with
  | Some _,None -> invalid_arg (who ^ ": tombstone cannot be cleared")
  | Some old,Some next when permanence next.reason<permanence old.reason ->
      invalid_arg (who ^ ": tombstone cannot become less permanent")
  | _ -> ()
let put_pair_unlocked t ~who ~previous ~expected_revision x =
  validate_pair who x;
  let stale=match previous,expected_revision with
    | None,None -> x.revision<>0L
    | Some old,Some expected ->
        if old.scope<>x.scope then invalid_arg (who ^ ": scope is immutable");
        old.revision<>expected || x.revision<>expected
    | _ -> true in
  if stale then `Stale_revision else (
    (match previous with
     | Some old ->
       let identity_changed old new_value = match old with
         | None -> false | Some _ -> old<>new_value in
       if identity_changed old.remote_uidvalidity x.remote_uidvalidity ||
          identity_changed old.remote_uid x.remote_uid ||
          identity_changed old.local_id x.local_id ||
          identity_changed old.content_sha256 x.content_sha256 ||
          identity_changed old.content_length x.content_length ||
          identity_changed old.internal_date x.internal_date then
         invalid_arg (who ^ ": occurrence identity is immutable");
       replace_tombstone who old.remote_tombstone x.remote_tombstone;
       replace_tombstone who old.local_tombstone x.local_tombstone
     | None -> ());
    if (match previous with None -> true
        | Some old -> old.remote_tombstone<>x.remote_tombstone) then
      check_inventory_tombstone t who x;
    let next={x with revision=Int64.succ x.revision} in
    run t "INSERT INTO sync_pairs (id,endpoint,account,mailbox_key,raw_name,\
      encoding,mailbox_id,remote_epoch,remote_uid,local_id,revision,\
      remote_tombstone_kind,remote_tombstone_evidence,\
      remote_tombstone_generation,local_tombstone_kind,\
      local_tombstone_evidence,local_tombstone_generation,content_sha256,\
      content_length,internal_date) \
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) \
      ON CONFLICT(id) DO UPDATE SET remote_epoch=excluded.remote_epoch,\
      remote_uid=excluded.remote_uid,local_id=excluded.local_id,\
      revision=excluded.revision,\
      remote_tombstone_kind=excluded.remote_tombstone_kind,\
      remote_tombstone_evidence=excluded.remote_tombstone_evidence,\
      remote_tombstone_generation=excluded.remote_tombstone_generation,\
      local_tombstone_kind=excluded.local_tombstone_kind,\
      local_tombstone_evidence=excluded.local_tombstone_evidence,\
      local_tombstone_generation=excluded.local_tombstone_generation,\
      content_sha256=excluded.content_sha256,\
      content_length=excluded.content_length,\
      internal_date=excluded.internal_date"
      ([s x.id]@scope_key x.scope@
       [s x.scope.raw_name;s (enc x.scope.encoding);ns x.scope.mailbox_id;
        ni (Option.map Imap.Uidvalidity.to_int64 x.remote_uidvalidity);
        ni (Option.map Imap.Uid.to_int64 x.remote_uid);ns x.local_id;
        i next.revision]@tombstone_columns x.remote_tombstone@
       tombstone_columns x.local_tombstone@
       [ns x.content_sha256;ni x.content_length;
        ns (Option.map Imap.Internal_date.to_string x.internal_date)]);
    run t "DELETE FROM sync_pair_flags WHERE pair_id=?" [s x.id];
    insert_flags t "sync_pair_flags" x.id x.common_flags;
    `Committed next)
let put_pair t ~expected_revision x =
  transaction t (fun () ->
    put_pair_unlocked t ~who:"Imap_store.Journal.put_pair"
      ~previous:(find_pair_unlocked t ~id:x.id) ~expected_revision x)

type conflict_kind = Flag_conflict | Identity_conflict | Content_conflict
  | Delete_conflict | Policy_conflict | Deletion_hold
type conflict = { id:string; pair_id:string; kind:conflict_kind;
  evidence:string; pair_revision:int64; resolved:bool }
let conflict_kind = function
  | Flag_conflict -> "flags" | Identity_conflict -> "identity"
  | Content_conflict -> "content"
  | Delete_conflict -> "delete" | Policy_conflict -> "policy"
  | Deletion_hold -> "deletion_hold"
let dec_conflict_kind = function
  | "flags" -> Flag_conflict | "identity" -> Identity_conflict
  | "content" -> Content_conflict
  | "delete" -> Delete_conflict | "policy" -> Policy_conflict
  | "deletion_hold" -> Deletion_hold
  | _ -> fail "unknown sync conflict kind"
let record_conflict t x =
  if x.id="" || x.evidence="" || x.resolved || x.pair_revision<0L then
    invalid_arg "Imap_store.Journal.record_conflict: invalid conflict";
  transaction t (fun () ->
    match find_pair_unlocked t ~id:x.pair_id with
    | Some pair when pair.revision=x.pair_revision ->
      run t "INSERT INTO sync_conflicts VALUES (?,?,?,?,?,0)"
        [s x.id;s x.pair_id;s (conflict_kind x.kind);s x.evidence;
         i x.pair_revision]
    | _ -> invalid_arg "Imap_store.Journal.record_conflict: stale pair")
let ensure_open_conflict t ~(pair:pair) ~kind ~id ~evidence =
  if id="" || evidence="" then
    invalid_arg "Imap_store.Journal.ensure_open_conflict: empty ID/evidence";
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let conflict_id=match rows t "SELECT id FROM sync_conflicts \
          WHERE pair_id=? AND kind=? AND resolved=0 ORDER BY id LIMIT 1"
          [s pair.id;s (conflict_kind kind)] with
          | r :: _ ->
              let existing=text r.(0) in
              run t "UPDATE sync_conflicts SET evidence=?,pair_revision=? \
                WHERE id=?" [s evidence;i pair.revision;s existing];
              existing
          | [] ->
              run t "INSERT INTO sync_conflicts VALUES (?,?,?,?,?,0)"
                [s id;s pair.id;s (conflict_kind kind);
                 s evidence;i pair.revision];
              id in
        `Open {id=conflict_id;pair_id=pair.id;kind;evidence;
          pair_revision=pair.revision;resolved=false}
    | _ -> `Stale_revision)
let resolve_open_conflicts t ~(pair:pair) ~kind =
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        run t "UPDATE sync_conflicts SET resolved=1 \
          WHERE pair_id=? AND kind=? AND resolved=0"
          [s pair.id;s (conflict_kind kind)];
        `Resolved (changes t)
    | _ -> `Stale_revision)
let has_open_conflict t ~(pair:pair) ~kind =
  locked t (fun () ->
    rows t "SELECT 1 FROM sync_conflicts \
      WHERE pair_id=? AND kind=? AND resolved=0 LIMIT 1"
      [s pair.id;s (conflict_kind kind)]<>[])
let resolve_conflict t ~id =
  transaction t (fun () ->
    run t "UPDATE sync_conflicts SET resolved=1 WHERE id=? AND resolved=0"
      [s id];
    if changes t=0 then
      invalid_arg "Imap_store.Journal.resolve_conflict: unknown or resolved")
let decode_conflicts (scope:M.scope) rows =
  List.map (fun r ->
      if text r.(6)<>scope.raw_name || dec_enc (text r.(7))<>scope.encoding ||
         nullable_text r.(8)<>scope.mailbox_id then
        fail "sync conflict scope mismatch";
      {id=text r.(0);pair_id=text r.(1);
       kind=dec_conflict_kind (text r.(2));evidence=text r.(3);
       pair_revision=int r.(4);resolved=int r.(5)<>0L}) rows
(* CROSS JOIN keeps the conflicts as the outer loop so the partial index
   sync_conflicts_open_id supplies ID order over open conflicts only. *)
let conflicts_query = "SELECT c.id,c.pair_id,c.kind,c.evidence,\
  c.pair_revision,c.resolved,p.raw_name,p.encoding,p.mailbox_id \
  FROM sync_conflicts AS c CROSS JOIN sync_pairs AS p ON p.id=c.pair_id \
  WHERE c.resolved=0 AND p.endpoint=? AND p.account=? AND p.mailbox_key=? "
let open_conflicts_page t ~(scope:M.scope) ?after ~limit () =
  check_limit "Imap_store.Journal.open_conflicts_page" limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    let query=conflicts_query ^
      (match after with None -> "" | Some _ -> "AND c.id>? ") ^
      "ORDER BY c.id LIMIT ?" in
    decode_conflicts scope
      (rows t query (page_values ?after (scope_key scope) limit)))

type operation_kind = Append | Local_append | Copy | Move | Flags
  | Delete | Local_delete
type operation_state = Prepared | Sent | Ambiguous | Observed
  | Committed | Rejected
type operation = { id:string; pair_id:string option; local_id:string option;
  scope:M.scope;
  kind:operation_kind; state:operation_state;
  source_uidvalidity:Imap.Uidvalidity.t option; source_uid:Imap.Uid.t option;
  destination:M.scope option;
  destination_uidvalidity:Imap.Uidvalidity.t option;
  blob_sha256:string option; blob_length:int64 option;
  desired_flags:F.t list option;
  receipt:string option;
  receipt_uidvalidity:Imap.Uidvalidity.t option;
  receipt_uid:Imap.Uid.t option }
let operation_kind = function
  | Append -> "append" | Local_append -> "local_append"
  | Copy -> "copy" | Move -> "move"
  | Flags -> "flags" | Delete -> "delete"
  | Local_delete -> "local_delete"
let dec_operation_kind = function
  | "append" -> Append | "local_append" -> Local_append
  | "copy" -> Copy | "move" -> Move
  | "flags" -> Flags | "delete" -> Delete
  | "local_delete" -> Local_delete
  | _ -> fail "unknown sync operation kind"
let operation_state = function
  | Prepared -> "prepared" | Sent -> "sent" | Ambiguous -> "ambiguous"
  | Observed -> "observed" | Committed -> "committed"
  | Rejected -> "rejected"
let dec_operation_state = function
  | "prepared" -> Prepared | "sent" -> Sent | "ambiguous" -> Ambiguous
  | "observed" -> Observed | "committed" -> Committed
  | "rejected" -> Rejected | _ -> fail "unknown sync operation state"
let validate_operation x =
  let who="Imap_store.Journal.prepare_operation" in
  if x.id="" || x.state<>Prepared || x.receipt<>None ||
     x.receipt_uidvalidity<>None || x.receipt_uid<>None ||
     Option.is_some x.source_uidvalidity<>Option.is_some x.source_uid ||
     (match x.pair_id with Some s -> s="" | None -> false) ||
     (match x.local_id with Some s -> s="" | None -> false) then
    invalid_arg (who ^ ": invalid operation");
  Option.iter (fun n -> if n<0L then
    invalid_arg (who ^ ": negative blob length"))
    x.blob_length;
  Option.iter (check_sync_flags who) x.desired_flags;
  (match x.kind with
   | Append when x.destination=None || x.blob_sha256=None ||
                 x.blob_length=None || x.source_uid<>None ->
     invalid_arg (who ^ ": incomplete APPEND")
   | Local_append when x.source_uid=None || x.local_id=None ||
                       x.destination<>None || x.blob_sha256=None ||
                       x.blob_length=None ->
     invalid_arg (who ^ ": incomplete local APPEND")
   | Copy | Move when x.source_uid=None || x.destination=None ->
     invalid_arg (who ^ ": incomplete COPY/MOVE")
   | Flags when x.source_uid=None || x.desired_flags=None ||
                x.destination<>None ->
     invalid_arg (who ^ ": incomplete FLAGS")
   | Delete when x.source_uid=None || x.destination<>None ->
     invalid_arg (who ^ ": incomplete DELETE")
   | Local_delete when x.pair_id=None || x.local_id=None ||
                       x.source_uid=None || x.destination<>None ->
     invalid_arg (who ^ ": incomplete local DELETE")
   | _ -> ());
  if x.destination_uidvalidity<>None && x.destination=None then
    invalid_arg (who ^ ": destination epoch without scope");
  if Option.is_some x.blob_sha256<>Option.is_some x.blob_length then
    invalid_arg (who ^ ": incomplete content evidence");
  Option.iter (fun hash -> if not (is_sha256_hex hash) then
      invalid_arg (who ^ ": invalid content digest"))
    x.blob_sha256
let destination_columns = function
  | None -> [S.Data.NULL;S.Data.NULL;S.Data.NULL;S.Data.NULL;
             S.Data.NULL;S.Data.NULL]
  | Some x -> scope_key x@[s x.raw_name;s (enc x.encoding);ns x.mailbox_id]
let prepare_operation ?local_flags ?local_source_mtime
    ?source_internal_date t x =
  let who="Imap_store.Journal.prepare_operation" in
  validate_operation x;
  (match local_flags with
   | None -> ()
   | Some flags ->
       if x.kind<>Flags || x.pair_id=None || x.local_id=None then
         invalid_arg (who ^ ": local preimage requires paired FLAGS");
       check_sync_flags who flags);
  (match local_source_mtime with
   | None -> ()
   | Some mtime when x.kind=Append && x.local_id<>None &&
       Float.is_finite mtime -> ()
   | Some _ -> invalid_arg (who ^ ": invalid local source mtime"));
  (match source_internal_date with
   | None -> ()
   | Some _ when x.kind=Local_append -> ()
   | Some _ -> invalid_arg (who ^ ": source date requires local append"));
  transaction t (fun () ->
    let pair_revision=Option.map (fun id -> match find_pair_unlocked t ~id with
      | Some pair when pair.scope=x.scope &&
          pair.local_id=x.local_id &&
          (x.source_uid=None ||
           (pair.remote_uidvalidity=x.source_uidvalidity &&
            pair.remote_uid=x.source_uid)) ->
          (match x.kind with
           | Flags | Delete | Local_delete ->
               if (x.blob_sha256<>None &&
                   x.blob_sha256<>pair.content_sha256) ||
                  (x.blob_length<>None &&
                   x.blob_length<>pair.content_length) then
                 invalid_arg (who ^ ": content preimage mismatch")
           | _ -> ());
          (match x.kind,x.desired_flags with
           | (Delete | Local_delete),Some flags ->
               if not (F.equal_durable flags pair.common_flags) then
                 invalid_arg (who ^ ": flag preimage mismatch")
           | _ -> ());
          pair.revision
      | _ -> invalid_arg (who ^ ": unknown pair"))
      x.pair_id in
    run t "INSERT INTO sync_operations VALUES \
      (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      ([s x.id;ns x.pair_id;ns x.local_id]@scope_key x.scope@
       [s x.scope.raw_name;s (enc x.scope.encoding);ns x.scope.mailbox_id;
        s (operation_kind x.kind);s (operation_state x.state);
        ni (Option.map Imap.Uidvalidity.to_int64 x.source_uidvalidity);
        ni (Option.map Imap.Uid.to_int64 x.source_uid)]@
       destination_columns x.destination@
       [ni (Option.map Imap.Uidvalidity.to_int64 x.destination_uidvalidity);
        ni (Option.map Imap.Uidvalidity.to_int64 x.receipt_uidvalidity);
        ni (Option.map Imap.Uid.to_int64 x.receipt_uid);
        ns x.blob_sha256;ni x.blob_length;
        ni (Option.map (fun _ -> 1L) x.desired_flags);ns x.receipt]);
    Option.iter (insert_flags t "sync_operation_flags" x.id) x.desired_flags;
    Option.iter (fun revision ->
      run t "INSERT INTO sync_operation_preconditions VALUES (?,?)"
        [s x.id;i revision]) pair_revision;
    Option.iter (fun flags ->
      run t "INSERT INTO sync_operation_local_preimages VALUES (?)" [s x.id];
      insert_flags t "sync_operation_local_preimage_flags" x.id flags)
      local_flags;
    Option.iter (fun mtime ->
      run t "INSERT INTO sync_operation_local_sources VALUES (?,?)"
        [s x.id;S.Data.FLOAT mtime]) local_source_mtime;
    Option.iter (fun date ->
      run t "INSERT INTO sync_operation_source_dates VALUES (?,?)"
        [s x.id;s (Imap.Internal_date.to_string date)]) source_internal_date)
let local_flags_preimage t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT 1 FROM sync_operation_local_preimages \
      WHERE operation_id=?" [s id] with
    | [] -> None
    | _ :: _ -> Some (read_flags t "operation local preimage flag"
        "SELECT flag FROM sync_operation_local_preimage_flags \
         WHERE operation_id=? ORDER BY ord" id))
let saved_pair_revision t id =
  match rows t "SELECT pair_revision FROM sync_operation_preconditions \
    WHERE operation_id=?" [s id] with
  | [] -> None
  | r :: _ -> Some (int r.(0))
let operation_pair_revision t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> saved_pair_revision t id)
let operation_source_mtime t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT mtime FROM sync_operation_local_sources \
      WHERE operation_id=?" [s id] with
    | [] -> None
    | r :: _ ->
        (match r.(0) with
         | S.Data.FLOAT mtime when Float.is_finite mtime -> Some mtime
         | _ -> fail "invalid operation source mtime"))
let source_date t id =
  match rows t "SELECT internal_date FROM sync_operation_source_dates \
    WHERE operation_id=?" [s id] with
  | [] -> None
  | r :: _ -> Some (of_checked "operation source INTERNALDATE"
      Imap.Internal_date.of_string (text r.(0)))
let operation_source_date t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> source_date t id)
let operation_columns = "id,pair_id,local_id,endpoint,account,mailbox_key,\
  raw_name,encoding,mailbox_id,kind,state,source_epoch,source_uid,\
  dest_endpoint,dest_account,dest_mailbox_key,dest_raw_name,dest_encoding,\
  dest_mailbox_id,dest_epoch,receipt_epoch,receipt_uid,blob_sha256,\
  blob_length,desired_flags_known,receipt"
let decode_operation (r,flags) =
  let destination=match r.(13) with
    | S.Data.NULL -> None
    | _ -> Some (decode_scope r 13) in
  let desired_flags=match r.(24) with
    | S.Data.NULL -> None
    | S.Data.INT 1L -> Some flags
    | _ -> fail "invalid operation flags marker" in
  {id=text r.(0);pair_id=nullable_text r.(1);local_id=nullable_text r.(2);
   scope=decode_scope r 3;
   kind=dec_operation_kind (text r.(9));
   state=dec_operation_state (text r.(10));
   source_uidvalidity=Option.map validity (nullable_int r.(11));
   source_uid=Option.map uid (nullable_int r.(12));destination;
   destination_uidvalidity=Option.map validity (nullable_int r.(19));
   receipt_uidvalidity=Option.map validity (nullable_int r.(20));
   receipt_uid=Option.map uid (nullable_int r.(21));
   blob_sha256=nullable_text r.(22);blob_length=nullable_int r.(23);
   desired_flags;receipt=nullable_text r.(25)}
let select_operations t where values =
  rows t ("SELECT o.*,f.flag FROM (SELECT " ^ operation_columns ^
    " FROM sync_operations WHERE " ^ where ^ ") AS o \
    LEFT JOIN sync_operation_flags AS f ON f.operation_id=o.id \
    ORDER BY o.id,f.ord") values
  |> group_flags "operation flag" ~flag:26 |> List.map decode_operation
let scoped_operations t (scope:M.scope) where values =
  select_operations t (scope_where ^ " AND " ^ where)
    (scope_key scope @ values)
  |> List.map (fun x ->
    if x.scope<>scope then fail "sync operation scope mismatch";
    x)
let find_operation_unlocked t ~id =
  match select_operations t "id=?" [s id] with [] -> None | x :: _ -> Some x
let find_operation t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> find_operation_unlocked t ~id)
let active_operations_page t ~scope ?after ~limit () =
  check_limit "Imap_store.Journal.active_operations_page" limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    scoped_operations t scope (page_where ?after active_states)
      (page_values ?after [] limit))
let active_operation_for_pair t ~pair_id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match select_operations t
      ("pair_id=? AND " ^ active_states ^ " ORDER BY id LIMIT 1")
      [s pair_id] with
    | [] -> None
    | x :: _ -> Some x)
let transition t ~id ~allowed ~next ~receipt ~epoch ~uid =
  transaction t (fun () ->
    match find_operation_unlocked t ~id with
    | Some x when List.mem x.state allowed ->
      run t "UPDATE sync_operations SET state=?,receipt=?,receipt_epoch=?,\
        receipt_uid=? WHERE id=?"
        [s (operation_state next);ns receipt;
         ni (Option.map Imap.Uidvalidity.to_int64 epoch);
         ni (Option.map Imap.Uid.to_int64 uid);s id]
    | _ -> invalid_arg "Imap_store.Journal: illegal operation transition")
let mark_sent t ~id =
  transition t ~id ~allowed:[Prepared] ~next:Sent ~receipt:None
    ~epoch:None ~uid:None
let mark_ambiguous ?reason t ~id =
  Option.iter (fun reason ->
    if reason="" || String.length reason>4096 then
      invalid_arg "Imap_store.Journal.mark_ambiguous: invalid reason") reason;
  transition t ~id ~allowed:[Prepared;Sent] ~next:Ambiguous
    ~receipt:reason ~epoch:None ~uid:None
let reject_operation t ~id ~receipt =
  if receipt="" then
    invalid_arg "Imap_store.Journal.reject_operation: empty receipt";
  transition t ~id ~allowed:[Prepared;Sent;Ambiguous;Observed]
    ~next:Rejected ~receipt:(Some receipt) ~epoch:None ~uid:None
let reject_prepared_operation t ~id ~receipt =
  if receipt="" then
    invalid_arg "Imap_store.Journal.reject_prepared_operation: empty receipt";
  transition t ~id ~allowed:[Prepared] ~next:Rejected
    ~receipt:(Some receipt) ~epoch:None ~uid:None
let observe_operation t ~id ~receipt ~destination_uidvalidity
    ~destination_uid =
  if receipt="" ||
     (destination_uid<>None && destination_uidvalidity=None) then
    invalid_arg "Imap_store.Journal.observe_operation: invalid receipt";
  transition t ~id ~allowed:[Sent;Ambiguous] ~next:Observed
    ~receipt:(Some receipt) ~epoch:destination_uidvalidity
    ~uid:destination_uid
let commit_operation t ~id =
  transaction t (fun () ->
    match find_operation_unlocked t ~id with
    | Some x when x.state=Observed && x.pair_id=None ->
      run t "UPDATE sync_operations SET state='committed' WHERE id=?" [s id]
    | _ -> invalid_arg "Imap_store.Journal.commit_operation: \
        operation is not unpaired and observed")
let check_operation_pair who (x:operation) (pair:pair) =
  let remote_identity,scope = match x.kind with
    | Append | Copy | Move ->
        ((x.receipt_uidvalidity,x.receipt_uid), x.destination)
    | Local_append | Flags | Delete | Local_delete ->
        ((x.source_uidvalidity,x.source_uid), Some x.scope) in
  let epoch,uid=remote_identity in
  let supplied_matches expected actual = match expected with
    | None -> true | Some value -> actual=Some value in
  let flags_match=match x.desired_flags with
    | None -> true
    | Some flags -> F.equal_durable flags pair.common_flags in
  let tombstones_match=match x.kind with
    | Delete -> pair.remote_tombstone<>None
    | Local_delete -> pair.local_tombstone<>None
    | Append | Local_append | Copy | Move | Flags ->
        pair.remote_tombstone=None && pair.local_tombstone=None in
  if scope<>Some pair.scope || epoch=None || uid=None ||
     pair.remote_uidvalidity<>epoch || pair.remote_uid<>uid ||
     pair.local_id<>x.local_id ||
     not (supplied_matches x.blob_sha256 pair.content_sha256) ||
     not (supplied_matches x.blob_length pair.content_length) ||
     not flags_match || not tombstones_match ||
     (match x.kind with
      | Append | Copy | Move ->
          not (supplied_matches x.destination_uidvalidity epoch)
      | _ -> false) then
    invalid_arg (who ^ ": pair contradicts operation evidence")

let commit_operation_with_pair t ~id ~expected_pair_revision (pair:pair) =
  let who="Imap_store.Journal.commit_operation_with_pair" in
  transaction t (fun () ->
    let x=match find_operation_unlocked t ~id with
      | Some x when x.state=Observed -> x
      | _ -> invalid_arg (who ^ ": invalid operation or pair") in
    (match x.pair_id,expected_pair_revision with
     | Some _,None ->
         invalid_arg (who ^ ": paired operation needs expected_pair_revision")
     | Some pair_id,Some _ when pair_id<>pair.id ->
         invalid_arg (who ^ ": invalid operation or pair")
     | None,Some _ -> invalid_arg (who ^ ": invalid operation or pair")
     | _ -> ());
    let precondition_ok=match expected_pair_revision with
      | None -> true
      | Some expected -> saved_pair_revision t id=Some expected in
    let current=find_pair_unlocked t ~id:pair.id in
    let revision_ok=match current,expected_pair_revision with
      | None,None -> pair.revision=0L
      | Some old,Some expected ->
          old.revision=expected && pair.revision=expected
      | _ -> false in
    if not precondition_ok || not revision_ok then `Stale_revision
    else (
      check_operation_pair who x pair;
      Option.iter (fun old ->
        let unchanged=match x.kind with
          | Flags -> Some {pair with common_flags=old.common_flags}
          | Delete -> Some {pair with remote_tombstone=old.remote_tombstone}
          | Local_delete ->
              Some {pair with local_tombstone=old.local_tombstone}
          | Append | Local_append | Copy | Move -> None in
        if Option.fold ~none:false ~some:(fun u -> u<>old) unchanged then
          invalid_arg (who ^ ": unrelated pair change")) current;
      if x.kind=Local_append then
        Option.iter (fun date ->
          if not (Option.fold ~none:false
              ~some:(Imap.Internal_date.equal_instant date)
              pair.internal_date) then
            invalid_arg (who ^ ": source date mismatch")) (source_date t id);
      match put_pair_unlocked t ~who ~previous:current
          ~expected_revision:expected_pair_revision pair with
       | `Stale_revision -> `Stale_revision
       | `Committed next ->
         run t "UPDATE sync_operations SET state='committed' WHERE id=?"
           [s id];
         if x.kind=Flags then
           run t ("UPDATE sync_conflicts SET resolved=1 WHERE pair_id=? \
             AND kind='flags' AND resolved=0 AND NOT EXISTS (SELECT 1 FROM \
             sync_operations WHERE pair_id=? AND kind='flags' AND " ^
             active_states ^ ")") [s pair.id;s pair.id];
         `Committed next))

let check_evidence who evidence =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    invalid_arg (who ^ ": invalid evidence")

(* A stale pair or saved precondition is [`Stale_revision]. An operation
   of the wrong kind, state or identity, or other active work on the pair,
   is [`Invalid_operation]. *)
let verify_repair t ~id ~kind ~states (pair:pair) ~matches =
  match find_operation_unlocked t ~id with
  | Some op when op.kind=kind && List.mem op.state states &&
                 op.pair_id=Some pair.id ->
      (match find_pair_unlocked t ~id:pair.id with
       | Some current when current=pair ->
           if not (op.scope=pair.scope && op.local_id=pair.local_id &&
                   op.source_uidvalidity=pair.remote_uidvalidity &&
                   op.source_uid=pair.remote_uid && matches op) then
             Error `Invalid_operation
           else if saved_pair_revision t id<>Some pair.revision then
             Error `Stale_revision
           else if rows t ("SELECT 1 FROM sync_operations WHERE pair_id=? \
               AND id<>? AND " ^ active_states ^ " LIMIT 1")
               [s pair.id;s id]<>[] then Error `Invalid_operation
           else Ok op
       | _ -> Error `Stale_revision)
  | _ -> Error `Invalid_operation

let settle_flag_operation t ~id (pair:pair) ~flags ~evidence =
  let who="Imap_store.Journal.settle_flag_operation" in
  check_evidence who evidence;
  transaction t (fun () ->
    match verify_repair t ~id ~kind:Flags ~states:[Sent;Ambiguous;Observed]
        pair ~matches:(fun _ ->
          pair.remote_tombstone=None && pair.local_tombstone=None) with
    | Error e -> e
    | Ok _ ->
        match put_pair_unlocked t ~who ~previous:(Some pair)
            ~expected_revision:(Some pair.revision)
            {pair with common_flags=flags} with
        | `Stale_revision -> `Stale_revision
        | `Committed next ->
            run t "UPDATE sync_operations SET state='rejected',receipt=? \
              WHERE id=?"
              [s ("operator accepted matching endpoint flags: " ^ evidence);
               s id];
            run t "UPDATE sync_conflicts SET resolved=1 \
              WHERE pair_id=? AND kind='flags' AND resolved=0" [s pair.id];
            `Settled next)

let unchanged_delete_target (pair:pair) (op:operation) =
  op.blob_sha256=pair.content_sha256 &&
  op.blob_length=pair.content_length &&
  (match op.desired_flags with
   | None -> true
   | Some flags -> F.equal_durable flags pair.common_flags) &&
  pair.remote_tombstone=None &&
  (match pair.local_tombstone with
   | Some {reason=Local_absence;_} -> true | _ -> false)

let reject_unchanged_delete_operation t ~id (pair:pair) ~evidence =
  check_evidence "Imap_store.Journal.reject_unchanged_delete_operation"
    evidence;
  transaction t (fun () ->
    match verify_repair t ~id ~kind:Delete ~states:[Sent;Ambiguous] pair
        ~matches:(unchanged_delete_target pair) with
    | Error e -> e
    | Ok _ ->
        run t "UPDATE sync_operations SET state='rejected',receipt=? \
          WHERE id=?"
          [s ("operator verified unchanged remote target: " ^ evidence);
           s id];
        `Rejected)

let attest_targeted_expunge t ~id (pair:pair) ~evidence =
  let who="Imap_store.Journal.attest_targeted_expunge" in
  check_evidence who evidence;
  transaction t (fun () ->
    match verify_repair t ~id ~kind:Delete ~states:[Sent;Ambiguous] pair
        ~matches:(unchanged_delete_target pair) with
    | Error e -> e
    | Ok op ->
        let note="operator authorized targeted UID EXPUNGE: " ^ evidence in
        let receipt=match op.receipt with
          | None -> note
          | Some previous -> previous ^ "; " ^ note in
        if String.length receipt>4096 then
          invalid_arg (who ^ ": receipt would exceed 4096 bytes");
        run t "UPDATE sync_operations SET state='ambiguous',receipt=? \
          WHERE id=?" [s receipt;s id];
        `Attested)
