type t = Database.t
open Database
open Record_codec
module M = Imap.Mirror
module P = Imap.Proto
module S = Sqlite3

type tombstone_reason = Inventory_absence | Expunge_receipt
  | Local_absence | Explicit_delete | Retention
type tombstone = { reason:tombstone_reason; evidence:string;
  generation:int64 option }
type pair = { id:string; scope:M.scope;
  remote_uidvalidity:P.Uidvalidity.t option; remote_uid:P.Uid.t option;
  local_id:string option; content_sha256:string option;
  content_length:int64 option;
  internal_date:Imap.Internal_date.t option;
  common_flags:Mail_flag.Imap_flag.t list;
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
let valid_tombstone side = function
  | None -> ()
  | Some x ->
    if x.evidence="" || (match x.generation with Some n -> n<0L | None -> false) then
      invalid_arg "Imap_store.Sync.put_pair: invalid tombstone evidence";
    (match side,x.reason with
     | `Remote,(Local_absence|Retention) | `Local,(Inventory_absence|Expunge_receipt) ->
       invalid_arg "Imap_store.Sync.put_pair: tombstone side mismatch"
     | _ -> ())
let check_sync_flags where flags =
  if List.exists (function Mail_flag.Imap_flag.Recent -> true | _ -> false)
      flags then invalid_arg (where ^ ": \\Recent is ephemeral");
  let rec check = function
    | a::(b::_ as rest) ->
      if Mail_flag.Imap_flag.equal a b then
        invalid_arg (where ^ ": duplicate flag")
      else check rest
    | _ -> () in
  check (List.sort Mail_flag.Imap_flag.compare flags)
let validate_pair x =
  if x.id="" || x.revision<0L then
    invalid_arg "Imap_store.Sync.put_pair: invalid ID or revision";
  if Option.is_some x.remote_uidvalidity <> Option.is_some x.remote_uid ||
     (x.remote_uid=None && x.local_id=None) then
    invalid_arg "Imap_store.Sync.put_pair: incomplete occurrence identity";
  Option.iter (fun id -> if id="" then
    invalid_arg "Imap_store.Sync.put_pair: empty local ID") x.local_id;
  if x.remote_tombstone<>None && x.remote_uid=None then
    invalid_arg "Imap_store.Sync.put_pair: remote tombstone without UID";
  if x.local_tombstone<>None && x.local_id=None then
    invalid_arg "Imap_store.Sync.put_pair: local tombstone without ID";
  if Option.is_some x.content_sha256<>Option.is_some x.content_length then
    invalid_arg "Imap_store.Sync.put_pair: incomplete content evidence";
  Option.iter (fun hash -> if not (is_sha256_hex hash) then
      invalid_arg "Imap_store.Sync.put_pair: invalid content digest")
    x.content_sha256;
  Option.iter (fun length -> if length<0L then
    invalid_arg "Imap_store.Sync.put_pair: negative content length")
    x.content_length;
  valid_tombstone `Remote x.remote_tombstone;
  valid_tombstone `Local x.local_tombstone;
  check_sync_flags "Imap_store.Sync.put_pair" x.common_flags
let decode_scope r start : M.scope =
  {endpoint=text r.(start);account=text r.(start+1);
   mailbox_key=text r.(start+2);raw_name=text r.(start+3);
   encoding=dec_enc (text r.(start+4));
   mailbox_id=nullable_text r.(start+5)}
let pair_columns t =
  "id,endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,remote_epoch,remote_uid,local_id,revision,remote_tombstone_kind,remote_tombstone_evidence,remote_tombstone_generation,local_tombstone_kind,local_tombstone_evidence,local_tombstone_generation,content_sha256,content_length," ^
  (if t.schema_version>=10L then "internal_date" else "NULL")
let find_pair_unlocked t ~id =
  match rows t ("SELECT "^pair_columns t^" FROM sync_pairs WHERE id=?") [s id] with
  | [] -> None
  | [r] ->
    let common_flags=rows t
      "SELECT flag FROM sync_pair_flags WHERE pair_id=? ORDER BY ord" [s id]
      |> List.map (fun f -> of_checked "sync flag"
        Mail_flag.Imap_flag.of_wire (text f.(0))) in
    Some {id;scope=decode_scope r 1;
      remote_uidvalidity=Option.map validity (nullable_int r.(7));
      remote_uid=Option.map uid (nullable_int r.(8));
      local_id=nullable_text r.(9);revision=int r.(10);common_flags;
      content_sha256=nullable_text r.(17);
      content_length=nullable_int r.(18);
      internal_date=Option.map (of_checked "pair INTERNALDATE"
        Imap.Internal_date.of_string) (nullable_text r.(19));
      remote_tombstone=dec_tombstone r.(11) r.(12) r.(13);
      local_tombstone=dec_tombstone r.(14) r.(15) r.(16)}
  | _ -> fail "duplicate sync pair ID"
let find_pair t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> find_pair_unlocked t ~id)
let side_name = function `Remote -> "remote" | `Local -> "local"
let last_presence_generation t ~pair_id ~side =
  if t.schema_version<13L then None
  else transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT generation FROM sync_pair_presence WHERE pair_id=? AND side=?"
      [s pair_id;s (side_name side)] with
    | [] -> None
    | [r] -> Some (int r.(0))
    | _ -> fail "duplicate pair presence")
let note_presence t ~pair ~side ~generation =
  if generation<0L then
    invalid_arg "Imap_store.Sync.note_presence: negative generation";
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let observed=match rows t
          "SELECT generation,inventory_ref,uidvalidity FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?"
          (scope_key pair.scope) with
        | [r] -> int r.(0)=generation && nullable_text r.(1)<>None &&
            (side=`Local || nullable_int r.(2)=
              Option.map P.Uidvalidity.to_int64 pair.remote_uidvalidity)
        | _ -> false in
        if not observed then
          invalid_arg "Imap_store.Sync.note_presence: unpublished generation";
        (match side with
         | `Remote ->
             let epoch=Option.get pair.remote_uidvalidity in
             let uid=Option.get pair.remote_uid in
             if rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=?"
               (scope_key pair.scope @ [i (P.Uidvalidity.to_int64 epoch);
                 i (P.Uid.to_int64 uid)])=[] then
               invalid_arg "Imap_store.Sync.note_presence: remote UID absent"
         | `Local -> ());
        run t "INSERT INTO sync_pair_presence(pair_id,side,generation) VALUES (?,?,?) ON CONFLICT(pair_id,side) DO UPDATE SET generation=MAX(generation,excluded.generation)"
          [s pair.id;s (side_name side);i generation];
        `Recorded
    | _ -> `Stale_revision)
let reactivate_local t ~pair ~generation =
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let allowed=match pair.local_tombstone with
          | Some {reason=Local_absence;generation=first;_} ->
              (match rows t
                "SELECT generation FROM sync_pair_presence WHERE pair_id=? AND side='local'"
                [s pair.id] with
               | [r] -> let seen=int r.(0) in
                   seen=generation &&
                   (match first with Some first -> seen>=first | None -> true)
               | _ -> false)
          | _ -> false in
        let published=match rows t
          "SELECT generation,inventory_ref FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?"
          (scope_key pair.scope) with
          | [r] -> int r.(0)=generation && nullable_text r.(1)<>None
          | _ -> false in
        if not allowed || not published then
          invalid_arg "Imap_store.Sync.reactivate_local: unverified presence";
        run t "UPDATE sync_pairs SET local_tombstone_kind=NULL,local_tombstone_evidence=NULL,local_tombstone_generation=NULL,revision=? WHERE id=?"
          [i (Int64.succ pair.revision);s pair.id];
        `Reactivated {pair with local_tombstone=None;
          revision=Int64.succ pair.revision}
    | _ -> `Stale_revision)
let find_by t ~(scope:M.scope) clause values =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t ("SELECT id,raw_name,encoding,mailbox_id FROM sync_pairs WHERE endpoint=? AND account=? AND mailbox_key=? AND "^clause)
      (scope_key scope@values) with
    | [] -> None
    | [r] ->
      if text r.(1)<>scope.raw_name ||
         dec_enc (text r.(2))<>scope.encoding ||
         nullable_text r.(3)<>scope.mailbox_id then
        fail "sync pair scope mismatch";
      find_pair_unlocked t ~id:(text r.(0))
    | _ -> fail "duplicate occurrence identity")
let find_remote t ~scope ~uidvalidity ~uid =
  find_by t ~scope "remote_epoch=? AND remote_uid=?"
    [i (P.Uidvalidity.to_int64 uidvalidity);i (P.Uid.to_int64 uid)]
let find_local t ~scope ~local_id =
  find_by t ~scope "local_id=?" [s local_id]
let pairs t ~(scope:M.scope) =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    rows t "SELECT id,raw_name,encoding,mailbox_id FROM sync_pairs WHERE endpoint=? AND account=? AND mailbox_key=? ORDER BY id"
      (scope_key scope)
    |> List.map (fun r ->
      if text r.(1)<>scope.raw_name ||
         dec_enc (text r.(2))<>scope.encoding ||
         nullable_text r.(3)<>scope.mailbox_id then
        fail "sync pair scope mismatch";
      Option.get (find_pair_unlocked t ~id:(text r.(0)))))
let pairs_page t ~(scope:M.scope) ?after ~limit () =
  if limit<1 || limit>10_000 then
    invalid_arg "Imap_store.Sync.pairs_page: limit must be 1..10000";
  transaction ~begin_sql:"BEGIN" t (fun () ->
    let query="SELECT id,raw_name,encoding,mailbox_id FROM sync_pairs WHERE endpoint=? AND account=? AND mailbox_key=? "^
      (match after with None -> "" | Some _ -> "AND id>? ")^
      "ORDER BY id LIMIT ?" in
    let params=scope_key scope@
      (match after with None -> [] | Some id -> [s id])@
      [i (Int64.of_int limit)] in
    rows t query params |> List.map (fun r ->
      if text r.(1)<>scope.raw_name ||
         dec_enc (text r.(2))<>scope.encoding ||
         nullable_text r.(3)<>scope.mailbox_id then
        fail "sync pair scope mismatch";
      Option.get (find_pair_unlocked t ~id:(text r.(0)))))
let check_inventory_tombstone t x = match x.remote_tombstone with
  | Some {reason=Inventory_absence;evidence;generation=Some generation} ->
    let epoch=P.Uidvalidity.to_int64 (Option.get x.remote_uidvalidity) in
    let uid=P.Uid.to_int64 (Option.get x.remote_uid) in
    (match rows t "SELECT uidvalidity,generation,inventory_ref FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?"
      (scope_key x.scope) with
     | [r] when nullable_int r.(0)=Some epoch && int r.(1)=generation &&
       nullable_text r.(2)=Some evidence -> ()
     | _ -> invalid_arg "Imap_store.Sync.put_pair: unverified inventory tombstone");
    if rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=?"
      (scope_key x.scope@[i epoch;i uid])<>[] then
      invalid_arg "Imap_store.Sync.put_pair: UID still in published inventory"
  | Some {reason=Inventory_absence;generation=None;_} ->
    invalid_arg "Imap_store.Sync.put_pair: missing inventory generation"
  | _ -> ()
let put_pair_unlocked t ~expected_revision x =
  validate_pair x;
    let previous=find_pair_unlocked t ~id:x.id in
    let stale=match previous,expected_revision with
      | None,None -> x.revision<>0L
      | Some old,Some expected -> old.revision<>expected ||
          x.revision<>expected || old.scope<>x.scope
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
           invalid_arg "Imap_store.Sync.put_pair: occurrence identity is immutable";
         if old.remote_tombstone<>None && x.remote_tombstone=None ||
            old.local_tombstone<>None && x.local_tombstone=None then
           invalid_arg "Imap_store.Sync.put_pair: tombstone cannot be cleared"
       | None -> ());
      if (match previous with None -> true
          | Some old -> old.remote_tombstone<>x.remote_tombstone) then
        check_inventory_tombstone t x;
      let next={x with revision=Int64.succ x.revision} in
      let base=[s x.id]@scope_key x.scope@
        [s x.scope.raw_name;s (enc x.scope.encoding);ns x.scope.mailbox_id;
         ni (Option.map P.Uidvalidity.to_int64 x.remote_uidvalidity);
         ni (Option.map P.Uid.to_int64 x.remote_uid);ns x.local_id;
         i next.revision] in
      (match previous with
       | None -> run t "INSERT INTO sync_pairs VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
           (base@tombstone_columns x.remote_tombstone@
            tombstone_columns x.local_tombstone@
            [ns x.content_sha256;ni x.content_length;
             ns (Option.map Imap.Internal_date.to_string x.internal_date)])
       | Some _ -> run t "UPDATE sync_pairs SET remote_epoch=?,remote_uid=?,local_id=?,revision=?,remote_tombstone_kind=?,remote_tombstone_evidence=?,remote_tombstone_generation=?,local_tombstone_kind=?,local_tombstone_evidence=?,local_tombstone_generation=?,content_sha256=?,content_length=?,internal_date=? WHERE id=?"
           ([ni (Option.map P.Uidvalidity.to_int64 x.remote_uidvalidity);
             ni (Option.map P.Uid.to_int64 x.remote_uid);ns x.local_id;
             i next.revision]@tombstone_columns x.remote_tombstone@
             tombstone_columns x.local_tombstone@
             [ns x.content_sha256;ni x.content_length;
              ns (Option.map Imap.Internal_date.to_string x.internal_date);
              s x.id]));
      run t "DELETE FROM sync_pair_flags WHERE pair_id=?" [s x.id];
      List.iteri (fun ord flag -> run t
        "INSERT INTO sync_pair_flags VALUES (?,?,?)"
        [s x.id;i (Int64.of_int ord);
         s (Mail_flag.Imap_flag.to_wire flag)]) x.common_flags;
      `Committed next)
let put_pair t ~expected_revision x =
  transaction t (fun () -> put_pair_unlocked t ~expected_revision x)

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
    invalid_arg "Imap_store.Sync.record_conflict: invalid conflict";
  transaction t (fun () ->
    match find_pair_unlocked t ~id:x.pair_id with
    | Some pair when pair.revision=x.pair_revision ->
      run t "INSERT INTO sync_conflicts VALUES (?,?,?,?,?,0)"
        [s x.id;s x.pair_id;s (conflict_kind x.kind);s x.evidence;
         i x.pair_revision]
    | _ -> invalid_arg "Imap_store.Sync.record_conflict: stale pair")
let ensure_open_conflict t ~(pair:pair) ~kind ~id ~evidence =
  if id="" || evidence="" then
    invalid_arg "Imap_store.Sync.ensure_open_conflict: empty ID/evidence";
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let existing=rows t "SELECT id FROM sync_conflicts WHERE pair_id=? AND kind=? AND resolved=0 ORDER BY id LIMIT 1"
          [s pair.id;s (conflict_kind kind)] in
        let conflict_id=match existing with
          | [r] -> text r.(0)
          | [] -> id
          | _ -> assert false in
        (match existing with
         | [] -> run t "INSERT INTO sync_conflicts VALUES (?,?,?,?,?,0)"
             [s conflict_id;s pair.id;s (conflict_kind kind);
              s evidence;i pair.revision]
         | _ -> run t "UPDATE sync_conflicts SET evidence=?,pair_revision=? WHERE id=?"
             [s evidence;i pair.revision;s conflict_id]);
        `Open {id=conflict_id;pair_id=pair.id;kind;evidence;
          pair_revision=pair.revision;resolved=false}
    | _ -> `Stale_revision)
let resolve_open_conflicts t ~(pair:pair) ~kind =
  transaction t (fun () ->
    match find_pair_unlocked t ~id:pair.id with
    | Some current when current=pair ->
        let count=match rows t "SELECT count(*) FROM sync_conflicts WHERE pair_id=? AND kind=? AND resolved=0"
          [s pair.id;s (conflict_kind kind)] with
          | [r] -> Int64.to_int (int r.(0))
          | _ -> fail "invalid open conflict count" in
        if count>0 then run t "UPDATE sync_conflicts SET resolved=1 WHERE pair_id=? AND kind=? AND resolved=0"
          [s pair.id;s (conflict_kind kind)];
        `Resolved count
    | _ -> `Stale_revision)
let has_open_conflict t ~(pair:pair) ~kind =
  locked t (fun () ->
    rows t "SELECT 1 FROM sync_conflicts WHERE pair_id=? AND kind=? AND resolved=0 LIMIT 1"
      [s pair.id;s (conflict_kind kind)]<>[])
let resolve_conflict t ~id =
  transaction t (fun () ->
    match rows t "SELECT resolved FROM sync_conflicts WHERE id=?" [s id] with
    | [r] when int r.(0)=0L ->
      run t "UPDATE sync_conflicts SET resolved=1 WHERE id=?" [s id]
    | _ -> invalid_arg "Imap_store.Sync.resolve_conflict: unknown or resolved")
let decode_conflicts (scope:M.scope) rows =
  List.map (fun r ->
      if text r.(6)<>scope.raw_name || dec_enc (text r.(7))<>scope.encoding ||
         nullable_text r.(8)<>scope.mailbox_id then fail "sync conflict scope mismatch";
      {id=text r.(0);pair_id=text r.(1);
       kind=dec_conflict_kind (text r.(2));evidence=text r.(3);
       pair_revision=int r.(4);resolved=int r.(5)<>0L}) rows
let conflicts_query = "SELECT c.id,c.pair_id,c.kind,c.evidence,c.pair_revision,c.resolved,p.raw_name,p.encoding,p.mailbox_id FROM sync_conflicts c JOIN sync_pairs p ON p.id=c.pair_id WHERE p.endpoint=? AND p.account=? AND p.mailbox_key=? AND c.resolved=0 "
let open_conflicts t ~(scope:M.scope) =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    decode_conflicts scope
      (rows t (conflicts_query ^ "ORDER BY c.id") (scope_key scope)))
let open_conflicts_page t ~(scope:M.scope) ?after ~limit () =
  if limit<1 || limit>10_000 then
    invalid_arg "Imap_store.Sync.open_conflicts_page: limit must be 1..10000";
  transaction ~begin_sql:"BEGIN" t (fun () ->
    let query=conflicts_query ^
      (match after with None -> "" | Some _ -> "AND c.id>? ") ^
      "ORDER BY c.id LIMIT ?" in
    let params=scope_key scope @
      (match after with None -> [] | Some id -> [s id]) @
      [i (Int64.of_int limit)] in
    decode_conflicts scope (rows t query params))

type operation_kind = Append | Local_append | Copy | Move | Flags
  | Delete | Local_delete
type operation_state = Prepared | Sent | Ambiguous | Observed
  | Committed | Rejected
type operation = { id:string; pair_id:string option; local_id:string option;
  scope:M.scope;
  kind:operation_kind; state:operation_state;
  source_uidvalidity:P.Uidvalidity.t option; source_uid:P.Uid.t option;
  destination:M.scope option;
  destination_uidvalidity:P.Uidvalidity.t option;
  blob_sha256:string option; blob_length:int64 option;
  desired_flags:Mail_flag.Imap_flag.t list option;
  receipt:string option;
  receipt_uidvalidity:P.Uidvalidity.t option;
  receipt_uid:P.Uid.t option }
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
  if x.id="" || x.state<>Prepared || x.receipt<>None ||
     x.receipt_uidvalidity<>None || x.receipt_uid<>None ||
     Option.is_some x.source_uidvalidity<>Option.is_some x.source_uid ||
     (match x.pair_id with Some s -> s="" | None -> false) ||
     (match x.local_id with Some s -> s="" | None -> false) then
    invalid_arg "Imap_store.Sync.prepare_operation: invalid operation";
  Option.iter (fun n -> if n<0L then
    invalid_arg "Imap_store.Sync.prepare_operation: negative blob length")
    x.blob_length;
  Option.iter (check_sync_flags "Imap_store.Sync.prepare_operation")
    x.desired_flags;
  (match x.kind with
   | Append when x.destination=None || x.blob_sha256=None ||
                 x.blob_length=None || x.source_uid<>None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete APPEND"
   | Local_append when x.source_uid=None || x.local_id=None ||
                       x.destination<>None || x.blob_sha256=None ||
                       x.blob_length=None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete local APPEND"
   | Copy | Move when x.source_uid=None || x.destination=None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete COPY/MOVE"
   | Flags when x.source_uid=None || x.desired_flags=None ||
                x.destination<>None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete FLAGS"
   | Delete when x.source_uid=None || x.destination<>None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete DELETE"
   | Local_delete when x.pair_id=None || x.local_id=None ||
                       x.source_uid=None || x.destination<>None ->
     invalid_arg "Imap_store.Sync.prepare_operation: incomplete local DELETE"
   | _ -> ());
  if x.destination_uidvalidity<>None && x.destination=None then
    invalid_arg "Imap_store.Sync.prepare_operation: destination epoch without scope";
  if Option.is_some x.blob_sha256<>Option.is_some x.blob_length then
    invalid_arg "Imap_store.Sync.prepare_operation: incomplete content evidence";
  Option.iter (fun hash -> if not (is_sha256_hex hash) then
      invalid_arg "Imap_store.Sync.prepare_operation: invalid content digest")
    x.blob_sha256
let destination_columns = function
  | None -> [S.Data.NULL;S.Data.NULL;S.Data.NULL;S.Data.NULL;
             S.Data.NULL;S.Data.NULL]
  | Some x -> scope_key x@[s x.raw_name;s (enc x.encoding);ns x.mailbox_id]
let prepare_operation ?local_flags ?local_source_mtime
    ?source_internal_date t x =
  validate_operation x;
  (match local_flags with
   | None -> ()
   | Some flags ->
       if x.kind<>Flags || x.pair_id=None || x.local_id=None then
         invalid_arg "Imap_store.Sync.prepare_operation: local preimage requires paired FLAGS";
       check_sync_flags "Imap_store.Sync.prepare_operation" flags);
  (match local_source_mtime with
   | None -> ()
   | Some mtime when x.kind=Append && x.local_id<>None &&
       Float.is_finite mtime -> ()
   | Some _ -> invalid_arg
       "Imap_store.Sync.prepare_operation: invalid local source mtime");
  (match source_internal_date with
   | None -> ()
   | Some _ when x.kind=Local_append -> ()
   | Some _ -> invalid_arg
       "Imap_store.Sync.prepare_operation: source date requires local append");
  transaction t (fun () ->
    let pair_revision=Option.map (fun id -> match find_pair_unlocked t ~id with
      | Some pair when pair.scope=x.scope &&
          pair.local_id=x.local_id &&
          (x.source_uid=None ||
           (pair.remote_uidvalidity=x.source_uidvalidity &&
            pair.remote_uid=x.source_uid)) ->
          (match x.kind with
           | Flags | Delete | Local_delete ->
               if (x.blob_sha256<>None && x.blob_sha256<>pair.content_sha256) ||
                  (x.blob_length<>None && x.blob_length<>pair.content_length) then
                 invalid_arg "Imap_store.Sync.prepare_operation: content preimage mismatch"
           | _ -> ());
          (match x.kind,x.desired_flags with
           | (Delete | Local_delete),Some flags ->
               let normalize=List.sort_uniq Mail_flag.Imap_flag.compare in
               if normalize flags<>normalize pair.common_flags then
                 invalid_arg "Imap_store.Sync.prepare_operation: flag preimage mismatch"
           | _ -> ());
          pair.revision
      | _ -> invalid_arg "Imap_store.Sync.prepare_operation: unknown pair")
      x.pair_id in
    run t "INSERT INTO sync_operations VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      ([s x.id;ns x.pair_id;ns x.local_id]@scope_key x.scope@
       [s x.scope.raw_name;s (enc x.scope.encoding);ns x.scope.mailbox_id;
        s (operation_kind x.kind);s (operation_state x.state);
        ni (Option.map P.Uidvalidity.to_int64 x.source_uidvalidity);
        ni (Option.map P.Uid.to_int64 x.source_uid)]@
       destination_columns x.destination@
       [ni (Option.map P.Uidvalidity.to_int64 x.destination_uidvalidity);
        ni (Option.map P.Uidvalidity.to_int64 x.receipt_uidvalidity);
        ni (Option.map P.Uid.to_int64 x.receipt_uid);
        ns x.blob_sha256;ni x.blob_length;
        ni (Option.map (fun _ -> 1L) x.desired_flags);ns x.receipt]);
    Option.iter (fun flags ->
      List.iteri (fun ord flag ->
        run t "INSERT INTO sync_operation_flags VALUES (?,?,?)"
          [s x.id;i (Int64.of_int ord);
           s (Mail_flag.Imap_flag.to_wire flag)]) flags)
      x.desired_flags;
    Option.iter (fun revision ->
      run t "INSERT INTO sync_operation_preconditions VALUES (?,?)"
        [s x.id;i revision]) pair_revision;
    Option.iter (fun flags ->
      run t "INSERT INTO sync_operation_local_preimages VALUES (?)" [s x.id];
      List.iteri (fun ord flag ->
        run t "INSERT INTO sync_operation_local_preimage_flags VALUES (?,?,?)"
          [s x.id;i (Int64.of_int ord);
           s (Mail_flag.Imap_flag.to_wire flag)]) flags) local_flags;
    Option.iter (fun mtime ->
      run t "INSERT INTO sync_operation_local_sources VALUES (?,?)"
        [s x.id;S.Data.FLOAT mtime]) local_source_mtime;
    Option.iter (fun date ->
      run t "INSERT INTO sync_operation_source_dates VALUES (?,?)"
        [s x.id;s (Imap.Internal_date.to_string date)]) source_internal_date)
let local_flags_preimage t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT 1 FROM sync_operation_local_preimages WHERE operation_id=?"
      [s id] with
    | [] -> None
    | [_] -> Some (rows t
        "SELECT flag FROM sync_operation_local_preimage_flags WHERE operation_id=? ORDER BY ord"
        [s id] |> List.map (fun row -> of_checked "operation local preimage flag"
          Mail_flag.Imap_flag.of_wire (text row.(0))))
    | _ -> fail "duplicate sync operation local preimage")
let operation_pair_revision t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT pair_revision FROM sync_operation_preconditions WHERE operation_id=?"
      [s id] with
    | [] -> None
    | [r] -> Some (int r.(0))
    | _ -> fail "duplicate sync operation precondition")
let operation_source_mtime t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t "SELECT 1 FROM sqlite_master WHERE type='table' AND name='sync_operation_local_sources'" [] with
    | [] -> None
    | [_] ->
        (match rows t "SELECT mtime FROM sync_operation_local_sources WHERE operation_id=?"
           [s id] with
         | [] -> None
         | [r] ->
             (match r.(0) with
              | S.Data.FLOAT mtime when Float.is_finite mtime -> Some mtime
              | _ -> fail "invalid operation source mtime")
         | _ -> fail "duplicate operation source mtime")
    | _ -> fail "duplicate operation source table")
let operation_source_date t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if t.schema_version<11L then None
    else match rows t
      "SELECT internal_date FROM sync_operation_source_dates WHERE operation_id=?"
      [s id] with
    | [] -> None
    | [r] -> Some (of_checked "operation source INTERNALDATE"
        Imap.Internal_date.of_string (text r.(0)))
    | _ -> fail "duplicate operation source INTERNALDATE")
let operation_columns = "id,pair_id,local_id,endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,kind,state,source_epoch,source_uid,dest_endpoint,dest_account,dest_mailbox_key,dest_raw_name,dest_encoding,dest_mailbox_id,dest_epoch,receipt_epoch,receipt_uid,blob_sha256,blob_length,desired_flags_known,receipt"
let decode_operation t r =
  let destination=match r.(13) with
    | S.Data.NULL -> None
    | _ -> Some (decode_scope r 13) in
  let desired_flags=match r.(24) with
    | S.Data.NULL -> None
    | S.Data.INT 1L -> Some (rows t
        "SELECT flag FROM sync_operation_flags WHERE operation_id=? ORDER BY ord"
        [r.(0)] |> List.map (fun row -> of_checked "operation flag"
          Mail_flag.Imap_flag.of_wire (text row.(0))))
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
let find_operation_unlocked t ~id =
  match rows t ("SELECT "^operation_columns^" FROM sync_operations WHERE id=?")
    [s id] with
  | [] -> None
  | [r] -> Some (decode_operation t r)
  | _ -> fail "duplicate sync operation ID"
let find_operation t ~id =
  transaction ~begin_sql:"BEGIN" t (fun () -> find_operation_unlocked t ~id)
let active_operations t ~(scope:M.scope) =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    rows t ("SELECT "^operation_columns^" FROM sync_operations WHERE endpoint=? AND account=? AND mailbox_key=? AND state IN ('prepared','sent','ambiguous','observed') ORDER BY rowid")
      (scope_key scope)
    |> List.map (fun r ->
      let x=decode_operation t r in
      if x.scope<>scope then fail "sync operation scope mismatch";
      x))
let active_operations_page t ~(scope:M.scope) ?after ~limit () =
  if limit<1 || limit>10_000 then
    invalid_arg "Imap_store.Sync.active_operations_page: limit must be 1..10000";
  transaction ~begin_sql:"BEGIN" t (fun () ->
    let query="SELECT "^operation_columns^
      " FROM sync_operations WHERE endpoint=? AND account=? AND mailbox_key=? "^
      "AND state IN ('prepared','sent','ambiguous','observed') "^
      (match after with None -> "" | Some _ -> "AND id>? ")^
      "ORDER BY id LIMIT ?" in
    let params=scope_key scope@
      (match after with None -> [] | Some id -> [s id])@
      [i (Int64.of_int limit)] in
    rows t query params |> List.map (fun r ->
      let x=decode_operation t r in
      if x.scope<>scope then fail "sync operation scope mismatch";
      x))
let active_operation_for_pair t ~pair_id =
  transaction ~begin_sql:"BEGIN" t (fun () ->
    match rows t ("SELECT "^operation_columns^
      " FROM sync_operations WHERE pair_id=? "^
      "AND state IN ('prepared','sent','ambiguous','observed') "^
      "ORDER BY id LIMIT 1") [s pair_id] with
    | [] -> None
    | [r] -> Some (decode_operation t r)
    | _ -> fail "active pair query exceeded LIMIT 1")
let transition t ~id ~allowed ~next ~receipt ~epoch ~uid =
  transaction t (fun () ->
    match find_operation_unlocked t ~id with
    | Some x when List.mem x.state allowed ->
      run t "UPDATE sync_operations SET state=?,receipt=?,receipt_epoch=?,receipt_uid=? WHERE id=?"
        [s (operation_state next);ns receipt;
         ni (Option.map P.Uidvalidity.to_int64 epoch);
         ni (Option.map P.Uid.to_int64 uid);s id]
    | _ -> invalid_arg "Imap_store.Sync: illegal operation transition")
let mark_sent t ~id =
  transition t ~id ~allowed:[Prepared] ~next:Sent ~receipt:None
    ~epoch:None ~uid:None
let mark_ambiguous ?reason t ~id =
  Option.iter (fun reason ->
    if reason="" || String.length reason>4096 then
      invalid_arg "Imap_store.Sync.mark_ambiguous: invalid reason") reason;
  transition t ~id ~allowed:[Prepared;Sent] ~next:Ambiguous
    ~receipt:reason ~epoch:None ~uid:None
let reject_operation t ~id ~receipt =
  if receipt="" then invalid_arg "Imap_store.Sync.reject_operation: empty receipt";
  transition t ~id ~allowed:[Prepared;Sent;Ambiguous;Observed]
    ~next:Rejected ~receipt:(Some receipt) ~epoch:None ~uid:None
let reject_prepared_operation t ~id ~receipt =
  if receipt="" then
    invalid_arg "Imap_store.Sync.reject_prepared_operation: empty receipt";
  transition t ~id ~allowed:[Prepared] ~next:Rejected
    ~receipt:(Some receipt) ~epoch:None ~uid:None
let observe_operation t ~id ~receipt ~destination_uidvalidity
    ~destination_uid =
  if receipt="" ||
     (destination_uid<>None && destination_uidvalidity=None) then
    invalid_arg "Imap_store.Sync.observe_operation: invalid receipt";
  transition t ~id ~allowed:[Sent;Ambiguous] ~next:Observed
    ~receipt:(Some receipt) ~epoch:destination_uidvalidity
    ~uid:destination_uid
let commit_operation t ~id =
  transaction t (fun () ->
    match find_operation_unlocked t ~id with
    | Some x when x.state=Observed && x.pair_id=None ->
      run t "UPDATE sync_operations SET state='committed' WHERE id=?" [s id]
    | _ -> invalid_arg "Imap_store.Sync.commit_operation: operation is not unpaired and observed")
let check_operation_pair (x:operation) (pair:pair) =
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
    | Some flags ->
        let normalize=List.sort_uniq Mail_flag.Imap_flag.compare in
        normalize flags=normalize pair.common_flags in
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
    invalid_arg "Imap_store.Sync.commit_operation_with_pair: pair contradicts operation evidence"

let commit_operation_with_pair t ~id ~expected_pair_revision (pair:pair) =
  transaction t (fun () ->
    match find_operation_unlocked t ~id with
    | Some x when x.state=Observed &&
        (match x.pair_id with None -> expected_pair_revision=None
         | Some pair_id -> pair_id=pair.id) ->
      let precondition_ok=match x.pair_id,expected_pair_revision with
        | None,None -> true
        | Some _,Some expected ->
            (match rows t "SELECT pair_revision FROM sync_operation_preconditions WHERE operation_id=?" [s id] with
             | [r] -> int r.(0)=expected
             | [] -> false
             | _ -> fail "duplicate sync operation precondition")
        | _ -> false in
      let current=find_pair_unlocked t ~id:pair.id in
      let revision_ok=match current,expected_pair_revision with
        | None,None -> pair.revision=0L
        | Some old,Some expected -> old.revision=expected && pair.revision=expected
        | _ -> false in
      if not precondition_ok || not revision_ok then `Stale_revision
      else (
        check_operation_pair x pair;
        (match current,x.kind with
         | Some old,(Flags | Delete | Local_delete) ->
             let unchanged=match x.kind with
               | Flags -> {pair with common_flags=old.common_flags}
               | Delete -> {pair with remote_tombstone=old.remote_tombstone}
               | Local_delete -> {pair with local_tombstone=old.local_tombstone}
               | _ -> assert false in
             if unchanged<>old then
               invalid_arg "Imap_store.Sync.commit_operation_with_pair: unrelated pair change"
         | _ -> ());
        if x.kind=Local_append then (
          match rows t "SELECT internal_date FROM sync_operation_source_dates WHERE operation_id=?" [s id] with
          | [] -> ()
          | [r] ->
              let date=of_checked "operation source date" Imap.Internal_date.of_string (text r.(0)) in
              if not (Option.fold ~none:false
                  ~some:(Imap.Internal_date.equal_instant date) pair.internal_date) then
                invalid_arg "Imap_store.Sync.commit_operation_with_pair: source date mismatch"
          | _ -> fail "duplicate source date");
        match put_pair_unlocked t ~expected_revision:expected_pair_revision pair with
         | `Stale_revision -> `Stale_revision
         | `Committed next ->
           run t "UPDATE sync_operations SET state='committed' WHERE id=?" [s id];
           if x.kind=Flags then
             run t "UPDATE sync_conflicts SET resolved=1 WHERE pair_id=? AND kind='flags' AND resolved=0 AND NOT EXISTS (SELECT 1 FROM sync_operations WHERE pair_id=? AND kind='flags' AND state IN ('prepared','sent','ambiguous','observed'))"
               [s pair.id;s pair.id];
           `Committed next)
    | _ -> invalid_arg "Imap_store.Sync.commit_operation_with_pair: invalid operation or pair")
let settle_flag_operation t ~id (pair:pair) ~flags ~evidence =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    invalid_arg "Imap_store.Sync.settle_flag_operation: invalid evidence";
  transaction t (fun () ->
    match find_operation_unlocked t ~id,find_pair_unlocked t ~id:pair.id with
    | Some op,Some current when op.kind=Flags &&
        List.mem op.state [Sent;Ambiguous;Observed] &&
        op.pair_id=Some pair.id && op.scope=pair.scope && current=pair &&
        op.local_id=pair.local_id &&
        op.source_uidvalidity=pair.remote_uidvalidity &&
        op.source_uid=pair.remote_uid &&
        pair.remote_tombstone=None && pair.local_tombstone=None ->
        let saved=rows t
          "SELECT pair_revision FROM sync_operation_preconditions WHERE operation_id=?"
          [s id] in
        if (match saved with
            | [r] -> int r.(0)<>pair.revision
            | [] -> true
            | _ -> fail "duplicate sync operation precondition") then
          `Stale_revision
        else if rows t "SELECT 1 FROM sync_operations WHERE pair_id=? AND id<>? AND state IN ('prepared','sent','ambiguous','observed') LIMIT 1"
            [s pair.id;s id]<>[] then `Invalid_operation
        else (match put_pair_unlocked t ~expected_revision:(Some pair.revision)
            {pair with common_flags=flags} with
          | `Stale_revision -> `Stale_revision
          | `Committed next ->
              run t "UPDATE sync_operations SET state='rejected',receipt=? WHERE id=?"
                [s ("operator accepted matching endpoint flags: " ^ evidence);
                 s id];
              run t "UPDATE sync_conflicts SET resolved=1 WHERE pair_id=? AND kind='flags' AND resolved=0"
                [s pair.id];
              `Settled next)
    | Some _,Some _ -> `Invalid_operation
    | _ -> `Invalid_operation)
let reject_unchanged_delete_operation t ~id (pair:pair) ~evidence =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    invalid_arg "Imap_store.Sync.reject_unchanged_delete_operation: invalid evidence";
  transaction t (fun () ->
    match find_operation_unlocked t ~id,find_pair_unlocked t ~id:pair.id with
    | Some op,Some current when op.kind=Delete &&
        List.mem op.state [Sent;Ambiguous] &&
        op.pair_id=Some pair.id && op.scope=pair.scope && current=pair &&
        op.local_id=pair.local_id &&
        op.source_uidvalidity=pair.remote_uidvalidity &&
        op.source_uid=pair.remote_uid &&
        op.blob_sha256=pair.content_sha256 &&
        op.blob_length=pair.content_length &&
        op.desired_flags=Some (List.sort_uniq
          Mail_flag.Imap_flag.compare pair.common_flags) &&
        pair.remote_tombstone=None &&
        (match pair.local_tombstone with
         | Some {reason=Local_absence;_} -> true | _ -> false) ->
        let saved=rows t
          "SELECT pair_revision FROM sync_operation_preconditions WHERE operation_id=?"
          [s id] in
        if (match saved with
            | [r] -> int r.(0)<>pair.revision
            | [] -> true
            | _ -> fail "duplicate sync operation precondition") then
          `Stale_revision
        else if rows t "SELECT 1 FROM sync_operations WHERE pair_id=? AND id<>? AND state IN ('prepared','sent','ambiguous','observed') LIMIT 1"
            [s pair.id;s id]<>[] then `Invalid_operation
        else (
          run t "UPDATE sync_operations SET state='rejected',receipt=? WHERE id=?"
            [s ("operator verified unchanged remote target: " ^ evidence);
             s id];
          `Rejected)
    | _ -> `Invalid_operation)
let attest_targeted_expunge t ~id (pair:pair) ~evidence =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    invalid_arg "Imap_store.Sync.attest_targeted_expunge: invalid evidence";
  transaction t (fun () ->
    match find_operation_unlocked t ~id,find_pair_unlocked t ~id:pair.id with
    | Some op,Some current when op.kind=Delete &&
        List.mem op.state [Sent;Ambiguous] &&
        op.pair_id=Some pair.id && op.scope=pair.scope && current=pair &&
        op.local_id=pair.local_id &&
        op.source_uidvalidity=pair.remote_uidvalidity &&
        op.source_uid=pair.remote_uid &&
        op.blob_sha256=pair.content_sha256 &&
        op.blob_length=pair.content_length &&
        op.desired_flags=Some (List.sort_uniq
          Mail_flag.Imap_flag.compare pair.common_flags) &&
        pair.remote_tombstone=None &&
        (match pair.local_tombstone with
         | Some {reason=Local_absence;_} -> true | _ -> false) ->
        let saved=rows t
          "SELECT pair_revision FROM sync_operation_preconditions WHERE operation_id=?"
          [s id] in
        if (match saved with
            | [r] -> int r.(0)<>pair.revision
            | [] -> true
            | _ -> fail "duplicate sync operation precondition") then
          `Stale_revision
        else if rows t "SELECT 1 FROM sync_operations WHERE pair_id=? AND id<>? AND state IN ('prepared','sent','ambiguous','observed') LIMIT 1"
            [s pair.id;s id]<>[] then `Invalid_operation
        else
          let note="operator authorized targeted UID EXPUNGE: " ^ evidence in
          let receipt=match op.receipt with
            | None -> note
            | Some previous when String.length previous+
                String.length note+2<=4096 -> previous ^ "; " ^ note
            | Some _ -> "" in
          if receipt="" then `Invalid_operation
          else (
            run t "UPDATE sync_operations SET state='ambiguous',receipt=? WHERE id=?"
              [s receipt;s id];
            `Attested)
    | _ -> `Invalid_operation)
