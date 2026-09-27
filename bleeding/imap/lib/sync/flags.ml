module J = Imap_store.Journal
module F = Mail_flag.Imap_flag
module E = Pair_evidence

open Error

type outcome = Unchanged | Updated of J.pair
type plan = No_change | Apply of F.t list
type decision = { plan : plan; deleted_held : bool }
type reconciled = { outcome : outcome; deleted_held : bool }

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok x -> Ok x | Error e -> Error (Client e)
let flags = F.durable
let same = F.equal_durable

let deleted = F.system F.Deleted
let has_deleted flags = List.exists (F.equal deleted) flags

(* An operation whose target agrees with the base on \Deleted never writes
   it: each endpoint keeps its own \Deleted state, which is how a held
   \Deleted change survives while the other flags merge. *)
let target ~base ~desired observed =
  if has_deleted desired<>has_deleted base ||
     has_deleted observed=has_deleted desired then desired
  else if has_deleted observed then flags (deleted :: desired)
  else List.filter (fun flag -> not (F.equal deleted flag)) desired

let plan_flags ?(propagate_deleted=false) ~base ~remote ~local
    ~condstore ~remote_modseq () =
  let policy=Imap.Sync_policy.reconcile_flags ~propagate_deleted ~base
    ~remote ~local () in
  let merged=flags policy.merged in
  let remote_target=target ~base ~desired:merged remote in
  let local_target=target ~base ~desired:merged local in
  let deleted_held=policy.deleted_held in
  if same remote remote_target && same local local_target &&
     same base merged then
    Ok {plan=No_change;deleted_held}
  else if not (same remote remote_target) &&
    (not condstore ||
     (match remote_modseq with Some n when n>0L -> false | _ -> true)) then
    Error Conditional_store_unavailable
  else Ok {plan=Apply merged;deleted_held}

let validate_permanent_flags ~available ~defined ~remote ~merged =
  match available with
  | None -> Ok ()
  | Some raw ->
      let rec collect known wildcard = function
        | [] -> Ok (known,wildcard)
        | "\\*" :: rest -> collect known true rest
        | wire :: rest ->
            (match F.of_wire wire with
             | Ok flag -> collect (flag::known) wildcard rest
             | Error _ -> Error (Diverged
                 "SELECT returned malformed PERMANENTFLAGS")) in
      let* permanent,wildcard=collect [] false raw in
      let defined=List.filter_map (fun wire ->
        Result.to_option (F.of_wire wire))
        (Option.value ~default:[] defined) in
      let new_keyword = function
        | F.Keyword _ as flag -> not (List.exists (F.equal flag) defined)
        | _ -> false in
      let permitted ~adding flag =
        List.exists (F.equal flag) permanent ||
        (adding && wildcard && new_keyword flag) in
      let find_bad ~adding from other =
        List.find_opt (fun flag ->
          not (List.exists (F.equal flag) other) &&
          not (permitted ~adding flag)) from in
      match find_bad ~adding:true merged remote with
      | Some flag -> Error (Permanent_flag_unavailable flag)
      | None ->
          (match find_bad ~adding:false remote merged with
           | Some flag -> Error (Permanent_flag_unavailable flag)
           | None -> Ok ())

let check_epoch (info:Imap.Response.select_metadata) epoch =
  if info.uidvalidity<>Imap.Uidvalidity.to_int64 epoch then
    Error Uidvalidity_changed
  else Ok ()

let remote selected ~uid ~modseq =
  let* found=E.remote_flags selected ~uid ~modseq in
  match found with
  | Some flags -> Ok flags
  | None -> Error Missing_occurrence

let with_selected (ctx:Ctx.t) ~mode ~epoch f =
  match Imap_eio.Client.with_mailbox ctx.client ~mode ctx.mailbox
      (fun selected ->
    Ok (let* info=network (Imap_eio.Selected.info selected) in
        let* ()=check_epoch info epoch in
        f selected info)) with
  | Error e -> Error (Client e)
  | Ok result -> result

let kind_name = function
  | J.Append -> "APPEND" | J.Local_append -> "local APPEND"
  | J.Copy -> "COPY" | J.Move -> "MOVE" | J.Flags -> "FLAGS"
  | J.Delete -> "DELETE" | J.Local_delete -> "local DELETE"

let intent (pair:J.pair) ~id ~epoch ~uid ~local_id ~merged : J.operation = {
  id;pair_id=Some pair.id;local_id=Some local_id;scope=pair.scope;
  kind=J.Flags;state=J.Prepared;
  source_uidvalidity=Some epoch;source_uid=Some uid;
  destination=None;destination_uidvalidity=None;
  blob_sha256=None;blob_length=None;desired_flags=Some merged;
  receipt=None;receipt_uidvalidity=None;receipt_uid=None}

let commit store (pair:J.pair) ~id ~merged =
  let next={pair with common_flags=flags merged} in
  match J.commit_operation_with_pair store ~id
    ~expected_pair_revision:(Some pair.revision) next with
  | `Committed pair -> Ok (Updated pair)
  | `Stale_revision -> Error Stale_pair

let flag_conflict store (pair:J.pair) ~id evidence =
  match J.ensure_open_conflict store ~pair ~kind:J.Flag_conflict
    ~id:(id ^ "-flag-conflict") ~evidence with
  | `Open _ -> Ok ()
  | `Stale_revision -> Error Stale_pair

let pending store pair ~id evidence =
  let* ()=flag_conflict store pair ~id evidence in
  Error (Pending_operations [id])

let hold_content store (pair:J.pair) ~id =
  match J.ensure_open_conflict store ~pair ~kind:J.Content_conflict
    ~id ~evidence:"local message content differs from the paired digest" with
  | `Open _ -> Error (Content_mismatch pair.id)
  | `Stale_revision -> Error Stale_pair

let clear_content_hold store (pair:J.pair) =
  match J.resolve_open_conflicts store ~pair ~kind:J.Content_conflict with
  | `Resolved _ -> Ok ()
  | `Stale_revision -> Error Stale_pair

let recover_sent ~inventory ~(ctx:Ctx.t) ~writer ~(operation:J.operation)
    ~merged =
  let store=ctx.store in
  let maildir=Maildir.of_writer writer in
  let* pair=E.operation_pair store operation in
  let* epoch,uid,local_id=E.live_target pair in
  if not (E.same_target operation ~epoch ~uid ~local_id &&
          E.journaled_at store operation pair) then
    Error Stale_pair
  else
    let base=pair.common_flags in
    let pending=pending store pair ~id:operation.id in
    let* staged=E.occurrence ?inventory maildir local_id in
    let* local_before,state=
      match E.local_content ?inventory maildir pair staged with
      | `Changed ->
          let* fresh=E.occurrence maildir local_id in
          Ok (fresh,E.local_content maildir pair fresh)
      | state -> Ok (staged,state) in
    let* ()=if state=`Matches then Ok ()
      else pending
        "uncertain FLAGS write: local message content differs from the paired \
         digest" in
    let* remote_flags,_=E.remote_flags_now ctx ~epoch ~uid ~modseq:false in
    if not (same remote_flags (target ~base ~desired:merged remote_flags)) then
      pending
        "uncertain FLAGS write: remote flags differ from the journaled target"
    else
      let local_target=target ~base ~desired:merged local_before.flags in
      let* local_after=
        if same local_before.flags local_target then
          if E.unchanged maildir local_before then Ok local_before
          else pending
            "uncertain FLAGS write: local message changed during recovery"
        else match J.local_flags_preimage store ~id:operation.id with
          | Some before when same local_before.flags before ->
              (* A sent/ambiguous STORE may have reached the server.  Its
                 verified target and the unchanged Maildir preimage are
                 enough to finish locally, without replaying STORE. *)
              let* _=E.current_pair store pair in
              (match Maildir.set_flags writer local_before local_target
               with
               | exception Maildir.Stale_occurrence ->
                   pending
                     "uncertain FLAGS write: local message changed during \
                      recovery"
               | Error error -> Error (Maildir error)
               | Ok written
                 when E.local_content maildir pair written=`Matches ->
                   Ok written
               | Ok _ -> pending
                   "uncertain FLAGS write: local message content changed \
                    during recovery")
          | _ -> pending
              "uncertain FLAGS write: local flags differ from the saved \
               preimage" in
      if not (same local_after.flags local_target) then
        pending "uncertain FLAGS write: local flags did not reach the target"
      else
        let* remote_after,_=E.remote_flags_now ctx ~epoch ~uid
          ~modseq:false in
        if not (same remote_after (target ~base ~desired:merged remote_after))
        then pending
          "uncertain FLAGS write: remote flags changed during verification"
        else (
          if operation.state<>J.Observed then
            J.observe_operation store ~id:operation.id
              ~receipt:"verified FLAGS on both endpoints"
              ~destination_uidvalidity:None ~destination_uid:None;
          commit store pair ~id:operation.id ~merged)

let recover_operation ?inventory ~(ctx:Ctx.t) ~writer
    ~(operation:J.operation) () =
  let store=ctx.store in
  match operation.kind,operation.state with
  | kind,_ when kind<>J.Flags -> Error (Diverged "operation is not FLAGS")
  | _,J.Prepared ->
      J.reject_prepared_operation store ~id:operation.id
        ~receipt:"prepared FLAGS was not dispatched";
      Ok Unchanged
  | _,(J.Committed | J.Rejected) -> Error (Diverged "operation is terminal")
  | _,(J.Sent | J.Ambiguous | J.Observed) ->
      match operation.pair_id,operation.desired_flags with
      | Some _,Some merged ->
          recover_sent ~inventory ~ctx ~writer ~operation ~merged
      | _ -> Error (Diverged "FLAGS operation has no pair or target flags")

let advance_baseline store (pair:J.pair) ~merged =
  match J.put_pair store ~expected_revision:(Some pair.revision)
    {pair with common_flags=merged} with
  | `Committed pair -> Ok (Updated pair)
  | `Stale_revision -> Error Stale_pair

let reconcile_pair ?(propagate_deleted=false) ?inventory ~(ctx:Ctx.t)
    ~writer ~(pair:J.pair) () =
  let {Ctx.client;store;next_id;_}=ctx in
  let maildir=Maildir.of_writer writer in
  let* pair=E.current_pair store pair in
  let* epoch,uid,local_id=E.live_target pair in
  match J.active_operation_for_pair store ~pair_id:pair.id with
  | Some op when op.kind=J.Flags -> Error (Pending_operations [op.id])
  | Some op -> Error (Diverged (Printf.sprintf
      "pair has a pending %s operation %s" (kind_name op.kind) op.id))
  | None ->
      let* local_before=E.occurrence ?inventory maildir local_id in
      let* ()=match E.local_content ?inventory maildir pair local_before with
        | `Matches -> clear_content_hold store pair
        | `Differs -> hold_content store pair ~id:(next_id ())
        | `Changed -> Error Modified in
      let has=Imap_eio.Client.has client in
      let condstore=has Imap.Capability.Condstore ||
        has Imap.Capability.Qresync in
      let base=pair.common_flags in
      let apply selected (info:Imap.Response.select_metadata)
          ~remote_before ~remote_modseq ~merged =
        let remote_target=target ~base ~desired:merged remote_before in
        let local_target=target ~base ~desired:merged local_before.flags in
        let remote_needed=not (same remote_before remote_target) in
        let local_needed=not (same local_before.flags local_target) in
        if not remote_needed && not local_needed then
          advance_baseline store pair ~merged
        else
        let* ()=if remote_needed then
            validate_permanent_flags ~available:info.permanentflags
              ~defined:info.flags ~remote:remote_before ~merged:remote_target
          else Ok () in
        let id=next_id () in
        let pending=pending store pair ~id in
        let unsent receipt error =
          J.reject_prepared_operation store ~id ~receipt;
          Error error in
        J.prepare_operation ~local_flags:local_before.flags store
          (intent pair ~id ~epoch ~uid ~local_id ~merged);
        let write_local ~stale =
          if not local_needed then
            if E.unchanged ?inventory maildir local_before then Ok local_before
            else stale ()
          else match Maildir.set_flags writer local_before local_target
          with
          | exception Maildir.Stale_occurrence -> stale ()
          | Error error -> Error (Maildir error)
          | Ok written
            when E.local_content maildir pair written=`Matches -> Ok written
          | Ok _ -> pending
              "local message content changed during the FLAGS update" in
        let finish ~stale ~verify_remote =
          let* local_after=write_local ~stale in
          if not (same local_after.flags local_target) then
            pending "local flags did not reach the journaled target"
          else
            let* ()=if not verify_remote then Ok () else
              let* observed,_=remote selected ~uid ~modseq:false in
              if same observed remote_target then Ok ()
              else pending
                "remote flags changed after the local FLAGS update" in
            J.observe_operation store ~id
              ~receipt:"verified FLAGS on both endpoints"
              ~destination_uidvalidity:None ~destination_uid:None;
            commit store pair ~id ~merged in
        match E.current_pair store pair with
        | Error error -> unsent "pair changed before FLAGS dispatch" error
        | Ok _ when not remote_needed ->
            (match remote selected ~uid ~modseq:false with
             | Error error ->
                 unsent "remote read failed before FLAGS dispatch" error
             | Ok (observed,_) when not (same observed remote_before) ->
                 unsent "remote flags changed before the local FLAGS write"
                   Modified
             | Ok _ when not (E.unchanged ?inventory maildir local_before) ->
                 unsent "local message changed before the FLAGS write"
                   Modified
             | Ok _ ->
                 J.mark_sent store ~id;
                 finish ~verify_remote:false ~stale:(fun () ->
                   J.reject_operation store ~id
                     ~receipt:"local message changed before the FLAGS write";
                   Error Modified))
        | Ok _ ->
            J.mark_sent store ~id;
            let set=Imap.Uid_set.singleton uid in
            let stored=match remote_modseq with
              | None ->
                  Imap_eio.Selected.uid_store_flags selected ~set
                    ~operation:`Replace ~flags:remote_target
              | Some unchangedsince ->
                  Result.bind (Imap_eio.Selected.Condstore.require selected)
                    (fun condstore ->
                      Imap_eio.Selected.Condstore.uid_store_flags condstore
                        ~set ~operation:`Replace ~flags:remote_target
                        ~unchangedsince) in
            match stored with
            | Error (Imap_eio.Error.Rejected _ | Imap_eio.Error.State _
                     | Imap_eio.Error.Unsupported _
                     | Imap_eio.Error.Not_enabled _ as error) ->
                J.reject_operation store ~id
                  ~receipt:("UID STORE not applied: " ^ E.describe error);
                Error (Client error)
            | Error error ->
                let reason="UID STORE outcome unknown: " ^ E.describe error in
                J.mark_ambiguous ~reason store ~id;
                let* ()=flag_conflict store pair ~id reason in
                Error (Client error)
            | Ok receipt when Imap.Uid_set.mem uid receipt.modified ->
                J.reject_operation store ~id ~receipt:"MODIFIED";
                Error Modified
            | Ok receipt when not (Imap.Uid_set.is_empty receipt.modified) ->
                let reason="MODIFIED named an unrelated UID" in
                J.mark_ambiguous ~reason store ~id;
                pending reason
            | Ok _ ->
                match remote selected ~uid ~modseq:false with
                | Error error ->
                    let* ()=flag_conflict store pair ~id
                      "FLAGS write returned but its verification read failed" in
                    Error error
                | Ok (observed,_) when not (same observed remote_target) ->
                    pending
                      "FLAGS write returned but remote flags differ from the \
                       target"
                | Ok _ ->
                    finish ~verify_remote:true ~stale:(fun () ->
                      pending
                        "local message changed while the FLAGS write was in \
                         flight") in
      with_selected ctx ~mode:`Read_write ~epoch
        (fun selected info ->
          let* remote_before,remote_modseq=
            remote selected ~uid ~modseq:condstore in
          let* decision=plan_flags ~propagate_deleted ~base
            ~remote:remote_before ~local:local_before.flags ~condstore
            ~remote_modseq () in
          let* outcome=match decision.plan with
            | No_change -> Ok Unchanged
            | Apply merged ->
                apply selected info ~remote_before ~remote_modseq ~merged in
          Ok {outcome;deleted_held=decision.deleted_held})
