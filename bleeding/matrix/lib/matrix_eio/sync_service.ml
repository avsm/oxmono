module Base = Matrix_client.Base_client
module Client_error = Matrix_client.Error
module Sliding = Matrix_proto.Sliding_sync
module Event = Matrix_proto.Event

type state = Base.state
type changes = Base.changes
type room_change = Base.room_change
type decrypted = Base.decrypted
type action = Sync.action = Continue | Stop | Retry_after of float

type t = {
  mutable state : state;
  hooks : Base.Hooks.t;
  mutable profile_hooks : (state -> Base.profile_change list -> unit) list;
  mutable next_profile_subscription : int;
  profile_subscribers :
    (int, state -> Base.profile_change list -> unit) Hashtbl.t;
  store : Matrix_client.Store.t option;
  (* Incremented by local destructive operations. A response fetched before
     one of those operations must not be folded after it completes. *)
  mutable generation : int;
  (* Serializes state folds with local state mutations. The network request is
     deliberately outside this lock; [generation] rejects its stale result. *)
  lifecycle_mutex : Eio.Mutex.t;
}

let create ?store state =
  {
    state;
    hooks = Base.Hooks.create ();
    profile_hooks = [];
    next_profile_subscription = 0;
    profile_subscribers = Hashtbl.create 4;
    store;
    generation = 0;
    lifecycle_mutex = Eio.Mutex.create ();
  }

let restore_store_best_effort store before =
  try Matrix_client.Store.restore store before
  with Invalid_argument message ->
    Logs.warn (fun m ->
        m "sync: restoring store state after a failed write failed: %s" message)

(* A persistence error is part of the service's retry protocol, not evidence
   that the mutex-protected data structure is corrupt. Convert exceptions to
   values while the lock is held and re-raise after releasing it, so Eio does
   not poison the lifecycle mutex. *)
let use_lifecycle t f =
  let result =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_mutex (fun () ->
        try Ok (f ())
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Error (exn, bt))
  in
  match result with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let of_store ~store ~user_id ?display_name ?ruleset () =
  create ~store (Base.of_store store ~user_id ?display_name ?ruleset ())

let of_user ~user_id ?display_name ?ruleset ?plaintext_policy () =
  create (Base.create ~user_id ?display_name ?ruleset ?plaintext_policy ())

let state t = t.state
let store t = t.store
let members t room_id = Base.members t.state room_id
let on_response t f = Base.Hooks.on_response t.hooks f
let on_sliding_response t f = Base.Hooks.on_sliding_response t.hooks f
let on_room_event t f = Base.Hooks.on_room_event t.hooks f
let on_profile_change t f = t.profile_hooks <- t.profile_hooks @ [ f ]

type profile_subscription = int

let subscribe_profile_changes t callback =
  use_lifecycle t (fun () ->
      let subscription = t.next_profile_subscription in
      t.next_profile_subscription <- subscription + 1;
      Hashtbl.replace t.profile_subscribers subscription callback;
      subscription)

let unsubscribe_profile_changes t subscription =
  use_lifecycle t (fun () -> Hashtbl.remove t.profile_subscribers subscription)

let notify_profile_hooks hooks state changes =
  List.iter
    (fun callback ->
      try callback state changes with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Logs.warn (fun m ->
              m "sync: profile-change callback failed: %s"
                (Printexc.to_string exn)))
    hooks

let notify_profile_subscribers t subscriptions state changes =
  List.iter
    (fun subscription ->
      let callback =
        Eio.Mutex.use_ro t.lifecycle_mutex (fun () ->
            Hashtbl.find_opt t.profile_subscribers subscription)
      in
      match callback with
      | None -> ()
      | Some callback -> (
          try callback state changes with
          | Eio.Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace exn bt
          | exn ->
              Logs.warn (fun m ->
                  m "sync: profile-change subscriber failed: %s"
                    (Printexc.to_string exn))))
    subscriptions

(* Persist only the common profile projection. The store savepoint keeps a
   failed optimistic or filesystem write from becoming observable through the
   shared in-memory handle. *)
let persist_profiles_unlocked t candidate =
  match t.store with
  | None -> ()
  | Some store -> (
      let before = Matrix_client.Store.snapshot store in
      try
        Matrix_client.Store.replace_profiles store (Base.profiles candidate);
        Error.with_context "flushing synced Matrix profiles" (fun () ->
            match Matrix_client.Store.flush store with
            | Ok () -> ()
            | Error error ->
                Matrix_client.Store.restore store before;
                Error.raise_client_error error)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        restore_store_best_effort store before;
        Printexc.raise_with_backtrace exn bt)

let commit_profile_state_unlocked t candidate changes =
  match changes with
  | [] -> (t.state, [], [], [])
  | _ ->
      persist_profiles_unlocked t candidate;
      t.state <- candidate;
      t.generation <- t.generation + 1;
      ( t.state,
        changes,
        t.profile_hooks,
        Hashtbl.to_seq_keys t.profile_subscribers |> List.of_seq )

let apply_profile_updates t updates =
  let state, changes, hooks, subscriptions =
    use_lifecycle t (fun () ->
        let candidate, changes = Base.apply_profile_updates t.state updates in
        commit_profile_state_unlocked t candidate changes)
  in
  notify_profile_hooks hooks state changes;
  notify_profile_subscribers t subscriptions state changes;
  changes

let clear_profiles t =
  let state, changes, hooks, subscriptions =
    use_lifecycle t (fun () ->
        let users =
          Base.profiles t.state
          |> List.map (fun (user_id, _) ->
              (user_id, Matrix_proto.Sliding_sync.Response.Dropped))
        in
        let updates : Matrix_proto.Sliding_sync.Response.profiles = { users } in
        let candidate, changes = Base.apply_profile_updates t.state updates in
        commit_profile_state_unlocked t candidate changes)
  in
  notify_profile_hooks hooks state changes;
  notify_profile_subscribers t subscriptions state changes;
  changes

let persist_state_unlocked t state =
  match t.store with
  | None -> ()
  | Some store -> (
      let before = Matrix_client.Store.snapshot store in
      try
        Base.persist store state;
        Error.with_context "flushing Matrix sync state" (fun () ->
            match Matrix_client.Store.flush store with
            | Ok () -> ()
            | Error error ->
                Matrix_client.Store.restore store before;
                Error.raise_client_error error)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        restore_store_best_effort store before;
        Printexc.raise_with_backtrace exn bt)

let persist_unlocked t = persist_state_unlocked t t.state
let generation t = Eio.Mutex.use_ro t.lifecycle_mutex (fun () -> t.generation)

let migrate_legacy_sliding_state t =
  use_lifecycle t (fun () ->
      match t.store with
      | None -> false
      | Some store -> (
          match Base.migrate_legacy_sliding_state store t.state with
          | Ok (candidate, migrated) ->
              if migrated then begin
                t.state <- candidate;
                t.generation <- t.generation + 1
              end;
              migrated
          | Error error ->
              Error.raise_client_error
                ~context:"migrating legacy sliding-sync state" error))

let reset_sliding_session t =
  use_lifecycle t (fun () ->
      t.generation <- t.generation + 1;
      let candidate = Base.reset_sliding_session t.state in
      persist_state_unlocked t candidate;
      t.state <- candidate)

(* [Base.persist] updates the mutable store before [flush] can report a
   failure, so bootstrap uses the same full savepoint as response commits. *)
let persist_bootstrap_unlocked t candidate =
  match t.store with
  | None -> ()
  | Some store -> (
      let before = Matrix_client.Store.snapshot store in
      try
        Base.persist store candidate;
        Error.with_context "flushing bootstrapped Matrix sync state" (fun () ->
            Error.unwrap (Matrix_client.Store.flush store))
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        restore_store_best_effort store before;
        Printexc.raise_with_backtrace exn bt)

let persist t = use_lifecycle t (fun () -> persist_unlocked t)

let set_local_unread_counts_if_current t ~room_id ~expected counts =
  let result =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_mutex (fun () ->
        try
          if t.state != expected then Ok None
          else
            let candidate =
              Base.with_local_unread_counts t.state ~room_id counts
            in
            if candidate == t.state then Ok (Some t.state)
            else
              match t.store with
              | None ->
                  t.state <- candidate;
                  t.generation <- t.generation + 1;
                  Ok (Some candidate)
              | Some store -> (
                  let before = Matrix_client.Store.snapshot store in
                  try
                    match Base.find_room candidate room_id with
                    | None -> Ok None
                    | Some info ->
                        Matrix_client.Store.set_room store info;
                        Error.with_context "flushing local Matrix unread state"
                          (fun () ->
                            Error.unwrap (Matrix_client.Store.flush store));
                        t.state <- candidate;
                        t.generation <- t.generation + 1;
                        Ok (Some candidate)
                  with exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    restore_store_best_effort store before;
                    Error (exn, bt))
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Error (exn, bt))
  in
  match result with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

(* A complete, valid account-data event is the durable marker that the active
   rules came from sync (or from the endpoint bootstrap).  [with_ruleset] is
   deliberately not used here: it is an in-memory fallback override and does
   not establish that marker. *)
let valid_push_rules_content content =
  match Jsont.Json.decode Matrix_proto.Push.Ruleset.global_jsont content with
  | Ok ruleset -> Some ruleset
  | Error _ -> None

let valid_push_rules state =
  Option.bind
    (Base.find_account_data state "m.push_rules")
    valid_push_rules_content

let valid_stored_push_rules t =
  Option.bind t.store (fun store ->
      Option.bind
        (Matrix_client.Store.find_account_data store "m.push_rules")
        valid_push_rules_content)

let push_rules_bootstrap_fallback = function
  | Client_error.Matrix_error
      {
        errcode =
          ( Client_error.M_NOT_FOUND | Client_error.M_UNRECOGNIZED
          | Client_error.M_UNKNOWN_CODE "M_UNSUPPORTED" );
        _;
      }
  | Client_error.Http_error { status = 404 | 405; _ }
  | Client_error.Json_error _ ->
      true
  | _ -> false

(* Fetch the endpoint representation only when neither the running state nor
   its common store has a usable event.  The request is outside the mutex, but
   the final installation is serialized with sync commits.  Incrementing the
   generation makes a response staged before this commit stale; a later sync
   carrying its own valid event is detected at the commit point and wins. *)
let bootstrap_push_rules client t =
  let source =
    Eio.Mutex.use_rw ~protect:true t.lifecycle_mutex (fun () ->
        match valid_push_rules t.state with
        | Some _ -> `Present
        | None -> (
            match valid_stored_push_rules t with
            | Some ruleset -> `Stored ruleset
            | None -> `Fetch))
  in
  let install ruleset ~already_stored =
    (* [Eio.Mutex] poisons a mutex when its protected callback raises.  Capture
       persistence failures as values so a retry can use the same service;
       re-raise only after the lock has been released. *)
    let result =
      Eio.Mutex.use_rw ~protect:true t.lifecycle_mutex (fun () ->
          try
            (match valid_push_rules t.state with
            | Some _ -> ()
            | None -> (
                (* A sync commit may have landed while the GET was in flight.
                   It is authoritative even if this service's state did not
                   observe the store update yet. *)
                match valid_stored_push_rules t with
                | Some synced_rules ->
                    t.state <- Base.with_push_rules t.state synced_rules;
                    t.generation <- t.generation + 1
                | None ->
                    let candidate = Base.with_push_rules t.state ruleset in
                    (* The endpoint result is not published until its
                       complete state has been persisted.  [already_stored]
                       avoids a needless filesystem write for the no-I/O
                       store fast path. *)
                    if not already_stored then
                      persist_bootstrap_unlocked t candidate;
                    t.state <- candidate;
                    t.generation <- t.generation + 1));
            Ok ()
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Error (exn, bt))
    in
    match result with
    | Ok () -> ()
    | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt
  in
  match source with
  | `Present -> ()
  | `Stored ruleset -> install ruleset ~already_stored:true
  | `Fetch -> (
      match Matrix_client.Push.get_push_rules (Client.base client) with
      | Ok ruleset -> install ruleset ~already_stored:false
      | Error error when push_rules_bootstrap_fallback error ->
          Logs.debug (fun m ->
              m "push-rules bootstrap unavailable; retaining current rules")
      | Error error ->
          Error.raise_client_error ~context:"bootstrapping Matrix push rules"
            error)

(* Invalidate responses fetched before a local destructive operation starts.
   The operation may yield to account-data HTTP, so waiting until
   [forget_room] would leave a window for a stale response to overwrite the
   cleanup. *)
let begin_forget t =
  Eio.Mutex.use_rw ~protect:true t.lifecycle_mutex (fun () ->
      t.generation <- t.generation + 1)

let forget_room t room_id =
  use_lifecycle t (fun () ->
      t.generation <- t.generation + 1;
      let candidate = Base.forget_room t.state room_id in
      match t.store with
      | None -> t.state <- candidate
      | Some store -> (
          let before = Matrix_client.Store.snapshot store in
          try
            Matrix_client.Store.remove_room store room_id;
            Matrix_client.Store.remove_receipts store room_id;
            persist_state_unlocked t candidate;
            t.state <- candidate
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            restore_store_best_effort store before;
            Printexc.raise_with_backtrace exn bt))

let remove_direct_room t room_id =
  use_lifecycle t (fun () ->
      t.generation <- t.generation + 1;
      let candidate = Base.remove_direct_room t.state room_id in
      persist_state_unlocked t candidate;
      t.state <- candidate)

let replace_members t room_id members =
  use_lifecycle t (fun () ->
      (* [/members] is another authoritative state write. Invalidate a sync
         candidate staged from the older snapshot instead of letting its final
         commit silently overwrite this result. *)
      t.generation <- t.generation + 1;
      let candidate = Base.replace_members t.state room_id members in
      persist_state_unlocked t candidate;
      t.state <- candidate)

(* The encryption machine must see a response before the base client does:
   the [m.room_key] events in its [to_device] section are what make the
   [m.room.encrypted] events in its timelines decryptable, and both arrive in
   the same response. The order per response is therefore: fold into the
   machine and perform the requests it asks for; route the decrypted
   [m.key.verification.*] to their flows; fold into the base client with the
   machine as the decryptor; track the members of rooms that turned out to be
   encrypted; save. The caller's hooks run last, so a callback always sees a
   response whose encrypted events have already been opened. *)

let route_verification verification client (outcome : Encryption.outcome) =
  match verification with
  | None -> ()
  | Some v ->
      List.iter
        (function
          | Matrix_client.Encryption.Verification
              { event_type; sender; content; _ } ->
              Verification_service.handle v client ~sender ~event_type ~content
          | _ -> ())
        outcome.Matrix_client.Encryption.events

let is_room_verification_event event_type =
  List.exists (String.equal event_type)
    [
      "m.room.message";
      "m.key.verification.ready";
      "m.key.verification.start";
      "m.key.verification.cancel";
      "m.key.verification.accept";
      "m.key.verification.key";
      "m.key.verification.mac";
      "m.key.verification.done";
    ]

let route_room_verification verification client encryption
    (changes : Base.changes) =
  match verification with
  | None -> ()
  | Some v ->
      List.iter
        (fun (change : Base.room_change) ->
          List.iter
            (fun (event : Event.Raw_event.t) ->
              let event =
                match
                  ( Event.Event_type.equal event.type_
                      Event.Event_type.Room_message_encrypted,
                    encryption )
                with
                | true, Some _ ->
                    Option.fold ~none:event
                      ~some:(fun (decrypted : Base.decrypted) ->
                        decrypted.plaintext)
                      (List.find_opt
                         (fun (decrypted : Base.decrypted) ->
                           decrypted.encrypted.event_id = event.event_id)
                         change.decrypted)
                | _ -> event
              in
              match event.event_id with
              | None -> ()
              | Some event_id ->
                  let event_type = Event.Event_type.to_string event.type_ in
                  if
                    Matrix_proto.Id.User_id.equal event.sender
                      (Client.user_id client)
                    || not (is_room_verification_event event_type)
                  then ()
                  else
                    Verification_service.handle_room v client
                      ~room_id:change.changed_room_id ~event_id
                      ~sender:event.sender ~timestamp:event.origin_server_ts
                      ~event_type ~content:event.content)
            change.timeline)
        changes.room_changes

(* The encryption side of a response performs HTTP, and a key upload that
   503s must not take the sync loop down with it. Every request the machine
   wants is recomputed from its state on the next response, so the retry is
   automatic and losing one attempt costs a single round; the failure is
   therefore logged rather than raised, and the response is folded in
   regardless, since dropping it would lose the [next_batch] and re-fetch
   everything. *)
let attempt ?on_encryption_error what f =
  try f ()
  with Eio.Io (Error.E err, _) as exn ->
    Logs.warn (fun m -> m "sync: %s failed: %a" what Eio.Exn.pp exn);
    Option.iter
      (fun callback ->
        try callback err with
        | Eio.Cancel.Cancelled _ as exn ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace exn bt
        | exn ->
            Logs.warn (fun m ->
                m "sync: on_encryption_error callback failed: %s"
                  (Printexc.to_string exn)))
      on_encryption_error

(* Runs after the fold, over the rooms whose [m.room.encryption] the response
   carried. [track_users] only marks the members outdated, so the
   [/keys/query] that follows is the one request this produces. *)
let track_encrypted_rooms ?on_encryption_error enc client state
    (changes : Base.changes) =
  (* [sync_hook] has already attempted every request which was pending before
     the base-state fold. Remember failures so this second phase executes only
     work newly created by learning that rooms are encrypted; otherwise a 503
     on the initial key upload is retried twice back-to-back in one sync. *)
  let already_attempted = Encryption.outgoing_requests enc in
  List.iter
    (fun (c : Base.room_change) ->
      (match c.info.encryption with
      | None -> ()
      | Some content -> (
          try
            Encryption.set_room_encryption_settings enc c.changed_room_id
              content
          with Eio.Io (Error.E err, _) as exn ->
            Logs.warn (fun m ->
                m
                  "sync: room %s is encrypted with an algorithm we cannot \
                   read: %a"
                  (Matrix_proto.Id.Room_id.to_string c.changed_room_id)
                  Eio.Exn.pp exn);
            Option.iter
              (fun callback ->
                try callback err with
                | Eio.Cancel.Cancelled _ as exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    Printexc.raise_with_backtrace exn bt
                | exn ->
                    Logs.warn (fun m ->
                        m "sync: on_encryption_error callback failed: %s"
                          (Printexc.to_string exn)))
              on_encryption_error));
      if Encryption.is_room_encrypted enc c.changed_room_id then
        Encryption.track_users enc (Base.members state c.changed_room_id))
    changes.room_changes;
  let newly_generated =
    Encryption.outgoing_requests enc
    |> List.filter (fun request -> not (List.mem request already_attempted))
  in
  Encryption.execute_requests ?on_error:on_encryption_error enc client
    newly_generated

(* Rust clears restored invite-acceptance gates whose room is no longer
   joinable. Reconcile the whole set rather than only this response's changes:
   Base and crypto use separate snapshots, so a crash can leave the latter one
   write behind. We retain Invited here because, unlike Rust, [Runtime.join]
   waits for sync rather than immediately changing Base to Joined. *)
let clear_stale_pending_key_bundles enc state =
  let machine = Encryption.machine enc in
  ignore
    (Matrix_client.Encryption.clear_expired_pending_key_bundles machine
      : Matrix_proto.Id.Room_id.t list);
  List.iter
    (fun (pending : Matrix_client.Encryption.pending_key_bundle) ->
      match Base.find_room state pending.room_id with
      | Some { membership = Base.Joined | Base.Invited; _ } -> ()
      | Some { membership = Base.Left | Base.Knocked; _ } | None ->
          ignore
            (Matrix_client.Encryption.clear_pending_key_bundle machine
               ~room_id:pending.room_id
              : bool))
    (Matrix_client.Encryption.pending_key_bundles machine)

let notify_bundle_error ?on_encryption_error error =
  Option.iter
    (fun callback ->
      try callback error with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Logs.warn (fun m ->
              m "sync: on_encryption_error callback failed: %s"
                (Printexc.to_string exn)))
    on_encryption_error

let process_received_key_bundles ?on_encryption_error enc client state =
  let machine = Encryption.machine enc in
  List.iter
    (fun (bundle : Matrix_client.Encryption.received_key_bundle) ->
      let joined =
        match Base.find_room state bundle.room_id with
        | Some { membership = Base.Joined; _ } -> true
        | Some { membership = Base.Invited | Base.Left | Base.Knocked; _ }
        | None ->
            false
      in
      match
        Encryption.accept_received_room_key_bundle enc client ~joined bundle
      with
      | Matrix_client.Encryption_driver.Bundle_not_applicable -> ()
      | Bundle_imported count ->
          Logs.info (fun m ->
              m "sync: imported %d historic room keys for %a" count
                Matrix_proto.Id.Room_id.pp bundle.room_id)
      | Bundle_rejected_sender ->
          Logs.warn (fun m ->
              m
                "sync: discarded room-key bundle for %a because inviter %a is \
                 not cross-signed"
                Matrix_proto.Id.Room_id.pp bundle.room_id
                Matrix_proto.Id.User_id.pp bundle.sender)
      | Bundle_discarded_not_found ->
          Logs.warn (fun m ->
              m "sync: discarded missing room-key bundle media for %a"
                Matrix_proto.Id.Room_id.pp bundle.room_id)
      | Bundle_discarded_malformed message ->
          Logs.warn (fun m ->
              m "sync: discarded malformed room-key bundle for %a: %s"
                Matrix_proto.Id.Room_id.pp bundle.room_id message)
      | Bundle_retry_key_query error ->
          Logs.warn (fun m ->
              m "sync: room-key bundle sender refresh failed: %s"
                (Matrix_client.Error.to_string error));
          notify_bundle_error ?on_encryption_error (Error.of_client_error error)
      | Bundle_retry_download error -> (
          Logs.warn (fun m ->
              m "sync: room-key bundle download failed: %a"
                Matrix_client.Media.pp_encrypted_error error);
          match error with
          | Matrix_client.Media.Media_error error ->
              notify_bundle_error ?on_encryption_error
                (Error.of_client_error error)
          | Matrix_client.Media.Attachment_error _ -> ()))
    (Matrix_client.Encryption.received_key_bundles machine)

exception Stale_response

(* The crypto prelude and post-fold trust work may yield and invoke user
   callbacks, so they deliberately run outside [lifecycle_mutex]. The base
   fold and each final persistence decision take the lock briefly. *)
let apply_unlocked ?expected_generation ?on_committed ?coverage ?encryption
    ?verification ?on_encryption_error client t response =
  Option.iter
    (fun enc ->
      attempt ?on_encryption_error "processing to-device events" (fun () ->
          route_verification verification client
            (Encryption.sync_hook ?on_error:on_encryption_error enc client
               response)))
    encryption;
  let decrypt = Option.map Encryption.decrypt_room_event encryption in
  let check_generation () =
    match expected_generation with
    | Some generation when generation <> t.generation -> raise Stale_response
    | Some _ | None -> ()
  in
  (* Stage the pure base fold without publishing it. A forget may happen in
     any of the yielding work below; until the final commit, [t.state] and its
     persisted token remain untouched. *)
  let state, changes =
    use_lifecycle t (fun () ->
        check_generation ();
        let state, changes = Base.apply ?decrypt ?coverage t.state response in
        (state, changes))
  in
  Option.iter
    (fun enc ->
      clear_stale_pending_key_bundles enc state;
      process_received_key_bundles ?on_encryption_error enc client state;
      attempt ?on_encryption_error "tracking encrypted rooms" (fun () ->
          track_encrypted_rooms ?on_encryption_error enc client state changes))
    encryption;
  (* Learn encryption state before a verification response is sent. A request
     may be the first event observed in a newly joined encrypted room; routing
     it first would leak the response as plaintext. Save afterwards because
     encrypting the response advances the Olm/Megolm machine. *)
  attempt "routing room verification events" (fun () ->
      route_room_verification verification client encryption changes);
  Option.iter
    (fun enc ->
      attempt ?on_encryption_error "saving the crypto state" (fun () ->
          Encryption.save enc))
    encryption;
  Option.iter
    (fun v ->
      attempt "expiring verifications" (fun () ->
          Verification_service.tick v client))
    verification;
  (* Commit only if the response survived all yielding work. *)
  let committed =
    use_lifecycle t (fun () ->
        match expected_generation with
        | Some generation when generation <> t.generation -> false
        | Some _ | None ->
            persist_state_unlocked t state;
            t.state <- state;
            true)
  in
  if not committed then raise Stale_response;
  (* Runtime uses this point to project the committed changes before hooks;
     callbacks are outside the mutex and may synchronously forget a room. *)
  Option.iter (fun f -> f state changes) on_committed;
  (* A reentrant forget from the commit callback invalidates extension hooks
     too. It has already cleaned the committed state, so report this response
     as stale to the sync loop and let it refetch from the surviving token. *)
  check_generation ();
  Base.Hooks.run t.hooks state response changes;
  changes

let apply ?coverage ?encryption ?verification ?on_encryption_error client t
    response =
  apply_unlocked ?coverage ?encryption ?verification ?on_encryption_error client
    t response

let apply_if_current generation ?on_committed ?coverage ?encryption
    ?verification ?on_encryption_error client t response =
  try
    Some
      (apply_unlocked ~expected_generation:generation ?coverage ?encryption
         ?verification ?on_encryption_error ?on_committed client t response)
  with Stale_response -> None

let apply_sliding_unlocked ?expected_generation ?to_device_enabled ?on_committed
    ?before_commit ?encryption ?verification ?on_encryption_error client t
    (response : Matrix_proto.Sliding_sync.Response.t) =
  let crypto_response : Matrix_proto.Sliding_sync.Response.t =
    match to_device_enabled with
    | Some false ->
        {
          response with
          extensions = { response.extensions with to_device = None };
        }
    | Some true | None -> response
  in
  Option.iter
    (fun enc ->
      attempt ?on_encryption_error "processing sliding to-device events"
        (fun () ->
          route_verification verification client
            (Encryption.sync_hook_sliding ?on_error:on_encryption_error enc
               client crypto_response)))
    encryption;
  let decrypt = Option.map Encryption.decrypt_room_event encryption in
  let check_generation () =
    match expected_generation with
    | Some generation when generation <> t.generation -> raise Stale_response
    | Some _ | None -> ()
  in
  let state, changes =
    use_lifecycle t (fun () ->
        check_generation ();
        Base.apply_sliding ?decrypt ?to_device_enabled t.state response)
  in
  Option.iter
    (fun enc ->
      clear_stale_pending_key_bundles enc state;
      process_received_key_bundles ?on_encryption_error enc client state;
      attempt ?on_encryption_error "tracking encrypted sliding rooms" (fun () ->
          track_encrypted_rooms ?on_encryption_error enc client state changes))
    encryption;
  attempt "routing sliding room verification events" (fun () ->
      route_room_verification verification client encryption changes);
  Option.iter
    (fun enc ->
      attempt ?on_encryption_error "saving the crypto state" (fun () ->
          Encryption.save enc))
    encryption;
  Option.iter
    (fun v ->
      attempt "expiring verifications" (fun () ->
          Verification_service.tick v client))
    verification;
  let committed =
    use_lifecycle t (fun () ->
        match expected_generation with
        | Some generation when generation <> t.generation -> (false, [])
        | Some _ | None ->
            Option.iter (fun f -> f ()) before_commit;
            persist_state_unlocked t state;
            t.state <- state;
            let subscriptions =
              if changes.profile_changes = [] then []
              else Hashtbl.to_seq_keys t.profile_subscribers |> List.of_seq
            in
            (true, subscriptions))
  in
  let committed, subscriptions = committed in
  if not committed then raise Stale_response;
  Option.iter (fun f -> f state changes) on_committed;
  check_generation ();
  if changes.profile_changes <> [] then
    notify_profile_hooks t.profile_hooks state changes.profile_changes;
  if changes.profile_changes <> [] then
    notify_profile_subscribers t subscriptions state changes.profile_changes;
  check_generation ();
  Base.Hooks.run_sliding t.hooks state response changes;
  changes

let apply_sliding ?to_device_enabled ?encryption ?verification
    ?on_encryption_error client t response =
  apply_sliding_unlocked ?to_device_enabled ?encryption ?verification
    ?on_encryption_error client t response

let apply_sliding_if_current generation ?to_device_enabled ?on_committed
    ?before_commit ?encryption ?verification ?on_encryption_error client t
    response =
  try
    Some
      (apply_sliding_unlocked ~expected_generation:generation ?to_device_enabled
         ?on_committed ?before_commit ?encryption ?verification
         ?on_encryption_error client t response)
  with Stale_response -> None

let min_backoff = 0.5
let max_backoff = 60.0

(* A client-owned presence update invalidates the request which is currently
   being waited on.  Keep this state separate from [t.lifecycle_mutex]: the
   presence callback is invoked by [Client.set_sync_presence], and must be
   able to wake the sync fiber without contending with a state fold. *)
exception Presence_changed

module Presence_wakeup = struct
  type t = {
    mutex : Eio.Mutex.t;
    mutable epoch : int;
    mutable active : Eio.Cancel.t option;
  }

  let create () = { mutex = Eio.Mutex.create (); epoch = 0; active = None }

  (* Install the cancellation token and return the epoch observed by the same
     critical section.  If a notification won the race before installation,
     the request will read the current client presence and this is the epoch
     it should use. *)
  let start t cancel =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        t.active <- Some cancel;
        t.epoch)

  let wake t =
    let cancel =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          t.epoch <- t.epoch + 1;
          t.active)
    in
    Option.iter (fun cancel -> Eio.Cancel.cancel cancel Presence_changed) cancel

  (* This is the response-acceptance linearization point.  A wake which takes
     this mutex first changes the epoch and makes the response stale; a wake
     which follows it belongs to the next poll.  Clearing [active] here also
     prevents that later wake from cancelling response processing. *)
  let finish t cancel epoch =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let current =
          match t.active with Some active -> active == cancel | None -> false
        in
        if current then t.active <- None;
        current && t.epoch = epoch)

  let stop t cancel =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match t.active with
        | Some active when active == cancel -> t.active <- None
        | Some _ | None -> ())
end

(* The loop's own token always wins over the caller's, so a service built from
   a store resumes rather than asking for an initial sync. *)
let params_for t params =
  { params with Matrix_client.Sync.since = Base.next_batch t.state }

let coverage_of_params (params : Matrix_client.Sync.params) =
  (* An initial unfiltered classic sync supplies the full room state, as does
     an unfiltered request with [full_state=true]. A filter can still restrict
     the state section, and an ordinary incremental response carries deltas. *)
  match (params.filter, params.full_state, params.since) with
  | None, true, _ | None, false, None -> Base.complete_state_coverage
  | _ -> Base.unknown_state_coverage

let run ~sw ~clock client t ?(params = Matrix_client.Sync.default_params)
    ?encryption ?verification ?(on_response = fun _ -> Continue) ?on_error
    ?on_encryption_error ~on_change () =
  let backoff = ref min_backoff in
  let default_on_error _ =
    let d = !backoff in
    backoff := Float.min max_backoff (d *. 2.);
    Retry_after d
  in
  let on_error = match on_error with Some f -> f | None -> default_on_error in
  let dynamic_presence =
    Option.is_none params.Matrix_client.Sync.set_presence
  in
  let wakeup = Presence_wakeup.create () in
  let with_presence f =
    if not dynamic_presence then `Result (f ())
    else
      try
        Eio.Cancel.sub (fun cancel ->
            let epoch = Presence_wakeup.start wakeup cancel in
            Fun.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                    Presence_wakeup.stop wakeup cancel))
              (fun () ->
                Eio.Cancel.check cancel;
                let result = f () in
                Eio.Cancel.check cancel;
                if Presence_wakeup.finish wakeup cancel epoch then
                  `Result result
                else `Restart))
      with Eio.Cancel.Cancelled Presence_changed ->
        (* Do not turn cancellation from the parent switch into an internal
           restart if both cancellations happen at the same time. *)
        Eio.Fiber.check ();
        `Restart
  in
  let rec loop () =
    Eio.Fiber.check ();
    let generation = t.generation in
    let params = params_for t params in
    let again = function
      | Continue -> loop ()
      | Stop -> ()
      | Retry_after d -> (
          match with_presence (fun () -> Eio.Time.sleep clock d) with
          | `Restart -> loop ()
          | `Result () -> loop ())
    in
    match
      Error.with_context "polling the Matrix sync service" (fun () ->
          with_presence (fun () ->
              Matrix_client.Sync.sync_once (Client.base client) ~params ()))
    with
    | `Restart -> loop ()
    | `Result (Error e) -> again (on_error (Error.of_client_error e))
    | `Result (Ok response) -> (
        if generation <> t.generation then
          (* A local operation such as [forget_room] happened while the
             request was in flight. Drop this response and ask again with the
             current state/token; otherwise an old room can be resurrected. *)
          loop ()
        else
          match
            apply_if_current generation
              ~coverage:(coverage_of_params params)
              ?encryption ?verification ?on_encryption_error
              ~on_committed:(fun state changes -> on_change state changes)
              client t response
          with
          | None -> loop ()
          | Some changes ->
              backoff := min_backoff;
              again (on_response response))
  in
  Eio.Fiber.fork ~sw (fun () ->
      let unregister =
        if dynamic_presence then
          Some
            (Client.register_presence_wakeup client (fun () ->
                 Presence_wakeup.wake wakeup))
        else None
      in
      Fun.protect
        ~finally:(fun () ->
          Option.iter
            (fun unregister -> Eio.Cancel.protect unregister)
            unregister)
        loop)

let sync_once client t ?(params = Matrix_client.Sync.default_params) ?encryption
    ?verification ?on_encryption_error () =
  let rec loop () =
    let generation = t.generation in
    let params = params_for t params in
    let response =
      Error.unwrap ~context:"performing a Matrix sync-service request"
        (Matrix_client.Sync.sync_once (Client.base client) ~params ())
    in
    match
      apply_if_current generation
        ~coverage:(coverage_of_params params)
        ?encryption ?verification ?on_encryption_error client t response
    with
    | Some changes -> (response, changes)
    | None -> loop ()
  in
  loop ()
