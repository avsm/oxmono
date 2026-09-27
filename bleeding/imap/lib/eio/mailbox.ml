module Cap = Imap.Capability

type t = Selected.t

let of_selected selected = selected

type ('a, 's) outcome = { strategy : 's; result : ('a, Error.t) result }

let outcome strategy result = {strategy; result}

let ( let* ) = Result.bind

let rec first_error = function
  | [] -> Ok ()
  | check :: rest -> let* () = check () in first_error rest

let search t ~criteria =
  outcome `Search (
    let* () = first_error (List.map (fun c () -> Selected.check_gate t c)
      (Imap.Search.capabilities criteria)) in
    Selected.uid_search t ~criteria)

let max_fetch = 1000

let distinct uids =
  let seen = Hashtbl.create 64 in
  List.filter (fun uid ->
    if Hashtbl.mem seen uid then false
    else (Hashtbl.add seen uid (); true)) uids

let rec chunks acc current n = function
  | [] ->
      List.rev (if current = [] then acc else List.rev current :: acc)
  | uid :: rest when n = max_fetch ->
      chunks (List.rev current :: acc) [uid] 1 rest
  | uid :: rest -> chunks acc (uid :: current) (n + 1) rest

let fetch ?(drop_unsupported=false) t ~uids ~items =
  let supported item =
    first_error (List.map (fun c () -> Selected.check_gate t c)
      (Imap.Fetch_item.capabilities item)) in
  let items =
    if drop_unsupported then
      Ok (List.filter (fun item -> Result.is_ok (supported item)) items)
    else
      let* () = first_error (List.map (fun item () -> supported item) items)
      in Ok items in
  match items with
  | Error e -> outcome (`Fetch 0) (Error e)
  | Ok items ->
      let rec go trips acc = function
        | [] -> outcome (`Fetch trips) (Ok (List.concat (List.rev acc)))
        | uids :: rest ->
            match Selected.fetch t ~uids ~items with
            | Ok rows -> go (trips + 1) (rows :: acc) rest
            | Error e -> outcome (`Fetch (trips + 1)) (Error e) in
      go 0 [] (chunks [] [] 0 (distinct uids))

let store ?unchangedsince t ~set ~operation ~flags =
  match unchangedsince with
  | None ->
      outcome `Unconditional
        (Selected.uid_store_flags t ~set ~operation ~flags)
  | Some unchangedsince ->
      outcome `Conditional (
        let* condstore = Selected.Condstore.require t in
        Selected.Condstore.uid_store_flags condstore ~set ~operation ~flags
          ~unchangedsince)

type move_strategy = [
  | `Move
  | `Copy_then_expunge
  | `Copy_then_flag
  | `Copied of Selected.copy_receipt option
  | `Copied_and_flagged of Selected.copy_receipt option ]

let deleted = [Mail_flag.Imap_flag.system Deleted]

let copy_then t ~set ~mailbox ~strategy ~expunge =
  match
    let* () = Selected.check_writable t in
    Selected.uid_copy t ~set ~mailbox
  with
  | Error e -> outcome strategy (Error e)
  | Ok receipt ->
      match Selected.uid_store_flags t ~set ~operation:`Add ~flags:deleted
      with
      | Error e -> outcome (`Copied receipt) (Error e)
      | Ok _ ->
          match expunge with
          | None -> outcome strategy (Ok receipt)
          | Some uidplus ->
              match Selected.Uidplus.uid_expunge uidplus ~set with
              | Ok () -> outcome strategy (Ok receipt)
              | Error e -> outcome (`Copied_and_flagged receipt) (Error e)

let move t ~set ~mailbox : (_, move_strategy) outcome =
  match Selected.Move.require t with
  | Ok move -> outcome `Move (Selected.Move.uid_move move ~set ~mailbox)
  | Error (Error.Unsupported _) ->
      (match Selected.Uidplus.require t with
       | Ok uidplus ->
           copy_then t ~set ~mailbox ~strategy:`Copy_then_expunge
             ~expunge:(Some uidplus)
       | Error (Error.Unsupported _) ->
           copy_then t ~set ~mailbox ~strategy:`Copy_then_flag ~expunge:None
       | Error e -> outcome `Copy_then_expunge (Error e))
  | Error e -> outcome `Move (Error e)

type change =
  | Flags of Imap.Uid.t * Mail_flag.Imap_flag.t list
  | Vanished of Imap.Uid_set.t
  | New of Imap.Uid.t

let protocol message = Error (Error.Protocol message)

let flags_change ?uidnext uid flags =
  match uidnext with
  | Some next when Imap.Uid.compare uid next >= 0 -> New uid
  | _ -> Flags (uid, flags)

let wire_change ?uidnext (row : Imap.Response.fetch) =
  match row.uid, row.flags with
  | Some uid, Some flags ->
      (match Imap.Uid.of_int64 uid with
       | Error message -> protocol message
       | Ok uid ->
           let rec decode acc = function
             | [] -> Ok (Some (flags_change ?uidnext uid (List.rev acc)))
             | raw :: rest ->
                 match Mail_flag.Imap_flag.of_wire raw with
                 | Ok flag -> decode (flag :: acc) rest
                 | Error message -> protocol message in
           decode [] flags)
  | _ -> Ok None

let rec collect f acc = function
  | [] -> Ok (List.rev acc)
  | x :: rest ->
      match f x with
      | Ok None -> collect f acc rest
      | Ok (Some change) -> collect f (change :: acc) rest
      | Error _ as e -> e

let every_uid =
  match Imap.Uid.of_int64 1L, Imap.Uid.of_int64 4294967295L with
  | Ok first, Ok last -> Imap.Uid_set.of_intervals [first, last]
  | _ -> assert false

let qresync_changes ?uidnext qresync since =
  let* responses = Selected.Qresync.fetch_changes qresync ~set:every_uid
    ~since ~vanished:true in
  collect (function
    | Imap.Response.Untagged (Imap.Response.Fetch row)
    | Imap.Response.Untagged (Imap.Response.Uidfetch row) ->
        wire_change ?uidnext row
    | Imap.Response.Untagged (Imap.Response.Vanished {uids; _}) ->
        (match Imap.Uid_set.of_wire uids with
         | Ok set -> Ok (Some (Vanished set))
         | Error message -> protocol message)
    | _ -> Ok None) [] responses

let window_bound t =
  let* info = Selected.info t in
  Ok (Int64.pred info.Imap.Response.uidnext)

let windows t f =
  let* last = window_bound t in
  let rec go first acc =
    if first > last then Ok (List.concat (List.rev acc))
    else
      let stop = min last (Int64.add first (Int64.of_int (max_fetch - 1))) in
      match Imap.Uid.of_int64 first, Imap.Uid.of_int64 stop with
      | Ok first_uid, Ok last_uid ->
          let* changes = f ~first:first_uid ~last:last_uid in
          go (Int64.succ stop) (changes :: acc)
      | Error message, _ | _, Error message -> protocol message in
  go 1L []

let condstore_changes ?uidnext t condstore since =
  windows t (fun ~first ~last ->
    let* rows =
      Selected.Condstore.fetch_changes_range condstore ~first ~last ~since in
    collect (wire_change ?uidnext) [] rows)

let full_changes ?uidnext t =
  windows t (fun ~first ~last ->
    let* rows = Selected.fetch_range t ~first ~last
      ~items:[Imap.Fetch_item.Flags] in
    Ok (List.filter_map (fun (row : Selected.row) ->
      Option.map (flags_change ?uidnext row.uid) row.flags) rows))

let changes_since ?uidnext t since =
  let condstore_or_full since =
    match Selected.Condstore.require t with
    | Ok condstore ->
        outcome `Condstore (condstore_changes ?uidnext t condstore since)
    | Error (Error.Unsupported _) -> outcome `Full (full_changes ?uidnext t)
    | Error e -> outcome `Condstore (Error e) in
  match since with
  | None -> outcome `Full (full_changes ?uidnext t)
  | Some since ->
      match Selected.Qresync.require t with
      | Ok qresync ->
          outcome `Qresync (qresync_changes ?uidnext qresync since)
      | Error (Error.Unsupported _ | Error.Not_enabled _) ->
          condstore_or_full since
      | Error e -> outcome `Qresync (Error e)

let list_with_status client ?reference ~pattern items =
  if Client.has client Cap.List_extended && Client.has client Cap.List_status
  then
    outcome `List_status (
      let* discovery = Client.list_extended client ?reference
        ~patterns:[pattern] ~status:items () in
      Ok discovery.Client.mailboxes)
  else
    outcome `List_then_status (
      if items = [] then Error (Error.State "empty STATUS items")
      else
        let* entries = Client.list client ?reference ~pattern () in
        collect (fun (entry : Client.mailbox_entry) ->
          match entry.info.selectable, entry.name.utf8 with
          | false, _ | _, Error _ -> Ok (Some (entry, None))
          | true, Ok mailbox ->
              match Client.status client ~mailbox ~items with
              | Ok status -> Ok (Some (entry, Some status))
              | Error (Error.Rejected {status = `No; _}) ->
                  Ok (Some (entry, None))
              | Error e -> Error e) [] entries)

let wakes = function
  | Imap.Response.(Untagged
      ( Exists _ | Expunge _ | Fetch _ | Uidfetch _ | Vanished _
      | Ok (Some _, _) | No (Some _, _) | Bad (Some _, _)
      | Bye (Some _, _) )) -> true
  | _ -> false

let keepalive = function
  | Imap.Response.Untagged (Imap.Response.Ok (None, _)) -> true
  | _ -> false

let rec until_change next =
  let* responses = next () in
  if List.exists wakes responses then
    Ok (List.filter (fun r -> not (keepalive r)) responses)
  else until_change next

let wait t ~clock ~poll_seconds =
  match Selected.Idle.require t with
  | Ok idle ->
      outcome `Idle
        (until_change (fun () -> Selected.Idle.wait_for_change idle))
  | Error (Error.Unsupported _) ->
      outcome `Poll (until_change (fun () ->
        Eio.Time.sleep clock poll_seconds;
        Selected.noop t))
  | Error e -> outcome `Idle (Error e)
