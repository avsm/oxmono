(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type action = {
  id : string;
  kind : string;
  detail : string;
  local : string option;
  remote : string option;
  candidate : string option;
}

let action ?(detail = "") ?candidate ~id ~kind ~local ~remote () =
  { id; kind; detail; local; remote; candidate }

let describe a =
  obj
    [
      ("id", str a.id);
      ("action", str a.kind);
      ("detail", str a.detail);
      ("local_revision", opt_string (Option.map digest a.local));
      ("remote_revision", opt_string (Option.map digest a.remote));
      ("candidate_revision", opt_string (Option.map digest a.candidate));
    ]

let initial store remote =
  obj
    [
      ("schema", str "dooit.sync/v1");
      ("store_id", str store.Store.id);
      ("destination", str remote.Remote.identity);
      ("bases", obj []);
      ("pending", obj []);
    ]

let state store remote =
  let s = Store.load_state store "sync.json" (initial store remote) in
  if
    field "schema" s <> "dooit.sync/v1"
    || field "store_id" s <> store.Store.id
    || field "destination" s <> remote.Remote.identity
  then
    fail
      "sync state belongs to another store or destination; use a separate \
       local clone";
  s

let raw_ref store = function
  | `Null -> None
  | v -> Some (Store.object_ store (string v))

let raw_field store key v = raw_ref store (get key v)

let parse id raw =
  let d = Doc.parse raw in
  if Doc.id d <> id then fail "remote filename and note identity disagree";
  d

let choose ~id ~base ~local ~remote =
  let finish candidate =
    let kind =
      if Some candidate = local && Some candidate = remote then "unchanged"
      else if Some candidate = remote then "pull"
      else if remote = None then "create"
      else "push"
    in
    action ~id ~kind ~local ~remote ~candidate ()
  in
  List.iter
    (Option.iter (fun raw -> ignore (parse id raw)))
    [ base; local; remote ];
  Option.iter
    (fun b ->
      List.iter
        (Option.iter (fun raw -> Doc.immutable (parse id b) (parse id raw)))
        [ local; remote ])
    base;
  match (base, local, remote) with
  | _, Some l, Some r when l = r -> finish l
  | Some _, None, _ | Some _, _, None ->
      action ~id ~kind:"conflict"
        ~detail:
          "untracked removal; explicitly restore or resolve as a retained \
           deletion"
        ~local ~remote ()
  | _, None, Some r -> finish r
  | None, Some l, None -> finish l
  | Some b, Some l, Some r -> (
      match
        Merge.run ~base:(parse id b) ~local:(parse id l) ~remote:(parse id r)
      with
      | Ok d -> finish d.raw
      | Error reason ->
          action ~id ~kind:"conflict" ~detail:reason ~local ~remote ())
  | None, Some _, Some _ ->
      action ~id ~kind:"conflict"
        ~detail:"different creations without a common baseline" ~local ~remote
        ()
  | _, None, None -> action ~id ~kind:"unchanged" ~local ~remote ()

let planned store s ~id ~local ~remote =
  let bases = get "bases" s and pending = get "pending" s in
  let base =
    Option.map (fun h -> Store.object_ store (string h)) (find id bases)
  in
  let remote_raw = Option.map (fun (f : Remote.file) -> f.raw) remote in
  let ordinary () = choose ~id ~base ~local ~remote:remote_raw in
  match find id pending with
  | None -> ordinary ()
  | Some p ->
      let candidate = Store.object_ store (field "candidate" p)
      and start = raw_field store "local" p
      and previous = raw_field store "remote" p in
      if remote_raw = Some candidate then
        (* The write reached the server. Rebase any new local edits onto the
         accepted candidate before advancing the common base. *)
        if local = start || local = Some candidate then
          action ~id
            ~kind:(if local = Some candidate then "unchanged" else "pull")
            ~candidate ~local ~remote:remote_raw
            ~detail:"recovering confirmed upload" ()
        else
          match (start, local) with
          | Some b, Some l ->
              choose ~id ~base:(Some b) ~local:(Some l) ~remote:remote_raw
          | _ ->
              action ~id ~kind:"conflict"
                ~detail:"local note changed during interrupted creation" ~local
                ~remote:remote_raw ()
      else if
        remote_raw = previous
        && Option.bind remote (fun f -> f.Remote.etag)
           = string_opt (get "etag" p)
      then ordinary ()
      else
        action ~id ~kind:"conflict"
          ~detail:
            "uncertain upload outcome; retained candidate requires resolution"
          ~local ~remote:remote_raw ()

let summary actions =
  obj
    [
      ("schema", str "dooit.sync-report/v1");
      ("actions", arr (List.map describe actions));
    ]

let write_report store path actions =
  separate path store.Store.root;
  if exists path then fail "report directory must be new: %s" path;
  mkdir path;
  save_json (Filename.concat path "report.json") (summary actions);
  let lines =
    List.map
      (fun a ->
        Printf.sprintf "- %s %s%s\n" a.kind a.id
          (if a.detail = "" then "" else ": " ^ a.detail))
      actions
  in
  atomic_write
    (Filename.concat path "report.md")
    ("# Dooit synchronization\n\n" ^ String.concat "" lines);
  List.iteri
    (fun i a ->
      let dir = Filename.concat path (Printf.sprintf "%04d" i) in
      mkdir dir;
      List.iter
        (fun (name, v) ->
          Option.iter (atomic_write (Filename.concat dir name)) v)
        [
          ("local.md", a.local);
          ("remote.md", a.remote);
          ("candidate.md", a.candidate);
        ])
    actions

let error_message = function
  | Error s -> s
  | Fetch_dav.Http_error e -> Printf.sprintf "WebDAV HTTP %d" e.status
  | Fetch_dav.Protocol_error s -> "WebDAV protocol: " ^ s
  | Yamlrw.Yamlrw_error _ -> "invalid YAML"
  | Unix.Unix_error (e, _, _) -> Unix.error_message e
  | Eio.Io _ -> "network or filesystem I/O failed"
  | exn -> raise exn

let bootstrap ?(allow_init = true) ~dry_run store remote =
  let collection_exists = Remote.exists_collection remote "" in
  let entries = if collection_exists then Remote.listing remote "" else [] in
  let meta =
    if collection_exists then Remote.get remote "store.json" else None
  in
  (match meta with
  | Some f ->
      if Store.parse_metadata f.raw <> store.Store.id then
        fail "remote store UUID differs; clone it into a new local root"
  | None ->
      if not allow_init then
        fail
          "previously synchronized remote store is missing; restore it \
           explicitly";
      if entries <> [] then
        fail
          "refusing to initialize a nonempty remote collection without \
           store.json";
      if not dry_run then (
        if not collection_exists then Remote.mkdir remote "";
        let raw = json_string (Store.metadata store.id) ^ "\n" in
        (* If initialization was interrupted, readback on the next run verifies
          the immutable store UUID before any note is touched. *)
        ignore (Remote.put remote ~path:"store.json" ~previous:None raw)));
  let notes_exist = List.mem ("notes/", true) entries in
  if (not notes_exist) && not allow_init then
    fail "remote notes collection was removed; restore it explicitly";
  if
    meta <> None && (not notes_exist)
    && List.exists (fun (name, _) -> name <> "store.json") entries
  then fail "remote collection has unexpected members";
  if (not notes_exist) && not dry_run then Remote.mkdir remote "notes/";
  meta = None || not notes_exist

let finish store remote ~id ~local ~observed candidate =
  Store.with_lock store (fun () ->
      let current = Store.raw store id in
      if current <> local then
        fail "local note changed during sync; upload journal retained";
      let s = state store remote in
      if local <> Some candidate then
        Store.install store ~id ~expected:(Option.map digest local) candidate;
      let bases = set id (str (Store.keep store candidate)) (get "bases" s) in
      let s =
        set "bases" bases (set "pending" (remove id (get "pending" s)) s)
      in
      ignore observed;
      Store.save_state store "sync.json" s;
      Store.clear_conflict store id)

let apply_one store remote a observed =
  match a.candidate with
  | None -> ()
  | Some candidate ->
      if a.kind = "push" || a.kind = "create" then (
        (* Validate the validator before making an operation pending. *)
        Option.iter
          (fun (f : Remote.file) ->
            match f.etag with
            | Some e -> ignore (Remote.etag e)
            | None -> fail "remote replacement requires an ETag")
          observed;
        Store.with_lock store (fun () ->
            if Store.raw store a.id <> a.local then
              fail "local note changed before upload";
            let s = state store remote in
            let ref_ = function
              | None -> `Null
              | Some raw -> str (Store.keep store raw)
            in
            let p =
              obj
                [
                  ("candidate", str (Store.keep store candidate));
                  ("local", ref_ a.local);
                  ("remote", ref_ a.remote);
                  ( "etag",
                    opt_string (Option.bind observed (fun f -> f.Remote.etag))
                  );
                ]
            in
            Store.save_state store "sync.json"
              (set "pending" (set a.id p (get "pending" s)) s));
        let written =
          try
            Remote.put remote
              ~path:("notes/" ^ a.id ^ ".md")
              ~previous:observed candidate
          with Fetch_dav.Http_error ({ status = 412; _ } as e) ->
            (* A precondition rejection proves this PUT did not take effect. *)
            Store.with_lock store (fun () ->
                let s = state store remote in
                Store.save_state store "sync.json"
                  (set "pending" (remove a.id (get "pending" s)) s));
            raise (Fetch_dav.Http_error e)
        in
        finish store remote ~id:a.id ~local:a.local ~observed:(Some written)
          candidate)
      else finish store remote ~id:a.id ~local:a.local ~observed candidate

let sync_lock store f =
  Store.ensure_state store;
  let path = Store.state_path store "sync.lock" in
  if exists path then regular path;
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_TLOCK 0;
      f ())

let run ?report ~dry_run store remote =
  if (not dry_run) && remote.Remote.readonly then
    fail "read-only remote cannot apply synchronization";
  Option.iter
    (fun p ->
      separate p store.Store.root;
      if exists p then fail "report directory must be new")
    report;
  let work () =
    let s = state store remote in
    let bootstrap_needed =
      bootstrap
        ~allow_init:(not (exists (Store.state_path store "sync.json")))
        ~dry_run store remote
    in
    if (not dry_run) && not (exists (Store.state_path store "sync.json")) then
      Store.with_lock store (fun () -> Store.save_state store "sync.json" s);
    let remote_notes =
      if bootstrap_needed && dry_run then [] else Remote.notes remote
    in
    let local_notes, errors = Store.scan store in
    let ids =
      List.sort_uniq compare
        (List.map Doc.id local_notes
        @ List.map fst remote_notes
        @ keys (get "bases" s)
        @ keys (get "pending" s))
    in
    let actions =
      List.map
        (fun id ->
          let observed = List.assoc_opt id remote_notes in
          let local = Store.raw store id in
          let remote_raw = Option.map (fun f -> f.Remote.raw) observed in
          let latest_local = ref local and latest_remote = ref remote_raw in
          let hold reason =
            let local = !latest_local and remote_raw = !latest_remote in
            if not dry_run then
              Store.with_lock store (fun () ->
                  let base =
                    Option.map
                      (fun h -> Store.object_ store (string h))
                      (find id (get "bases" s))
                  in
                  Store.record_conflict store ~id ~base ~local
                    ~remote:remote_raw reason);
            action ~id ~kind:"conflict" ~detail:reason ~local ~remote:remote_raw
              ()
          in
          let rec attempt retries observed =
            let current = if dry_run then s else state store remote in
            let local = Store.raw store id in
            latest_local := local;
            latest_remote := Option.map (fun f -> f.Remote.raw) observed;
            let a = planned store current ~id ~local ~remote:observed in
            if a.kind = "conflict" then hold a.detail
            else (
              if a.kind = "push" then
                Option.iter
                  (fun (f : Remote.file) ->
                    match f.etag with
                    | Some e -> ignore (Remote.etag e)
                    | None -> fail "remote replacement requires an ETag")
                  observed;
              try
                if not dry_run then apply_one store remote a observed;
                a
              with
              | Fetch_dav.Http_error { status = 412; _ } when retries > 0 ->
                attempt (retries - 1)
                  (Remote.get remote ("notes/" ^ id ^ ".md")))
          in
          try attempt 2 observed with exn -> hold (error_message exn))
        ids
    in
    let diagnostics =
      List.map
        (fun (name, detail) ->
          action ~id:name ~kind:"invalid" ~detail ~local:None ~remote:None ())
        errors
    in
    let actions =
      (if bootstrap_needed then
         [
           action ~id:"store"
             ~kind:(if dry_run then "initialize" else "initialized")
             ~local:None ~remote:None ();
         ]
       else [])
      @ actions @ diagnostics
    in
    Option.iter (fun path -> write_report store path actions) report;
    actions
  in
  if dry_run then work () else sync_lock store work

let clone ~root remote =
  if exists root && Array.length (Sys.readdir root) <> 0 then
    fail "clone root must be absent or empty";
  let meta =
    match Remote.get remote "store.json" with
    | Some f -> f
    | None -> fail "remote store is not initialized"
  in
  let id = Store.parse_metadata meta.raw in
  (* Validate and fetch everything before making a local store. *)
  let notes = Remote.notes remote in
  List.iter (fun (id, f) -> ignore (parse id f.Remote.raw)) notes;
  let store = Store.init ~id root in
  Store.with_lock store (fun () ->
      let bases =
        List.fold_left
          (fun acc (id, f) ->
            Store.install store ~id ~expected:None f.Remote.raw;
            set id (str (Store.keep store f.raw)) acc)
          (obj []) notes
      in
      Store.save_state store "sync.json"
        (set "bases" bases (initial store remote)));
  store

let resolve store remote ~id ~expected ~file =
  let candidate = Doc.parse (read file) in
  if Doc.id candidate <> id then fail "resolution file has a different task ID";
  sync_lock store (fun () ->
      ignore (bootstrap ~allow_init:false ~dry_run:true store remote);
      let observed = Remote.get remote ("notes/" ^ id ^ ".md") in
      let local = Store.raw store id in
      if Option.map digest local <> expected then
        fail "resolution local revision changed";
      Option.iter (fun raw -> Doc.immutable (Doc.parse raw) candidate) local;
      let s = state store remote in
      Option.iter
        (fun h ->
          Doc.immutable (Doc.parse (Store.object_ store (string h))) candidate)
        (find id (get "bases" s));
      let remote_raw = Option.map (fun f -> f.Remote.raw) observed in
      let kind =
        if remote_raw = Some candidate.raw then "pull"
        else if observed = None then "create"
        else "push"
      in
      let a =
        action ~id ~kind ~local ~remote:remote_raw ~candidate:candidate.raw ()
      in
      apply_one store remote a observed;
      a)
