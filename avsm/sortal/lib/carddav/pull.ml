(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let editable = [ "FN"; "EMAIL"; "KIND"; "X-ADDRESSBOOKSERVER-KIND" ]

let extra =
  [
    "NICKNAME";
    "TEL";
    "NOTE";
    "BDAY";
    "ANNIVERSARY";
    "CATEGORIES";
    "IMPP";
    "LANG";
    "GENDER";
    "RELATED";
    "KEY";
    "TZ";
    "GEO";
  ]

let wire_header (p : Mapping.property) =
  Mapping.header
    {
      p with
      params =
        List.filter
          (fun (k, _) -> not (String.starts_with ~prefix:"X-SORTAL-" k))
          p.params;
    }

let extra_fields props =
  let xs =
    List.filter_map
      (fun (p : Mapping.property) ->
        if List.mem p.name ("EMAIL" :: extra) then
          Some (wire_header p, str p.value)
        else None)
      props
  in
  let value = obj xs in
  unique value;
  value

let eq_opt a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> equal a b
  | _ -> false

let merge_value base local remote field =
  if eq_opt remote base then local
  else if eq_opt local base || eq_opt local remote then remote
  else fail "concurrent local/remote edits conflict at %s" field

let keys v = List.map fst (assoc v)

let remote_record base old_data new_data uid store_id =
  let old = Mapping.parse old_data and newer = Mapping.parse new_data in
  if Mapping.untext (Mapping.only newer "UID").value <> uid then
    fail "remote UID changed";
  List.iter
    (fun (name, expected) ->
      let values =
        List.filter_map
          (fun (p : Mapping.property) ->
            if p.name = name then Some (Mapping.untext p.value) else None)
          newer
      in
      if values <> [] && values <> [ expected ] then
        fail "conflicting remote Sortal identity")
    [ ("X-SORTAL-ID", field "handle" base); ("X-SORTAL-STORE", store_id) ];
  let fixed props =
    List.filter
      (fun (p : Mapping.property) ->
        not (List.mem p.name (editable @ [ "REV"; "PRODID" ] @ extra)))
      props
  in
  if
    Mapping.signatures ~client:true (fixed old)
    <> Mapping.signatures ~client:true (fixed newer)
  then fail "changes outside the supported pull fields require reconciliation";
  let name = Mapping.untext (Mapping.only newer "FN").value in
  if name = "" then fail "empty primary name";
  let candidate =
    ref (set "names" (arr (str name :: List.tl (items "names" base))) base)
  in
  let kinds =
    List.filter_map
      (fun (p : Mapping.property) ->
        if List.mem p.name [ "KIND"; "X-ADDRESSBOOKSERVER-KIND" ] then
          Some (String.lowercase_ascii p.value)
        else None)
      newer
    |> List.sort_uniq String.compare
  in
  (match kinds with
  | [] -> ()
  | [ "individual" ] -> candidate := set "kind" (str "person") !candidate
  | [ "org" ] -> candidate := set "kind" (str "organization") !candidate
  | _ -> fail "ambiguous remote kind");
  let emails =
    List.filter_map
      (fun (p : Mapping.property) ->
        if p.name = "EMAIL" then Some (str (Mapping.untext p.value)) else None)
      newer
  in
  if List.length emails <> List.length (List.sort_uniq compare emails) then
    fail "duplicate remote email values require reconciliation";
  if emails <> items "emails" base then
    candidate := set "emails" (arr emails) !candidate;
  let before = extra_fields old and after = extra_fields newer in
  let extras = ref (Option.value ~default:(obj []) (find "vcard" base)) in
  List.iter
    (fun header ->
      let old = find header before and newer = find header after in
      if not (eq_opt old newer) then (
        if find header !extras <> None && not (eq_opt (find header !extras) old)
        then fail "existing passthrough value differs from remote baseline";
        extras :=
          match newer with
          | None -> remove header !extras
          | Some v -> set header v !extras))
    (List.sort_uniq String.compare (keys before @ keys after));
  if assoc !extras <> [] || find "vcard" base <> None then
    candidate := set "vcard" !extras !candidate;
  !candidate

let merge_records base local candidate =
  List.fold_left
    (fun result field ->
      match
        merge_value (find field base) (find field local) (find field candidate)
          field
      with
      | None -> remove field result
      | Some v -> set field v result)
    local
    (List.sort_uniq String.compare (keys base @ keys candidate))

let reconcile base local old_data new_data uid store_id =
  merge_records base local (remote_record base old_data new_data uid store_id)

let seed_directory ?seed bundle =
  match seed with
  | Some s -> s
  | None ->
      let native = Filename.concat bundle "seed" in
      if exists (Filename.concat native "report.json") then native
      else Filename.concat bundle "fastmail-seed"

let prepare ?previous ?seed ~bundle ~snapshot ~source ~output () =
  fresh output;
  separate output
    ([
       source;
       snapshot;
       Filename.concat bundle "originals";
       Filename.concat bundle "cards";
     ]
    @ Option.to_list previous);
  let manifest = Bundle.verify bundle in
  let seed_dir = seed_directory ?seed bundle in
  let seed = load_json (Filename.concat seed_dir "report.json") in
  let current = load_json (Filename.concat snapshot "report.json") in
  let same_destination a b =
    field "account" a = field "account" b
    && field "href" (get "book" a) = field "href" (get "book" b)
  in
  if not (same_destination seed current) then
    fail "snapshot belongs to a different destination";
  let uploaded =
    items "results" seed
    |> List.filter (fun r -> field "status" r = "verified")
    |> List.map (fun r -> (field "uid" r, r))
  in
  if
    List.length uploaded
    <> List.length (List.sort_uniq String.compare (List.map fst uploaded))
  then fail "duplicate baseline UID";
  let entries =
    List.map (fun e -> (field "uid" e, e)) (items "contacts" manifest)
  in
  let baselines = Hashtbl.create 32 and visited = Hashtbl.create 8 in
  let rec visit = function
    | None -> ()
    | Some cursor ->
        let cursor = absolute cursor in
        if Hashtbl.mem visited cursor then
          fail "cycle in previous pull journals";
        separate output [ cursor ];
        Hashtbl.add visited cursor ();
        let prior = load_json (Filename.concat cursor "report.json") in
        if
          field "status" prior <> "applied"
          || (not (same_destination seed prior))
          || field "store_id" prior <> field "store_id" manifest
        then
          fail
            "previous pull is incomplete or belongs to a different destination";
        List.iter
          (fun change ->
            let uid = field "uid" change in
            let binding =
              match List.assoc_opt uid uploaded with
              | Some x -> x
              | None -> fail "previous pull has an invalid identity binding"
            in
            if
              field "href" change <> field "href" binding
              || field "status" change <> "applied"
            then fail "previous pull has an invalid identity binding";
            let directory = safe_path cursor uid in
            let remote = read (Filename.concat directory "remote-get.vcf") in
            let name, hash =
              match find "common_sha256" change with
              | Some h -> ("common.yaml", string h)
              | None -> ("after.yaml", field "after_sha256" change)
            in
            let local = read (Filename.concat directory name) in
            if
              digest remote <> field "remote_get_sha256" change
              || digest local <> hash
            then fail "previous pull baseline checksum mismatch";
            if not (Hashtbl.mem baselines uid) then
              Hashtbl.add baselines uid (remote, local))
          (items "changes" prior);
        visit
          (match find "previous_pull" prior with
          | None | Some `Null -> None
          | Some s -> Some (string s))
  in
  visit previous;
  let before_dir = Filename.concat snapshot "before" in
  let remote =
    Sys.readdir before_dir |> Array.to_list
    |> List.filter (fun p -> Filename.check_suffix p ".vcf")
  in
  let remote =
    List.map
      (fun name ->
        let data = read (safe_path before_dir name) in
        (Mapping.untext (Mapping.only (Mapping.parse data) "UID").value, data))
      remote
  in
  let remote_ids = List.map fst remote in
  if
    List.length remote_ids
    <> List.length (List.sort_uniq String.compare remote_ids)
  then fail "duplicate snapshot UID";
  (* Unrelated pre-existing cards may be present on the destination. Changes
     to membership of linked contacts are never interpreted as deletions. *)
  if List.exists (fun (uid, _) -> not (List.mem_assoc uid remote)) uploaded then
    fail "deleted remote contacts require reconciliation";
  let changes =
    List.filter_map
      (fun (uid, binding) ->
        let entry =
          match List.assoc_opt uid entries with
          | Some e -> e
          | None -> fail "baseline UID is absent from export"
        in
        let old_data =
          read (safe_path (Filename.concat seed_dir "after") (uid ^ ".vcf"))
        in
        if digest old_data <> field "sha256" binding then
          fail "upload baseline checksum mismatch";
        let base_raw =
          read
            (safe_path
               (Filename.concat bundle "originals")
               (field "source" entry))
        in
        let old_data, base_raw =
          Option.value ~default:(old_data, base_raw)
            (Hashtbl.find_opt baselines uid)
        in
        let remote_data = List.assoc uid remote in
        if
          Mapping.signatures (Mapping.parse old_data)
          = Mapping.signatures (Mapping.parse remote_data)
        then None
        else
          let target = safe_path source (field "source" entry) in
          if (Unix.lstat target).Unix.st_kind <> Unix.S_REG then
            fail "source contact must be a regular file";
          let before = read target in
          let base = Bundle.contact base_raw
          and local = Bundle.contact before in
          let candidate =
            remote_record base old_data remote_data uid
              (field "store_id" manifest)
          in
          let merged = merge_records base local candidate in
          let common = Yaml_edit.update base_raw candidate
          and after = Yaml_edit.update before merged in
          ignore (Bundle.contact after);
          let projected, _ =
            Mapping.encode ~uid
              ~store_id:(field "store_id" manifest)
              ~originals:source merged
          in
          let decoded, photos = Mapping.decode projected in
          if not (equal decoded merged) then
            fail "merged contact does not survive vCard reverse mapping";
          List.iter
            (fun (p, raw) ->
              if raw <> read (safe_path source p) then
                fail "merged photo differs")
            photos;
          Some
            ( entry,
              binding,
              before,
              after,
              common,
              old_data,
              remote_data,
              projected ))
      uploaded
  in
  Unix.mkdir output 0o700;
  let rows =
    List.map
      (fun (entry, binding, before, after, common, old, remote, projected) ->
        let directory = safe_path output (field "uid" entry) in
        Unix.mkdir directory 0o700;
        List.iter
          (fun (name, data) -> write (Filename.concat directory name) data)
          [
            ("before.yaml", before);
            ("after.yaml", after);
            ("common.yaml", common);
            ("baseline.vcf", old);
            ("remote.vcf", remote);
            ("projected.vcf", projected);
          ];
        obj
          [
            ("uid", get "uid" entry);
            ("handle", get "handle" entry);
            ("source", get "source" entry);
            ("href", get "href" binding);
            ("before_sha256", str (digest before));
            ("after_sha256", str (digest after));
            ("common_sha256", str (digest common));
            ("remote_sha256", str (digest remote));
            ("status", str (if before = after then "unchanged" else "prepared"));
          ])
      changes
  in
  let report =
    obj
      [
        ("version", int 1);
        ("account", get "account" seed);
        ("book", get "book" seed);
        ("source", str (absolute source));
        ("bundle", str (absolute bundle));
        ("seed", str (absolute seed_dir));
        ("snapshot", str (absolute snapshot));
        ( "previous_pull",
          match previous with Some p -> str (absolute p) | None -> `Null );
        ("store_id", get "store_id" manifest);
        ("status", str "prepared");
        ("changes", arr rows);
      ]
  in
  save_json (Filename.concat output "report.json") report;
  report

let apply ~dav ~dry_run ~username output =
  let path = Filename.concat output "report.json" in
  let report = ref (load_json path) in
  if field "account" !report <> username then
    fail "account does not match the prepared pull";
  if not (List.mem (field "status" !report) [ "prepared"; "applied" ]) then
    fail "invalid pull journal status";
  let save () = save_json path !report in
  let record uid updated =
    report :=
      set "changes"
        (arr
           (List.map
              (fun c -> if field "uid" c = uid then updated else c)
              (items "changes" !report)))
        !report;
    save ()
  in
  let updates = ref [] and unchanged = ref [] in
  List.iter
    (fun change ->
      let uid = field "uid" change in
      let directory = safe_path output uid in
      let target = safe_path (field "source" !report) (field "source" change) in
      let before = read (Filename.concat directory "before.yaml")
      and after = read (Filename.concat directory "after.yaml") in
      let remote = read (Filename.concat directory "remote.vcf") in
      if
        digest before <> field "before_sha256" change
        || digest after <> field "after_sha256" change
        || digest remote <> field "remote_sha256" change
      then fail "prepared pull checksum mismatch";
      Option.iter
        (fun h ->
          if digest (read (Filename.concat directory "common.yaml")) <> string h
          then fail "prepared common baseline checksum mismatch")
        (find "common_sha256" change);
      if (Unix.lstat target).Unix.st_kind <> Unix.S_REG then
        fail "source contact must be a regular file";
      if not (List.mem (read target) [ before; after ]) then
        fail "local contact changed after preparation";
      let status, headers, fetched =
        Remote.request dav "GET" (field "href" change)
      in
      if status <> 200 then fail "remote readback failed";
      let etag = Remote.strong_etag headers in
      if
        Mapping.signatures (Mapping.parse fetched)
        <> Mapping.signatures (Mapping.parse remote)
      then fail "remote contact changed after preparation; refetch and replan";
      if read target = after then unchanged := get "source" change :: !unchanged
      else updates := get "source" change :: !updates;
      if not dry_run then (
        write (Filename.concat directory "remote-get.vcf") fetched;
        let change =
          change
          |> set "etag" (str etag)
          |> set "remote_get_sha256" (str (digest fetched))
          |> set "status" (str "applying")
        in
        record uid change;
        (if read target <> after then
           let mode = (Unix.stat target).Unix.st_perm in
           atomic_write ~mode
             ~check:(fun () ->
               if
                 (Unix.lstat target).Unix.st_kind <> Unix.S_REG
                 || read target <> before
               then fail "local contact changed during preparation")
             target after);
        if read target <> after then fail "local readback failed";
        record uid (set "status" (str "applied") change)))
    (items "changes" !report);
  if not dry_run then (
    report := set "status" (str "applied") !report;
    save ());
  obj
    [
      ("dry_run", `Bool dry_run);
      ("local_updates", arr (List.rev !updates));
      ("unchanged", arr (List.rev !unchanged));
    ]
