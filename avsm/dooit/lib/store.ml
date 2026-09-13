(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type t = { root : string; id : string }

let state_dir t = Filename.concat t.root ".dooit"
let state_path t name = Filename.concat (state_dir t) name

let note_path t id =
  check_uuid id;
  Filename.concat (Filename.concat t.root "notes") (id ^ ".md")

let metadata id = obj [ ("schema", str "dooit.store/v1"); ("id", str id) ]

let parse_metadata raw =
  let v = json raw in
  if field "schema" v <> "dooit.store/v1" then fail "unsupported store schema";
  let id = field "id" v in
  check_uuid id;
  id

let open_ root =
  let id = parse_metadata (read (Filename.concat root "store.json")) in
  { root = absolute root; id }

let ensure_state t =
  mkdir (state_dir t);
  List.iter (fun n -> mkdir (state_path t n)) [ "objects"; "conflicts" ]

let init ?id root =
  mkdir root;
  let path = Filename.concat root "store.json" in
  if exists path then open_ root
  else (
    if
      exists (Filename.concat root "notes")
      && Array.length (Sys.readdir (Filename.concat root "notes")) <> 0
    then fail "cannot initialize a store over existing notes";
    let id = Option.value id ~default:(new_uuid ()) in
    check_uuid id;
    let t = { root = absolute root; id } in
    mkdir (Filename.concat root "notes");
    ensure_state t;
    create_file path (json_string (metadata id) ^ "\n");
    t)

let with_lock t f =
  ensure_state t;
  let path = state_path t "lock" in
  if exists path then regular path;
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_TLOCK 0;
      Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) f)

let object_path t hash =
  if
    String.length hash <> 64
    || not
         (String.for_all
            (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
            hash)
  then fail "invalid object hash";
  Filename.concat (state_path t "objects") hash

let keep t raw =
  let hash = digest raw in
  let path = object_path t hash in
  if exists path then (if read path <> raw then fail "corrupt revision object")
  else atomic_write path raw;
  hash

let object_ t hash =
  let raw = read (object_path t hash) in
  if digest raw <> hash then fail "corrupt revision object";
  raw

let load_state t name default =
  match read_opt (state_path t name) with None -> default | Some s -> json s

let save_state t name v = save_json (state_path t name) v

let get t id =
  let note = Doc.parse (read (note_path t id)) in
  if Doc.id note <> id then fail "filename and note identity disagree";
  note

let raw t id = read_opt (note_path t id)
let revision_opt = Option.map digest

let scan t =
  let dir = Filename.concat t.root "notes" in
  if not (exists dir) then fail "notes directory missing";
  let notes = ref [] and errors = ref [] in
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.iter (fun name ->
      if not (String.ends_with ~suffix:".md" name) then
        errors := (name, "unrecognized file") :: !errors
      else
        let id = String.sub name 0 (String.length name - 3) in
        try notes := get t id :: !notes with
        | Error s -> errors := (name, s) :: !errors
        | Yamlrw.Yamlrw_error _ -> errors := (name, "invalid YAML") :: !errors
        | Unix.Unix_error (e, _, _) ->
            errors := (name, Unix.error_message e) :: !errors);
  (List.rev !notes, List.rev !errors)

let install t ~id ~expected candidate =
  let path = note_path t id in
  let current = read_opt path in
  if revision_opt current <> expected then fail "local revision changed: %s" id;
  Option.iter (fun s -> ignore (keep t s)) current;
  ignore (keep t candidate);
  if expected = None then create_file path candidate
  else
    atomic_write
      ~check:(fun () ->
        if revision_opt (read_opt path) <> expected then
          fail "local revision changed: %s" id)
      path candidate

let apply t ~id ~expected ~operation ~payload change =
  check_uuid operation;
  with_lock t (fun () ->
      let ops = load_state t "operations.json" (obj []) in
      let fingerprint =
        digest
          (id ^ "\n" ^ Option.value expected ~default:"absent" ^ "\n" ^ payload)
      in
      match find operation ops with
      | Some op ->
          if field "request" op <> fingerprint then
            fail "operation UUID already used with different arguments";
          let after = field "after" op in
          let result = Doc.parse (object_ t after) in
          if field "phase" op = "done" then result
          else
            let before = string_opt (Common.get "before" op)
            and current = revision_opt (raw t id) in
            if current = Some after then ()
            else if current = before then
              install t ~id ~expected:before result.raw
            else
              fail
                "interrupted operation conflicts with current note; retained \
                 as %s"
                operation;
            save_state t "operations.json"
              (set operation (set "phase" (str "done") op) ops);
            result
      | None ->
          let before = raw t id in
          if revision_opt before <> expected then
            fail "stale revision for %s (current %s)" id
              (Option.value (revision_opt before) ~default:"absent");
          Option.iter (fun raw -> ignore (keep t raw)) before;
          let candidate = change (Option.map Doc.parse before) in
          if Doc.id candidate <> id then fail "task identity changed";
          Option.iter
            (fun raw -> Doc.immutable (Doc.parse raw) candidate)
            before;
          let after = keep t candidate.raw in
          let op =
            obj
              [
                ("id", str id);
                ("request", str fingerprint);
                ("before", opt_string expected);
                ("after", str after);
                ("phase", str "prepared");
              ]
          in
          save_state t "operations.json" (set operation op ops);
          install t ~id ~expected candidate.raw;
          save_state t "operations.json"
            (set operation (set "phase" (str "done") op) ops);
          candidate)

let add t doc =
  apply t ~id:(Doc.id doc) ~expected:None ~operation:(new_uuid ())
    ~payload:doc.raw (fun _ -> doc)

let modify t id f =
  let note = get t id in
  apply t ~id
    ~expected:(Some (Doc.revision note))
    ~operation:(new_uuid ()) ~payload:"interactive edit"
    (function Some current -> f current | None -> fail "note disappeared")

let recover t =
  with_lock t (fun () ->
      let ops = ref (load_state t "operations.json" (obj [])) in
      List.filter_map
        (fun (operation, op) ->
          if field "phase" op = "done" then None
          else
            let id = field "id" op and after = field "after" op in
            let before = string_opt (Common.get "before" op) in
            let current = revision_opt (raw t id) in
            if current <> before && current <> Some after then
              Some (operation, "current note differs; retained for review")
            else (
              if current <> Some after then
                install t ~id ~expected:before (object_ t after);
              ops := set operation (set "phase" (str "done") op) !ops;
              save_state t "operations.json" !ops;
              Some (operation, "recovered")))
        (assoc !ops))

let conflicts t =
  let path = state_path t "conflicts" in
  if not (exists path) then []
  else
    Sys.readdir path |> Array.to_list |> List.sort compare
    |> List.filter (fun id ->
        valid_uuid id
        && exists (Filename.concat (Filename.concat path id) "current"))

let record_conflict t ~id ~base ~local ~remote reason =
  let dir = Filename.concat (state_path t "conflicts") id in
  mkdir dir;
  let run = Filename.concat dir (new_uuid ()) in
  mkdir run;
  List.iter
    (fun (name, raw) ->
      Option.iter (fun s -> atomic_write (Filename.concat run name) s) raw)
    [ ("base.md", base); ("local.md", local); ("remote.md", remote) ];
  save_json
    (Filename.concat run "conflict.json")
    (obj [ ("reason", str reason); ("created_at", str (now ())) ]);
  atomic_write (Filename.concat dir "current") (Filename.basename run ^ "\n")

let clear_conflict t id =
  let p =
    Filename.concat (Filename.concat (state_path t "conflicts") id) "current"
  in
  if exists p then (
    Unix.unlink p;
    fsync_dir (Filename.dirname p))
