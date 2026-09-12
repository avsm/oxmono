(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let contact raw =
  let c = yaml raw in
  ignore (get_ok (Yamlt.decode_string Sortal_schema.Contact.json_t raw));
  c

let verify ?source bundle =
  let manifest = load_json (Filename.concat bundle "manifest.json") in
  let version = number (get "version" manifest) in
  if not (List.mem version [ 1; 2 ]) then fail "unsupported manifest version";
  let originals = Filename.concat bundle "originals" in
  let files = assoc (get "files" manifest) in
  let names = List.sort String.compare (List.map fst files) in
  if inventory originals <> names then
    fail "snapshot inventory differs from manifest";
  Option.iter
    (fun source ->
      if inventory source <> names then fail "source file inventory changed")
    source;
  List.iter
    (fun (name, entry) ->
      let raw = read (safe_path originals name) in
      if
        String.length raw <> number (get "bytes" entry)
        || digest raw <> field "sha256" entry
      then fail "snapshot checksum mismatch: %s" name;
      Option.iter
        (fun source ->
          if read (safe_path source name) <> raw then
            fail "source differs from snapshot: %s" name)
        source)
    files;
  let seen = Hashtbl.create 512 and handles = Hashtbl.create 512 in
  let cards =
    List.map
      (fun entry ->
        let data = read (safe_path bundle (field "card" entry)) in
        if digest data <> field "sha256" entry then
          fail "vCard checksum mismatch";
        let props = Mapping.parse data in
        let one name = Mapping.untext (Mapping.only props name).value in
        let uid = field "uid" entry and handle = field "handle" entry in
        if
          one "UID" <> uid || Hashtbl.mem seen uid || Hashtbl.mem handles handle
        then fail "UID/handle mismatch or duplicate";
        Hashtbl.add seen uid ();
        Hashtbl.add handles handle ();
        ignore (safe_path bundle (uid ^ ".vcf"));
        if
          one "X-SORTAL-ID" <> handle
          || one "X-SORTAL-STORE" <> field "store_id" manifest
        then fail "Sortal identity mismatch";
        let raw = read (safe_path originals (field "source" entry)) in
        let c = contact raw in
        if version = 1 then (
          let meta = json (one "X-SORTAL-META") in
          if
            number (get "version" meta) <> 1
            || field "source" meta <> field "source" entry
            || field "yaml" meta <> raw
            || field "sha256" meta <> digest raw
          then fail "embedded source does not round-trip";
          Option.iter
            (fun p ->
              let photo = string p in
              if not (String.starts_with ~prefix:"http" photo) then
                let bytes = Base64.decode_exn (one "PHOTO") in
                if bytes <> read (safe_path originals photo) then
                  fail "embedded photo differs")
            (find "photo" c))
        else (
          if List.exists (fun p -> p.Mapping.name = "X-SORTAL-META") props then
            fail "serialized contact payload is forbidden";
          let decoded, photos = Mapping.decode data in
          if not (equal decoded c) then
            fail "%s: mapping does not reconstruct the complete contact"
              (field "source" entry);
          List.iter
            (fun (p, raw) ->
              if raw <> read (safe_path originals p) then
                fail "embedded photo differs")
            photos);
        data)
      (items "contacts" manifest)
  in
  if read (Filename.concat bundle "contacts.vcf") <> String.concat "" cards then
    fail "combined vCard file differs";
  manifest

let export ?previous ?(renames = []) ?as_of ?(version = "3.0") ~source ~output
    () =
  let source = absolute source and output = absolute output in
  fresh output;
  separate output [ source ];
  let old = Option.map (fun p -> verify p) previous in
  let store_id =
    match old with None -> new_uuid () | Some m -> field "store_id" m
  in
  let identities = Hashtbl.create 512 in
  Option.iter
    (fun m ->
      List.iter
        (fun e -> Hashtbl.add identities (field "handle" e) (field "uid" e))
        (items "contacts" m))
    old;
  let renamed = ref [] and targets = ref [] in
  List.iter
    (fun spec ->
      match String.split_on_char '=' spec with
      | [ before; after ]
        when Hashtbl.mem identities before
             && (not (Hashtbl.mem identities after))
             && after <> "" ->
          Hashtbl.add identities after (Hashtbl.find identities before);
          Hashtbl.remove identities before;
          renamed := before :: !renamed;
          targets := after :: !targets
      | _ -> fail "invalid identity rename: %s" spec)
    renames;
  let files = inventory source in
  let contact_files =
    List.filter
      (fun p ->
        (not (String.contains p '/'))
        && List.mem (Filename.extension p) [ ".yaml"; ".yml" ])
      files
  in
  if contact_files = [] then fail "no contact YAML files found";
  Unix.mkdir output 0o700;
  try
    let originals = Filename.concat output "originals" in
    Unix.mkdir originals 0o700;
    Unix.mkdir (Filename.concat output "cards") 0o700;
    let file_entries =
      List.map
        (fun name ->
          let src = safe_path source name in
          let stat = Unix.stat src in
          let raw = read src in
          let dst = safe_path originals name in
          mkdir (Filename.dirname dst);
          write ~mode:stat.st_perm dst raw;
          Unix.chmod dst stat.st_perm;
          Unix.utimes dst stat.st_atime stat.st_mtime;
          ( name,
            obj
              [
                ("bytes", int (String.length raw)); ("sha256", str (digest raw));
              ] ))
        files
    in
    let handles = Hashtbl.create 512 and cards = Buffer.create 4096 in
    let contacts =
      List.map
        (fun name ->
          let c = contact (read (safe_path originals name)) in
          if number (get "version" c) <> 2 then
            fail "this export requires Sortal V2";
          let handle = field "handle" c in
          if
            handle = "" || Hashtbl.mem handles handle
            || List.mem handle !renamed
          then fail "invalid, duplicate or old renamed handle: %s" handle;
          Hashtbl.add handles handle ();
          let uid =
            match Hashtbl.find_opt identities handle with
            | Some s -> s
            | None -> contact_uuid store_id handle
          in
          let data, warnings =
            Mapping.encode ~version ~uid ~store_id ~originals c
          in
          let path = "cards/" ^ uid ^ ".vcf" in
          write (safe_path output path) data;
          Buffer.add_string cards data;
          obj
            [
              ("handle", str handle);
              ("uid", str uid);
              ("source", str name);
              ("card", str path);
              ("sha256", str (digest data));
              ("warnings", arr (List.map str warnings));
            ])
        contact_files
    in
    List.iter
      (fun h ->
        if not (Hashtbl.mem handles h) then
          fail "renamed handle absent from source")
      !targets;
    let manifest =
      obj
        [
          ("version", int 2);
          ("vcard_version", str version);
          ("store_id", str store_id);
          ("as_of", str (Option.value ~default:(today ()) as_of));
          ("source", str source);
          ("files", obj file_entries);
          ("contacts", arr contacts);
        ]
    in
    write (Filename.concat output "contacts.vcf") (Buffer.contents cards);
    save_json (Filename.concat output "manifest.json") manifest;
    ignore (verify ~source output);
    manifest
  with exn ->
    remove_tree output;
    raise exn
