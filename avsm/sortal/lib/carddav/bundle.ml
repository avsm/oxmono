(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let contact raw =
  ignore (Document.contact raw);
  Document.value raw

let verify ?source bundle =
  let manifest = load_json (Filename.concat bundle "manifest.json") in
  let version = number (get "version" manifest) in
  if version <> 3 then fail "requires a native vCard bundle (version 3)";
  let originals = Filename.concat bundle "originals" in
  if Store.identity originals <> field "store_id" manifest then
    fail "snapshot store identity differs from manifest";
  let files = assoc (get "files" manifest) in
  let names = List.sort String.compare (List.map fst files) in
  if inventory originals <> names then
    fail "snapshot inventory differs from manifest";
  Option.iter
    (fun source ->
      let present = inventory source in
      if not (List.for_all (fun name -> List.mem name names) present) then
        fail "source file inventory changed")
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
          if exists (safe_path source name) && read (safe_path source name) <> raw then
            fail "source differs from snapshot: %s" name)
        source)
    files;
  let seen = Hashtbl.create 512 and handles = Hashtbl.create 512 in
  let photo_names = Hashtbl.create 128 in
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
        if one "X-SORTAL-ID" <> handle then fail "Sortal handle mismatch";
        let raw = read (safe_path originals (field "source" entry)) in
        let c = contact raw in
        List.iter (fun (name, _) -> Hashtbl.replace photo_names name ())
          (snd (Mapping.decode raw));
        let normalized = Mapping.without_store raw in
        if Document.edit ~originals raw (Document.contact raw) <> normalized
        then fail "typed contact round-trip changed the vCard";
        if Mapping.without_store data <> normalized then
          fail "archived vCard differs from active card";
        if List.exists (fun p -> p.Mapping.name = "X-SORTAL-META") props then
          fail "serialized contact payload is forbidden";
        let decoded, photos = Mapping.decode data in
        if not (equal decoded c) then fail "vCard fields differ";
        List.iter
          (fun (p, bytes) ->
            if bytes <> read (safe_path originals p) then
              fail "embedded photo differs: %s" p)
          photos;
        data)
      (items "contacts" manifest)
  in
  Option.iter
    (fun source ->
      let present = inventory source in
      List.iter
        (fun name ->
          if not (List.mem name present) && not (Hashtbl.mem photo_names name)
          then fail "snapshot omitted a non-photo source file: %s" name)
        names)
    source;
  if read (Filename.concat bundle "contacts.vcf") <> String.concat "" cards then
    fail "combined vCard file differs";
  manifest

let export ?previous ~source ~output () =
  let source = absolute source and output = absolute output in
  mkdir (Filename.dirname output);
  fresh output;
  separate output [ source ];
  let store_id = Store.identity source in
  Option.iter
    (fun p ->
      if field "store_id" (verify p) <> store_id then
        fail "previous snapshot belongs to another local store")
    previous;
  let files = inventory source in
  let contact_files =
    List.filter
      (fun p -> Filename.dirname p = "cards" && Filename.extension p = ".vcf")
      files
  in
  Unix.mkdir output 0o700;
  try
    let originals = Filename.concat output "originals" in
    Unix.mkdir originals 0o700;
    Unix.mkdir (Filename.concat output "cards") 0o700;
    let file_entries = ref (
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
        files)
    in
    List.iter
      (fun name ->
        let raw = read (safe_path source name) in
        List.iter
          (fun (photo, bytes) ->
            let dst = safe_path originals photo in
            if not (exists dst) then (
              mkdir (Filename.dirname dst);
              write dst bytes;
              file_entries :=
                ( photo,
                  obj
                    [
                      ("bytes", int (String.length bytes));
                      ("sha256", str (digest bytes));
                    ] )
                :: !file_entries))
          (snd (Mapping.decode raw)))
      contact_files;
    let handles = Hashtbl.create 512 and cards = Buffer.create 4096 in
    let contacts =
      List.map
        (fun name ->
          let c = contact (read (safe_path originals name)) in
          let handle = field "handle" c in
          if handle = "" || Hashtbl.mem handles handle then
            fail "duplicate or empty contact handle: %s" handle;
          Hashtbl.add handles handle ();
          let data = Mapping.without_store (read (safe_path originals name)) in
          let props = Mapping.parse data in
          let uid = Mapping.untext (Mapping.only props "UID").value in
          let path = "cards/" ^ uid ^ ".vcf" in
          if name <> path then fail "vCard filename differs from UID";
          write (safe_path output path) data;
          Buffer.add_string cards data;
          obj
            [
              ("handle", str handle);
              ("uid", str uid);
              ("source", str name);
              ("card", str path);
              ("sha256", str (digest data));
              ("warnings", arr []);
            ])
        contact_files
    in
    let manifest =
      obj
        [
          ("version", int 3);
          ("store_id", str store_id);
          ("as_of", str (today ()));
          ("source", str source);
          ("files", obj !file_entries);
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
