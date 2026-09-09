type t = { dir : Eio.Fs.dir_ty Eio.Path.t }
type entry = { name : string; value : Jsont.json }
type file = { version : int; selected : string option; entries : entry list }

let entry_jsont =
  Jsont.Object.map (fun name value -> { name; value })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun e -> e.name)
  |> Jsont.Object.mem "value" Jsont.json ~enc:(fun e -> e.value)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map (fun version selected entries ->
      { version; selected; entries })
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun f -> f.version)
  |> Jsont.Object.mem "selected" (Jsont.option Jsont.string) ~enc:(fun f ->
      f.selected)
  |> Jsont.Object.mem "entries" (Jsont.list entry_jsont) ~enc:(fun f ->
      f.entries)
  |> Jsont.Object.finish

let validate_name name =
  if
    name = ""
    || String.length name > 64
    || name = "." || name = ".."
    || not
         (String.for_all
            (function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
              | _ -> false)
            name)
  then invalid_arg "Names must contain 1 to 64 ASCII letters, digits, - or _."

let stat path =
  try Some (Unix.lstat (Eio.Path.native_exn path))
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let private_stat kind perm s =
  if s.Unix.st_kind <> kind || s.st_uid <> Unix.getuid () || s.st_perm <> perm
  then
    invalid_arg
      "Secret configuration needs owned directories (0700) and regular files \
       (0600), without symlinks."

let private_dir dir =
  (match stat dir with None -> Eio.Path.mkdir ~perm:0o700 dir | Some _ -> ());
  private_stat Unix.S_DIR 0o700 (Option.get (stat dir))

let with_dir ~sw dir f =
  private_dir dir;
  let dir = Eio.Path.open_subtree ~sw dir in
  let path = Eio.Path.(dir / ".lock") in
  (match stat path with
  | None -> Eio.Path.save ~create:(`Exclusive 0o600) path ""
  | Some s -> private_stat Unix.S_REG 0o600 s);
  let before = Option.get (stat path) in
  let fd =
    Unix.openfile (Eio.Path.native_exn path) [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0
  in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  let after = Unix.fstat fd in
  private_stat Unix.S_REG 0o600 after;
  if before.st_dev <> after.st_dev || before.st_ino <> after.st_ino then
    invalid_arg "Secret configuration lock changed while opening.";
  (try Unix.lockf fd Unix.F_TLOCK 0
   with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
     failwith
       "Secret configuration is in use. Retry after the other command finishes.");
  f { dir }

let with_xdg ~sw ~fs ~profile ~profile_dir f =
  Matrix_client.Profile_store.validate_profile_name profile;
  let config = Xdge.config_dir (Xdge.create fs "crowthebot") in
  let native = Unix.realpath (Eio.Path.native_exn config) in
  let data = Unix.realpath (Eio.Path.native_exn profile_dir) in
  let profiles = Filename.dirname data in
  if native = profiles || String.starts_with ~prefix:(profiles ^ "/") native
  then
    invalid_arg
      "Secret configuration must be outside the profile data directory.";
  let base_stat = Unix.stat native in
  if base_stat.st_uid <> Unix.getuid () || base_stat.st_perm land 0o022 <> 0
  then
    invalid_arg
      "The crowthebot config directory must be owned by you and not writable \
       by others.";
  let root = Eio.Path.(config / "secrets") in
  private_dir root;
  with_dir ~sw Eio.Path.(root / profile) f

let path t tool =
  validate_name tool;
  Eio.Path.(t.dir / (tool ^ ".json"))

let load t tool =
  let path = path t tool in
  match stat path with
  | None -> { version = 1; selected = None; entries = [] }
  | Some s ->
      private_stat Unix.S_REG 0o600 s;
      if s.st_size > 1024 * 1024 then
        invalid_arg "Secret configuration is too large.";
      let file =
        match Jsont_bytesrw.decode_string jsont (Eio.Path.load path) with
        | Ok file -> file
        | Error _ -> invalid_arg "Invalid secret configuration file."
      in
      if file.version <> 1 then
        invalid_arg "Unsupported secret configuration version.";
      List.iter (fun e -> validate_name e.name) file.entries;
      let names = List.map (fun e -> e.name) file.entries in
      if
        List.length names <> List.length (List.sort_uniq String.compare names)
        || Option.fold ~none:false
             ~some:(fun n -> not (List.mem n names))
             file.selected
      then invalid_arg "Invalid named secret configuration.";
      file

let save t tool file =
  let encoded =
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont file with
    | Ok value when String.length value <= 1024 * 1024 -> value ^ "\n"
    | _ -> invalid_arg "Cannot encode secret configuration."
  in
  let target = path t tool in
  let tmp = Eio.Path.(t.dir / (tool ^ ".pending")) in
  (* A fixed temporary name is safe under the operator lock. A stale file
     from an interrupted write is validated before it is removed. *)
  (match stat tmp with
  | None -> ()
  | Some s ->
      private_stat Unix.S_REG 0o600 s;
      Eio.Path.unlink tmp);
  Eio.Path.save ~create:(`Exclusive 0o600) tmp encoded;
  Fun.protect ~finally:(fun () -> if stat tmp <> None then Eio.Path.unlink tmp)
  @@ fun () -> Eio.Path.rename tmp target

let list t ~tool =
  let file = load t tool in
  List.map (fun e -> (e.name, file.selected = Some e.name)) file.entries
  |> List.sort compare

let get t ~tool ~name =
  validate_name name;
  List.find_opt (fun e -> e.name = name) (load t tool).entries
  |> Option.map (fun e -> e.value)

let selected t ~tool =
  let file = load t tool in
  Option.map
    (fun name ->
      (name, (List.find (fun e -> e.name = name) file.entries).value))
    file.selected

let put t ~tool ~name ~replace value =
  validate_name name;
  let file = load t tool in
  let exists = List.exists (fun e -> e.name = name) file.entries in
  if exists && not replace then
    invalid_arg "Name already exists. Use set to replace it.";
  let selected = if file.entries = [] then Some name else file.selected in
  save t tool
    {
      file with
      selected;
      entries =
        { name; value } :: List.filter (fun e -> e.name <> name) file.entries;
    }

let require file name =
  validate_name name;
  if not (List.exists (fun e -> e.name = name) file.entries) then
    invalid_arg "No configuration with that name."

let remove t ~tool ~name =
  let file = load t tool in
  require file name;
  save t tool
    {
      file with
      selected = (if file.selected = Some name then None else file.selected);
      entries = List.filter (fun e -> e.name <> name) file.entries;
    }

let rename t ~tool ~name ~into =
  let file = load t tool in
  require file name;
  validate_name into;
  if List.exists (fun e -> e.name = into) file.entries then
    invalid_arg "Destination name already exists.";
  save t tool
    {
      file with
      selected =
        (if file.selected = Some name then Some into else file.selected);
      entries =
        List.map
          (fun e -> if e.name = name then { e with name = into } else e)
          file.entries;
    }

let select t ~tool ~name =
  let file = load t tool in
  require file name;
  save t tool { file with selected = Some name }
