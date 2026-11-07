type binding = string * string option

external setenv : string -> string -> unit = "xdge_testenv_setenv"
external unsetenv : string -> unit = "xdge_testenv_unsetenv"

let set name = function Some v -> setenv name v | None -> unsetenv name

let with_bindings bindings f =
  let previous =
    List.map (fun (name, _) -> (name, Sys.getenv_opt name)) bindings
  in
  List.iter (fun (name, value) -> set name value) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (name, value) -> set name value) previous)
    f

let xdg_variables =
  [
    "HOME";
    "XDG_CONFIG_HOME";
    "XDG_DATA_HOME";
    "XDG_CACHE_HOME";
    "XDG_STATE_HOME";
    "XDG_RUNTIME_DIR";
    "XDG_CONFIG_DIRS";
    "XDG_DATA_DIRS";
  ]

(* Entries are classified with [lstat], so a symbolic link is unlinked and
   never followed, including links that point outside [root]. *)
let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun entry -> remove_tree (Filename.concat path entry))
        (Sys.readdir path);
      Sys.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error _ -> ()

let with_xdg ?(prefix = "testenv-") f =
  let root = Filename.temp_dir ~perms:0o700 prefix "" in
  let sub ?(perm = 0o755) name =
    let dir = Filename.concat root name in
    Sys.mkdir dir perm;
    dir
  in
  let bindings =
    [
      ("HOME", Some root);
      ("XDG_CONFIG_HOME", Some (sub "config"));
      ("XDG_DATA_HOME", Some (sub "data"));
      ("XDG_CACHE_HOME", Some (sub "cache"));
      ("XDG_STATE_HOME", Some (sub "state"));
      ("XDG_RUNTIME_DIR", Some (sub ~perm:0o700 "runtime"));
      ("XDG_CONFIG_DIRS", None);
      ("XDG_DATA_DIRS", None);
    ]
  in
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () -> with_bindings bindings (fun () -> f root))
