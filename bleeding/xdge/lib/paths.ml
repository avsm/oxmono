exception Invalid_xdg_path of string

let validate_absolute context path =
  if Filename.is_relative path then
    raise
      (Invalid_xdg_path
         (Printf.sprintf "%s must be an absolute path, got: %s" context path))

let home () =
  let path =
    match Sys.getenv_opt "HOME" with
    | Some path when path <> "" -> path
    | _ ->
        if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
          match Sys.getenv_opt "USERPROFILE" with
          | Some path when path <> "" -> path
          | _ -> failwith "Cannot determine home directory"
        else (Unix.getpwuid (Unix.getuid ())).pw_dir
  in
  path

let absolute ~base path =
  if Filename.is_relative path then Filename.concat base path else path

let expand ~base path =
  let path =
    if path = "~" then home ()
    else if String.starts_with ~prefix:"~/" path then
      Filename.concat (home ()) (String.sub path 2 (String.length path - 2))
    else path
  in
  absolute ~base path

let user_base ~app_name ~suffix ~default =
  let variable = String.uppercase_ascii app_name ^ "_" ^ suffix in
  match Sys.getenv_opt variable with
  | Some path when path <> "" ->
      validate_absolute variable path;
      (path, Some variable)
  | _ -> (default (), None)

let base variable fallback =
  match Sys.getenv_opt variable with
  | Some path when path <> "" ->
      validate_absolute variable path;
      path
  | _ -> Filename.concat (home ()) fallback

let config_home () = base "XDG_CONFIG_HOME" ".config"
let data_home () = base "XDG_DATA_HOME" ".local/share"
let cache_home () = base "XDG_CACHE_HOME" ".cache"
let state_home () = base "XDG_STATE_HOME" ".local/state"

let directory suffix default app_name =
  let path, _ = user_base ~app_name ~suffix ~default in
  Filename.concat (absolute ~base:(home ()) path) app_name

let config_dir = directory "CONFIG_DIR" config_home
let data_dir = directory "DATA_DIR" data_home
let cache_dir = directory "CACHE_DIR" cache_home
let state_dir = directory "STATE_DIR" state_home
