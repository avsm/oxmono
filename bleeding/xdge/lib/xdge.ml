(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type source = Default | Env of string | Cmdline

type t = {
  app_name : string;
  config_dir : Eio.Fs.dir_ty Eio.Path.t;
  config_dir_source : source;
  data_dir : Eio.Fs.dir_ty Eio.Path.t;
  data_dir_source : source;
  cache_dir : Eio.Fs.dir_ty Eio.Path.t;
  cache_dir_source : source;
  state_dir : Eio.Fs.dir_ty Eio.Path.t;
  state_dir_source : source;
  runtime_dir : Eio.Fs.dir_ty Eio.Path.t option;
  runtime_dir_source : source;
  config_dirs : Eio.Fs.dir_ty Eio.Path.t list;
  data_dirs : Eio.Fs.dir_ty Eio.Path.t list;
}

let ensure_dir ?(perm = 0o755) path = Eio.Path.mkdirs ~exists_ok:true ~perm path

let validate_runtime_base_dir base_path =
  (* Validate the base XDG_RUNTIME_DIR has correct permissions per spec *)
  try
    let path_str = Eio.Path.native_exn base_path in
    let stat = Eio.Path.stat ~follow:true base_path in
    let current_perm = stat.perm land 0o777 in
    if current_perm <> 0o700 then
      failwith
        (Printf.sprintf
           "XDG_RUNTIME_DIR base directory %s has incorrect permissions: %o \
            (must be 0700)"
           path_str current_perm);
    (* Check ownership - directory should be owned by current user *)
    let uid = Unix.getuid () in
    if stat.uid <> Int64.of_int uid then
      failwith
        (Printf.sprintf
           "XDG_RUNTIME_DIR base directory %s not owned by current user (uid \
            %d, owner %Ld)"
           path_str uid stat.uid)
    (* TODO: Check that directory is on local filesystem (not networked).
       This would require filesystem type detection which is OS-specific. *)
  with exn ->
    failwith
      (Printf.sprintf "Cannot validate XDG_RUNTIME_DIR: %s"
         (Printexc.to_string exn))

let ensure_runtime_dir _fs app_runtime_path =
  (* Base directory validation is done in resolve_runtime_dir, 
     so we just create the app subdirectory *)
  ensure_dir app_runtime_path

let get_home_dir fs =
  let home_str =
    match Sys.getenv_opt "HOME" with
    | Some home -> home
    | None -> (
        match Sys.os_type with
        | "Win32" | "Cygwin" -> (
            match Sys.getenv_opt "USERPROFILE" with
            | Some profile -> profile
            | None -> failwith "Cannot determine home directory")
        | _ -> (
            try Unix.((getpwuid (getuid ())).pw_dir)
            with _ -> failwith "Cannot determine home directory"))
  in
  Eio.Path.(fs / home_str)

let make_env_var_name app_name suffix =
  String.uppercase_ascii app_name ^ "_" ^ suffix

exception Invalid_xdg_path of string

let validate_absolute_path context path =
  if Filename.is_relative path then
    raise
      (Invalid_xdg_path
         (Printf.sprintf "%s must be an absolute path, got: %s" context path))

let resolve_path fs home_path base_path =
  if Filename.is_relative base_path then Eio.Path.(home_path / base_path)
  else Eio.Path.(fs / base_path)

(* Helper to resolve system directories (config_dirs or data_dirs) *)
let resolve_system_dirs fs home_path app_name override_suffix xdg_var
    default_paths =
  let override_var = make_env_var_name app_name override_suffix in
  match Sys.getenv_opt override_var with
  | Some dirs when dirs <> "" ->
      String.split_on_char ':' dirs
      |> List.filter (fun s -> s <> "")
      |> List.filter_map (fun path ->
          try
            validate_absolute_path override_var path;
            Some Eio.Path.(resolve_path fs home_path path / app_name)
          with Invalid_xdg_path _ -> None)
  | Some _ | None -> (
      match Sys.getenv_opt xdg_var with
      | Some dirs when dirs <> "" ->
          String.split_on_char ':' dirs
          |> List.filter (fun s -> s <> "")
          |> List.filter_map (fun path ->
              try
                validate_absolute_path xdg_var path;
                Some Eio.Path.(resolve_path fs home_path path / app_name)
              with Invalid_xdg_path _ -> None)
      | Some _ | None ->
          List.map
            (fun path -> Eio.Path.(resolve_path fs home_path path / app_name))
            default_paths)

(* Helper to resolve a user directory with override precedence *)
let resolve_user_dir fs home_path app_name xdg_ctx xdg_getter override_suffix =
  let override_var = make_env_var_name app_name override_suffix in
  match Sys.getenv_opt override_var with
  | Some dir when dir <> "" ->
      validate_absolute_path override_var dir;
      (Eio.Path.(fs / dir / app_name), Env override_var)
  | Some _ | None ->
      let xdg_base = xdg_getter xdg_ctx in
      let base_path = resolve_path fs home_path xdg_base in
      (Eio.Path.(base_path / app_name), Default)

(* Helper to resolve runtime directory (special case since it can be None) *)
let resolve_runtime_dir fs home_path app_name xdg_ctx =
  let override_var = make_env_var_name app_name "RUNTIME_DIR" in
  match Sys.getenv_opt override_var with
  | Some dir when dir <> "" ->
      validate_absolute_path override_var dir;
      (* Validate the base runtime directory has correct permissions *)
      let base_runtime_dir = resolve_path fs home_path dir in
      validate_runtime_base_dir base_runtime_dir;
      (Some Eio.Path.(base_runtime_dir / app_name), Env override_var)
  | Some _ | None ->
      ( (match Xdg.runtime_dir xdg_ctx with
        | Some base ->
            (* Validate the base runtime directory has correct permissions *)
            let base_runtime_dir = resolve_path fs home_path base in
            validate_runtime_base_dir base_runtime_dir;
            Some Eio.Path.(base_runtime_dir / app_name)
        | None -> None),
        Default )

let validate_standard_xdg_vars () =
  (* Validate standard XDG environment variables for absolute paths *)
  let xdg_vars =
    [
      "XDG_CONFIG_HOME";
      "XDG_DATA_HOME";
      "XDG_CACHE_HOME";
      "XDG_STATE_HOME";
      "XDG_RUNTIME_DIR";
      "XDG_CONFIG_DIRS";
      "XDG_DATA_DIRS";
    ]
  in
  List.iter
    (fun var ->
      match Sys.getenv_opt var with
      | Some value when value <> "" ->
          if String.contains value ':' then
            (* Colon-separated list - validate each part *)
            String.split_on_char ':' value
            |> List.filter (fun s -> s <> "")
            |> List.iter (fun path -> validate_absolute_path var path)
          else
            (* Single path *)
            validate_absolute_path var value
      | _ -> ())
    xdg_vars

let create fs app_name =
  let fs = fs in
  let home_path = get_home_dir fs in
  (* First validate all standard XDG environment variables *)
  validate_standard_xdg_vars ();
  let xdg_ctx = Xdg.create ~env:Sys.getenv_opt () in
  (* User directories *)
  let config_dir, config_dir_source =
    resolve_user_dir fs home_path app_name xdg_ctx Xdg.config_dir "CONFIG_DIR"
  in
  let data_dir, data_dir_source =
    resolve_user_dir fs home_path app_name xdg_ctx Xdg.data_dir "DATA_DIR"
  in
  let cache_dir, cache_dir_source =
    resolve_user_dir fs home_path app_name xdg_ctx Xdg.cache_dir "CACHE_DIR"
  in
  let state_dir, state_dir_source =
    resolve_user_dir fs home_path app_name xdg_ctx Xdg.state_dir "STATE_DIR"
  in
  (* Runtime directory *)
  let runtime_dir, runtime_dir_source =
    resolve_runtime_dir fs home_path app_name xdg_ctx
  in
  (* System directories *)
  let config_dirs =
    resolve_system_dirs fs home_path app_name "CONFIG_DIRS" "XDG_CONFIG_DIRS"
      [ "/etc/xdg" ]
  in
  let data_dirs =
    resolve_system_dirs fs home_path app_name "DATA_DIRS" "XDG_DATA_DIRS"
      [ "/usr/local/share"; "/usr/share" ]
  in
  ensure_dir config_dir;
  ensure_dir data_dir;
  ensure_dir cache_dir;
  ensure_dir state_dir;
  Option.iter (ensure_runtime_dir fs) runtime_dir;
  {
    app_name;
    config_dir;
    config_dir_source;
    data_dir;
    data_dir_source;
    cache_dir;
    cache_dir_source;
    state_dir;
    state_dir_source;
    runtime_dir;
    runtime_dir_source;
    config_dirs;
    data_dirs;
  }

let app_name t = t.app_name
let config_dir t = t.config_dir
let data_dir t = t.data_dir
let cache_dir t = t.cache_dir
let state_dir t = t.state_dir
let runtime_dir t = t.runtime_dir
let config_dirs t = t.config_dirs
let data_dirs t = t.data_dirs

(* Check if an Eio exception indicates a missing file/directory *)
let is_not_found_error = function
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true
  | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> true
  | _ -> false

(* File search following XDG specification *)
let find_file_in_dirs dirs filename =
  let rec search_dirs = function
    | [] -> None
    | dir :: remaining_dirs -> (
        let file_path = Eio.Path.(dir / filename) in
        try
          (* Try to check if file exists and is readable *)
          let _ = Eio.Path.stat ~follow:true file_path in
          Some file_path
        with exn when is_not_found_error exn ->
          (* File is inaccessible (non-existent, permissions, etc.)
            Skip and continue with next directory per XDG spec *)
          search_dirs remaining_dirs)
  in
  search_dirs dirs

let find_config_file t filename =
  (* Search user config dir first, then system config dirs *)
  find_file_in_dirs (t.config_dir :: t.config_dirs) filename

let find_data_file t filename =
  (* Search user data dir first, then system data dirs *)
  find_file_in_dirs (t.data_dir :: t.data_dirs) filename

let pp ?(brief = false) ?(sources = false) ppf t =
  let pp_source ppf = function
    | Default -> Fmt.(styled `Faint string) ppf "default"
    | Env var -> Fmt.pf ppf "%a" Fmt.(styled `Yellow string) ("env(" ^ var ^ ")")
    | Cmdline -> Fmt.(styled `Blue string) ppf "cmdline"
  in
  let pp_path_with_source ppf path source =
    if sources then
      Fmt.pf ppf "%a %a"
        Fmt.(styled `Green Eio.Path.pp)
        path
        Fmt.(styled `Faint (brackets pp_source))
        source
    else Fmt.(styled `Green Eio.Path.pp) ppf path
  in
  let pp_path_opt_with_source ppf path_opt source =
    match path_opt with
    | None ->
        if sources then
          Fmt.pf ppf "%a %a"
            Fmt.(styled `Red string)
            "<none>"
            Fmt.(styled `Faint (brackets pp_source))
            source
        else Fmt.(styled `Red string) ppf "<none>"
    | Some path -> pp_path_with_source ppf path source
  in
  let pp_paths ppf paths =
    Fmt.(list ~sep:(any ";@ ") (styled `Green Eio.Path.pp)) ppf paths
  in
  if brief then
    Fmt.pf ppf "%a config=%a data=%a>"
      Fmt.(styled `Cyan string)
      ("<xdg:" ^ t.app_name)
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.config_dir, t.config_dir_source)
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.data_dir, t.data_dir_source)
  else (
    Fmt.pf ppf "@[<v>%a@,"
      Fmt.(styled `Bold string)
      ("XDG directories for '" ^ t.app_name ^ "':");
    Fmt.pf ppf "@[<v 2>%a@," Fmt.(styled `Bold string) "User directories:";
    Fmt.pf ppf "%a %a@,"
      Fmt.(styled `Cyan string)
      "config:"
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.config_dir, t.config_dir_source);
    Fmt.pf ppf "%a %a@,"
      Fmt.(styled `Cyan string)
      "data:"
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.data_dir, t.data_dir_source);
    Fmt.pf ppf "%a %a@,"
      Fmt.(styled `Cyan string)
      "cache:"
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.cache_dir, t.cache_dir_source);
    Fmt.pf ppf "%a %a@,"
      Fmt.(styled `Cyan string)
      "state:"
      (fun ppf (path, source) -> pp_path_with_source ppf path source)
      (t.state_dir, t.state_dir_source);
    Fmt.pf ppf "%a %a@]@,"
      Fmt.(styled `Cyan string)
      "runtime:"
      (fun ppf (path_opt, source) ->
        pp_path_opt_with_source ppf path_opt source)
      (t.runtime_dir, t.runtime_dir_source);
    Fmt.pf ppf "@[<v 2>%a@," Fmt.(styled `Bold string) "System directories:";
    Fmt.pf ppf "%a [@[<hov>%a@]]@,"
      Fmt.(styled `Cyan string)
      "config_dirs:" pp_paths t.config_dirs;
    Fmt.pf ppf "%a [@[<hov>%a@]]@]@]"
      Fmt.(styled `Cyan string)
      "data_dirs:" pp_paths t.data_dirs)

module Cmd = struct
  type xdg_t = t
  type 'a with_source = { value : 'a option; source : source }

  type t = {
    config_dir : string with_source;
    data_dir : string with_source;
    cache_dir : string with_source;
    state_dir : string with_source;
    runtime_dir : string with_source;
  }

  type dir = [ `Config | `Cache | `Data | `State | `Runtime ]

  let term app_name fs ?(dirs = [ `Config; `Data; `Cache; `State; `Runtime ]) ()
      =
    let open Cmdliner in
    let app_upper = String.uppercase_ascii app_name in
    let show_paths =
      let doc = "Show only the resolved directory paths without formatting" in
      Arg.(value & flag & info [ "show-paths" ] ~doc)
    in
    let has_dir d = List.mem d dirs in
    let make_dir_arg ~enabled name env_suffix xdg_var default_path =
      if not enabled then
        (* Return a term that always gives the environment-only result *)
        Term.(
          const (fun () ->
              let app_env = app_upper ^ "_" ^ env_suffix in
              match Sys.getenv_opt app_env with
              | Some v when v <> "" -> { value = Some v; source = Env app_env }
              | Some _ | None -> (
                  match Sys.getenv_opt xdg_var with
                  | Some v -> { value = Some v; source = Env xdg_var }
                  | None -> { value = None; source = Default }))
          $ const ())
      else
        let app_env = app_upper ^ "_" ^ env_suffix in
        let doc =
          match default_path with
          | Some path ->
              Printf.sprintf
                "Override %s directory. Can also be set with %s or %s. \
                 Default: %s"
                name app_env xdg_var path
          | None ->
              Printf.sprintf
                "Override %s directory. Can also be set with %s or %s. No \
                 default value."
                name app_env xdg_var
        in
        let arg =
          Arg.(
            value
            & opt (some string) None
            & info [ name ^ "-dir" ] ~docv:"DIR" ~doc)
        in
        Term.(
          const (fun cmdline_val ->
              match cmdline_val with
              | Some v -> { value = Some v; source = Cmdline }
              | None -> (
                  match Sys.getenv_opt app_env with
                  | Some v when v <> "" ->
                      { value = Some v; source = Env app_env }
                  | Some _ | None -> (
                      match Sys.getenv_opt xdg_var with
                      | Some v -> { value = Some v; source = Env xdg_var }
                      | None -> { value = None; source = Default })))
          $ arg)
    in
    let home_prefix = "\\$HOME" in
    let config_dir =
      make_dir_arg ~enabled:(has_dir `Config) "config" "CONFIG_DIR"
        "XDG_CONFIG_HOME"
        (Some (home_prefix ^ "/.config/" ^ app_name))
    in
    let data_dir =
      make_dir_arg ~enabled:(has_dir `Data) "data" "DATA_DIR" "XDG_DATA_HOME"
        (Some (home_prefix ^ "/.local/share/" ^ app_name))
    in
    let cache_dir =
      make_dir_arg ~enabled:(has_dir `Cache) "cache" "CACHE_DIR"
        "XDG_CACHE_HOME"
        (Some (home_prefix ^ "/.cache/" ^ app_name))
    in
    let state_dir =
      make_dir_arg ~enabled:(has_dir `State) "state" "STATE_DIR"
        "XDG_STATE_HOME"
        (Some (home_prefix ^ "/.local/state/" ^ app_name))
    in
    let runtime_dir =
      make_dir_arg ~enabled:(has_dir `Runtime) "runtime" "RUNTIME_DIR"
        "XDG_RUNTIME_DIR" None
    in
    Term.(
      const
        (fun
          show_paths_flag
          config_dir_ws
          data_dir_ws
          cache_dir_ws
          state_dir_ws
          runtime_dir_ws
        ->
          let config =
            {
              config_dir = config_dir_ws;
              data_dir = data_dir_ws;
              cache_dir = cache_dir_ws;
              state_dir = state_dir_ws;
              runtime_dir = runtime_dir_ws;
            }
          in
          let home_path = get_home_dir fs in
          (* First validate all standard XDG environment variables *)
          validate_standard_xdg_vars ();
          let xdg_ctx = Xdg.create ~env:Sys.getenv_opt () in
          (* Helper to resolve directory from config with source tracking *)
          let resolve_from_config config_ws xdg_getter =
            match config_ws.value with
            | Some dir -> (resolve_path fs home_path dir, config_ws.source)
            | None ->
                let xdg_base = xdg_getter xdg_ctx in
                let base_path = resolve_path fs home_path xdg_base in
                (Eio.Path.(base_path / app_name), config_ws.source)
          in
          (* User directories *)
          let config_dir, config_dir_source =
            resolve_from_config config.config_dir Xdg.config_dir
          in
          let data_dir, data_dir_source =
            resolve_from_config config.data_dir Xdg.data_dir
          in
          let cache_dir, cache_dir_source =
            resolve_from_config config.cache_dir Xdg.cache_dir
          in
          let state_dir, state_dir_source =
            resolve_from_config config.state_dir Xdg.state_dir
          in
          (* Runtime directory *)
          let runtime_dir, runtime_dir_source =
            match config.runtime_dir.value with
            | Some dir ->
                (Some (resolve_path fs home_path dir), config.runtime_dir.source)
            | None ->
                ( Option.map
                    (fun base ->
                      let base_path = resolve_path fs home_path base in
                      Eio.Path.(base_path / app_name))
                    (Xdg.runtime_dir xdg_ctx),
                  config.runtime_dir.source )
          in
          (* System directories - reuse shared helper *)
          let config_dirs =
            resolve_system_dirs fs home_path app_name "CONFIG_DIRS"
              "XDG_CONFIG_DIRS" [ "/etc/xdg" ]
          in
          let data_dirs =
            resolve_system_dirs fs home_path app_name "DATA_DIRS"
              "XDG_DATA_DIRS"
              [ "/usr/local/share"; "/usr/share" ]
          in
          ensure_dir config_dir;
          ensure_dir data_dir;
          ensure_dir cache_dir;
          ensure_dir state_dir;
          Option.iter (ensure_runtime_dir fs) runtime_dir;
          let xdg =
            {
              app_name;
              config_dir;
              config_dir_source;
              data_dir;
              data_dir_source;
              cache_dir;
              cache_dir_source;
              state_dir;
              state_dir_source;
              runtime_dir;
              runtime_dir_source;
              config_dirs;
              data_dirs;
            }
          in
          (* Handle --show-paths option *)
          if show_paths_flag then (
            let print_path name path =
              match path with
              | None -> Printf.printf "%s: <none>\n" name
              | Some p -> Printf.printf "%s: %s\n" name (Eio.Path.native_exn p)
            in
            let print_paths name paths =
              match paths with
              | [] -> Printf.printf "%s: []\n" name
              | paths ->
                  let paths_str =
                    String.concat ":" (List.map Eio.Path.native_exn paths)
                  in
                  Printf.printf "%s: %s\n" name paths_str
            in
            print_path "config_dir" (Some config_dir);
            print_path "data_dir" (Some data_dir);
            print_path "cache_dir" (Some cache_dir);
            print_path "state_dir" (Some state_dir);
            print_path "runtime_dir" runtime_dir;
            print_paths "config_dirs" config_dirs;
            print_paths "data_dirs" data_dirs;
            Stdlib.exit 0);
          (xdg, config))
      $ show_paths $ config_dir $ data_dir $ cache_dir $ state_dir $ runtime_dir)

  let cache_term app_name =
    let open Cmdliner in
    let app_upper = String.uppercase_ascii app_name in
    let app_env = app_upper ^ "_CACHE_DIR" in
    let xdg_var = "XDG_CACHE_HOME" in
    let home = Sys.getenv "HOME" in
    let default_path = home ^ "/.cache/" ^ app_name in

    let doc =
      Printf.sprintf
        "Override cache directory. Can also be set with %s or %s. Default: %s"
        app_env xdg_var default_path
    in

    let arg =
      Arg.(
        value & opt string default_path
        & info [ "cache-dir"; "c" ] ~docv:"DIR" ~doc)
    in

    Term.(
      const (fun cmdline_val ->
          (* Check command line first *)
          if cmdline_val <> default_path then cmdline_val
          else
            (* Then check app-specific env var *)
            match Sys.getenv_opt app_env with
            | Some v when v <> "" -> v
            | _ -> (
                (* Then check XDG env var *)
                match Sys.getenv_opt xdg_var with
                | Some v when v <> "" -> v ^ "/" ^ app_name
                | _ -> default_path))
      $ arg)

  let env_docs app_name =
    let app_upper = String.uppercase_ascii app_name in
    Printf.sprintf
      {|
Configuration Precedence (follows standard Unix conventions):
  1. Command-line flags (e.g., --config-dir) - highest priority
  2. Application-specific environment variable (e.g., %s_CONFIG_DIR)
  3. XDG standard environment variable (e.g., XDG_CONFIG_HOME)
  4. Default path (e.g., ~/.config/%s) - lowest priority
  
  This allows per-application overrides without affecting other XDG-compliant programs.
  For example, setting %s_CONFIG_DIR only changes the config directory for %s,
  while XDG_CONFIG_HOME affects all XDG-compliant applications.

Application-specific variables:
  %s_CONFIG_DIR    Override config directory for %s only
  %s_DATA_DIR      Override data directory for %s only
  %s_CACHE_DIR     Override cache directory for %s only
  %s_STATE_DIR     Override state directory for %s only
  %s_RUNTIME_DIR   Override runtime directory for %s only
  
XDG standard variables (shared by all XDG applications):
  XDG_CONFIG_HOME  User configuration directory (default: ~/.config/%s)
  XDG_DATA_HOME    User data directory (default: ~/.local/share/%s)
  XDG_CACHE_HOME   User cache directory (default: ~/.cache/%s)
  XDG_STATE_HOME   User state directory (default: ~/.local/state/%s)
  XDG_RUNTIME_DIR  User runtime directory (no default)
  XDG_CONFIG_DIRS  System configuration directories (default: /etc/xdg/%s)
  XDG_DATA_DIRS    System data directories (default: /usr/local/share/%s:/usr/share/%s)
|}
      app_upper app_name app_upper app_name app_upper app_name app_upper
      app_name app_upper app_name app_upper app_name app_upper app_name app_name
      app_name app_name app_name app_name app_name app_name

  let pp ppf config =
    let pp_source ppf = function
      | Default -> Fmt.(styled `Faint string) ppf "default"
      | Env var ->
          Fmt.pf ppf "%a" Fmt.(styled `Yellow string) ("env(" ^ var ^ ")")
      | Cmdline -> Fmt.(styled `Blue string) ppf "cmdline"
    in
    let pp_with_source name ppf ws =
      match ws.value with
      | None when ws.source = Default -> ()
      | None ->
          Fmt.pf ppf "@,%a %a %a"
            Fmt.(styled `Cyan string)
            (name ^ ":")
            Fmt.(styled `Red string)
            "<unset>"
            Fmt.(styled `Faint (brackets pp_source))
            ws.source
      | Some value ->
          Fmt.pf ppf "@,%a %a %a"
            Fmt.(styled `Cyan string)
            (name ^ ":")
            Fmt.(styled `Green string)
            value
            Fmt.(styled `Faint (brackets pp_source))
            ws.source
    in
    Fmt.pf ppf "@[<v>%a%a%a%a%a%a@]"
      Fmt.(styled `Bold string)
      "XDG config:"
      (pp_with_source "config_dir")
      config.config_dir
      (pp_with_source "data_dir")
      config.data_dir
      (pp_with_source "cache_dir")
      config.cache_dir
      (pp_with_source "state_dir")
      config.state_dir
      (pp_with_source "runtime_dir")
      config.runtime_dir
end
