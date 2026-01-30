open! Core
module Unix = Core_unix
include Filesystem_types

open struct
  let time_ns_to_float time_ns = Time_ns.Span.to_sec (Time_ns.to_span_since_epoch time_ns)
end

let executable_name = lazy (File_path.of_string Sys_unix.executable_name)
let default_temp_dir = lazy (File_path.of_string Filename.temp_dir_name)

include struct
  (* [Filename] wrappers *)

  let realpath_relative_to_cwd path =
    File_path.Absolute.of_string (Filename_unix.realpath (File_path.to_string path))
  ;;

  let realpath_absolute path = realpath_relative_to_cwd (File_path.of_absolute path)

  let realpath path ~relative_to =
    realpath_absolute (File_path.make_absolute path ~under:relative_to)
  ;;
end

include struct
  (* [Sys] wrappers *)

  let exists_exn ?follow_symlinks path =
    Sys_unix.file_exists_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_directory_exn ?follow_symlinks path =
    Sys_unix.is_directory_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_file_exn ?follow_symlinks path =
    Sys_unix.is_file_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_symlink_exn path = Sys_unix.is_symlink_exn (File_path.to_string path)

  let exists ?follow_symlinks path =
    Sys_unix.file_exists ?follow_symlinks (File_path.to_string path)
  ;;

  let is_directory ?follow_symlinks path =
    Sys_unix.is_directory ?follow_symlinks (File_path.to_string path)
  ;;

  let is_file ?follow_symlinks path =
    Sys_unix.is_file ?follow_symlinks (File_path.to_string path)
  ;;

  let is_symlink path = Sys_unix.is_symlink (File_path.to_string path)

  let ls_dir path =
    Sys_unix.ls_dir (File_path.to_string path)
    |> List.map ~f:File_path.Part.of_string
    |> List.sort ~compare:File_path.Part.compare
  ;;
end

include struct
  (* [Unix] wrappers *)

  let unlink path = Unix.unlink (File_path.to_string path)

  let rename ~src ~dst =
    Unix.rename ~src:(File_path.to_string src) ~dst:(File_path.to_string dst)
  ;;

  let mkdir ?(parents = false) ?(perm = File_permissions.t_0755) path =
    let perm = File_permissions.to_int perm in
    if parents
    then Unix.mkdir_p ~perm (File_path.to_string path)
    else Unix.mkdir ~perm (File_path.to_string path)
  ;;

  let rmdir path = Unix.rmdir (File_path.to_string path)
  let chdir path = Unix.chdir (File_path.to_string path)
  let getcwd () = Unix.getcwd () |> File_path.Absolute.of_string
  let stat path = Unix.stat (File_path.to_string path) |> File_stats.of_unix_stats
  let lstat path = Unix.lstat (File_path.to_string path) |> File_stats.of_unix_stats

  let chmod path ~perm =
    Unix.chmod (File_path.to_string path) ~perm:(File_permissions.to_int perm)
  ;;

  let update_timestamps_separately path ~access_time ~modify_time =
    Unix.utimes
      (File_path.to_string path)
      ~access:(time_ns_to_float access_time)
      ~modif:(time_ns_to_float modify_time)
  ;;

  let update_timestamps ?at path =
    let time =
      match at with
      | Some time -> time
      | None -> Time_ns.now ()
    in
    update_timestamps_separately path ~access_time:time ~modify_time:time
  ;;
end

include struct
  (* command-line functionality *)

  let rm ?(force = true) ?(recursive = false) (path : File_path.t) =
    let args =
      [ Option.some_if force "-f"
      ; Option.some_if recursive "-r"
      ; (if String.is_prefix (path :> string) ~prefix:"-" then Some "--" else None)
      ; Some (File_path.to_string path)
      ]
      |> List.filter_opt
    in
    let process_info = Unix.create_process ~prog:"rm" ~args in
    (* [rm] should not expect any input. *)
    Unix.close process_info.stdin;
    (* We are not expecting any output. If [rm] produces any, we discard it, and [rm] may
       fail when it tries to write. *)
    Unix.close process_info.stdout;
    Unix.close process_info.stderr;
    (* We fail if [rm] died due to a signal or with a non-zero exit code. *)
    Unix.waitpid_exn process_info.pid
  ;;
end

include struct
  (* current directory functions *)

  let make_absolute_under_cwd path =
    match File_path.to_variant path with
    | Absolute abspath -> abspath
    | Relative relpath -> File_path.Absolute.append (getcwd ()) relpath
  ;;

  let make_relative_to_cwd path =
    match File_path.to_relative path with
    | Some _ as some -> some
    | None -> File_path.make_relative path ~if_under:(getcwd ())
  ;;

  let make_relative_to_cwd_exn path =
    match File_path.to_relative path with
    | Some relpath -> relpath
    | None -> File_path.make_relative_exn path ~if_under:(getcwd ())
  ;;

  let make_relative_to_cwd_if_possible path =
    if File_path.is_relative path
    then path
    else File_path.make_relative_if_possible path ~if_under:(getcwd ())
  ;;
end

include struct
  (* temporary file/directory functions *)

  (* We write [internal_] versions of functions that all take the same set of required
     arguments. *)

  let internal_create_temp_file ~in_dir ~perm ~prefix ~suffix () =
    File_path.Absolute.of_string
      (Filename_unix.temp_file
         ~perm:(File_permissions.to_int perm)
         ~in_dir:(File_path.Absolute.to_string in_dir)
         prefix
         suffix)
  ;;

  let internal_create_temp_dir ~in_dir ~perm ~prefix ~suffix () =
    File_path.Absolute.of_string
      (Filename_unix.temp_dir
         ~perm:(File_permissions.to_int perm)
         ~in_dir:(File_path.Absolute.to_string in_dir)
         prefix
         suffix)
  ;;

  let with_cleanup ~on_cleanup_error path ~(local_ f) ~(local_ cleanup) =
    Exn.protectx ~f path ~finally:(fun path ->
      try cleanup path with
      | exn ->
        let backtrace = Backtrace.Exn.most_recent_for_exn exn in
        On_cleanup_error.on_cleanup_error
          on_cleanup_error
          ~backtrace
          ~exn
          ~log_s:eprint_s
          ~path
          ~return:Fn.id)
    [@nontail]
  ;;

  let internal_with_temp_file ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    let path = internal_create_temp_file ~in_dir ~perm ~prefix ~suffix () in
    with_cleanup ~on_cleanup_error path ~f ~cleanup:(fun path ->
      (* delete the temp file, ok if it was already deleted *)
      try unlink (File_path.of_absolute path) with
      | Unix.Unix_error (ENOENT, _, _) -> ())
  ;;

  let internal_with_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    let path = internal_create_temp_dir ~in_dir ~perm ~prefix ~suffix () in
    with_cleanup
      path
      ~f
      ~cleanup:(fun path -> File_path.of_absolute path |> rm ~recursive:true)
      ~on_cleanup_error
  ;;

  let internal_within_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    internal_with_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix (fun path ->
      let prev = getcwd () in
      chdir (path :> File_path.t);
      Exn.protect ~f ~finally:(fun () ->
        (* restore the previous working directory before cleaning up the temp dir *)
        chdir (prev :> File_path.t)))
  ;;

  (* We wrap the internal functions with optional arguments uniformly. *)

  let wrap f ~default_perm =
    stage (fun ?in_dir ?(perm = default_perm) ?(prefix = "") ?(suffix = "") arg ->
      let in_dir =
        Option.value_or_thunk in_dir ~default:(fun () -> force default_temp_dir)
        |> make_absolute_under_cwd
      in
      f ~in_dir ~perm ~prefix ~suffix arg)
  ;;

  let create_temp_file =
    unstage (wrap internal_create_temp_file ~default_perm:File_permissions.u_rw)
  ;;

  let create_temp_dir =
    unstage (wrap internal_create_temp_dir ~default_perm:File_permissions.u_rwx)
  ;;

  (* We have to eta-expand the polymorphic functions. *)

  let wrap_with_temp f ~default_perm =
    stage
      (fun
          ?in_dir
          ?(on_cleanup_error = On_cleanup_error.Raise)
          ?(perm = default_perm)
          ?(prefix = "")
          ?(suffix = "")
          arg
        ->
         let in_dir =
           Option.value_or_thunk in_dir ~default:(fun () -> force default_temp_dir)
           |> make_absolute_under_cwd
         in
         f ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix arg)
  ;;

  let with_temp_file ?in_dir =
    unstage
      (wrap_with_temp internal_with_temp_file ~default_perm:File_permissions.u_rw)
      ?in_dir
  ;;

  let with_temp_dir ?in_dir =
    unstage
      (wrap_with_temp internal_with_temp_dir ~default_perm:File_permissions.u_rwx)
      ?in_dir
  ;;

  let within_temp_dir ?in_dir =
    unstage
      (wrap_with_temp internal_within_temp_dir ~default_perm:File_permissions.u_rwx)
      ?in_dir
  ;;
end

include struct
  (* file i/o *)

  let read_file path = In_channel.read_all (File_path.to_string path)

  let write_file ?(perm = File_permissions.u_rw) path ~contents =
    let temp =
      create_temp_file
        ~perm
        ~in_dir:(File_path.dirname_defaulting_to_dot_or_root path)
        ~prefix:
          ("." ^ File_path.Part.to_string (File_path.basename_defaulting_to_dot path))
        ()
    in
    try
      Out_channel.write_all (temp :> string) ~data:contents;
      rename ~src:(temp :> File_path.t) ~dst:path
    with
    | exn ->
      unlink (temp :> File_path.t);
      Exn.reraise exn "failed to atomically write to file"
  ;;

  let load_as_sexp path ~of_sexp =
    Sexp.load_sexp_conv_exn (File_path.to_string path) of_sexp
  ;;

  let load_as_sexps path ~of_sexp =
    Sexp.load_sexps_conv_exn (File_path.to_string path) of_sexp
  ;;

  let load_sexp path = load_as_sexp path ~of_sexp:Fn.id
  let load_sexps path = load_as_sexps path ~of_sexp:Fn.id

  let write_with_buffer ?perm path ~f =
    let buf = Buffer.create 16 in
    f buf;
    write_file ?perm path ~contents:(Buffer.contents buf)
  ;;

  let save_as_sexps ?perm path xs ~sexp_of =
    write_with_buffer ?perm path ~f:(fun buf ->
      List.iter xs ~f:(fun x ->
        Sexp.to_buffer_hum (sexp_of x) ~buf;
        Buffer.add_char buf '\n'))
  ;;

  let save_as_sexp ?perm path x ~sexp_of = save_as_sexps ?perm path [ x ] ~sexp_of
  let save_sexp ?perm path sexp = save_as_sexp ?perm path sexp ~sexp_of:Fn.id
  let save_sexps ?perm path sexps = save_as_sexps ?perm path sexps ~sexp_of:Fn.id
end

include struct
  (* hard links and symlinks *)

  let hard_link path ~referring_to =
    Unix.link
      ~link_name:(File_path.to_string path)
      ~target:(File_path.to_string referring_to)
      ()
  ;;

  let symlink_internal path ~referring_to =
    Unix.symlink ~link_name:(File_path.to_string path) ~target:referring_to
  ;;

  let symlink_raw path ~referring_to =
    (* We do want to raise if the string is invalid, but we don't want to change it. *)
    let (_ : File_path.t) = File_path.of_string referring_to in
    symlink_internal path ~referring_to
  ;;

  let symlink path ~referring_to =
    symlink_internal path ~referring_to:(File_path.to_string referring_to)
  ;;

  let readlink_raw path = Unix.readlink (File_path.to_string path)
  let readlink path = readlink_raw path |> File_path.of_string
end

include struct
  (* advisory file locks *)

  module Fd = struct
    type t = Core_unix.File_descr.t
  end

  module Flock = struct
    (** An open, locked file descriptor. Closing the file descriptor releases the lock iff
        [close_upon_funlock]. *)
    type t =
      { fd : Fd.t
      ; close_upon_funlock : bool
      }
  end

  open Flock

  let open_for_flock path ~create =
    let perm = Option.map create ~f:File_permissions.to_int in
    let mode =
      (* We pass [O_CREAT] if (possibly) creating the file. We pass [O_RDONLY] because we
         have to choose to read, write, or both, even though we do neither. We pass
         [O_CLOEXEC] so that locks are not held past [exec] calls. *)
      List.concat
        [ (if Option.is_some perm then [ Core_unix.O_CREAT ] else [])
        ; [ Core_unix.O_RDONLY; Core_unix.O_CLOEXEC ]
        ]
    in
    Core_unix.openfile (File_path.to_string path) ~mode ?perm
  ;;

  let flock_command ~shared =
    if shared
    then Core_unix.Flock_command.lock_shared
    else Core_unix.Flock_command.lock_exclusive
  ;;

  let flock_of_fd_internal fd ~close_upon_funlock ~shared =
    Core_unix.flock_blocking fd (flock_command ~shared);
    { fd; close_upon_funlock }
  ;;

  let flock_internal ~create ~shared path =
    let fd = open_for_flock path ~create in
    flock_of_fd_internal fd ~close_upon_funlock:true ~shared
  ;;

  let flock_of_fd ?(shared = false) fd =
    flock_of_fd_internal fd ~close_upon_funlock:false ~shared
  ;;

  let flock ?(shared = false) path = flock_internal ~create:None ~shared path

  let flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path =
    flock_internal ~create:(Some perm) ~shared path
  ;;

  let try_flock_of_fd_internal fd ~close_upon_funlock ~shared =
    match Core_unix.flock fd (flock_command ~shared) with
    | true -> Some { fd; close_upon_funlock }
    | false ->
      if close_upon_funlock then Core_unix.close fd;
      None
  ;;

  let try_flock_internal ~create ~shared path =
    let fd = open_for_flock path ~create in
    try_flock_of_fd_internal fd ~close_upon_funlock:true ~shared
  ;;

  let try_flock_of_fd ?(shared = false) fd =
    try_flock_of_fd_internal fd ~close_upon_funlock:false ~shared
  ;;

  let try_flock ?(shared = false) path = try_flock_internal ~create:None ~shared path

  let try_flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path =
    try_flock_internal ~create:(Some perm) ~shared path
  ;;

  let funlock { fd; close_upon_funlock } =
    match close_upon_funlock with
    | true -> Core_unix.close fd
    | false -> Core_unix.flock_blocking fd Core_unix.Flock_command.unlock
  ;;

  let flock_fd t = t.fd

  let with_flock_of_fd_internal fd ~close_upon_funlock ~shared ~f =
    let t = flock_of_fd_internal fd ~close_upon_funlock ~shared in
    Exn.protectx t ~f ~finally:funlock
  ;;

  let with_flock_internal ~create ~shared path ~f =
    let fd = open_for_flock path ~create in
    with_flock_of_fd_internal fd ~close_upon_funlock:true ~shared ~f
  ;;

  let with_flock_of_fd ?(shared = false) fd ~f =
    with_flock_of_fd_internal fd ~close_upon_funlock:false ~shared ~f
  ;;

  let with_flock ?(shared = false) path ~f =
    with_flock_internal ~create:None ~shared path ~f
  ;;

  let with_flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path ~f =
    with_flock_internal ~create:(Some perm) ~shared path ~f
  ;;
end
