open! Core
open! Async_kernel
open! Async_unix
open! Async_log_kernel.Ppx_log_syntax
include Filesystem_types

open struct
  let time_ns_to_float time_ns = Time_ns.Span.to_sec (Time_ns.to_span_since_epoch time_ns)
end

let executable_name = lazy (File_path.of_string Sys.executable_name)
let default_temp_dir = lazy (File_path.of_string Filename.temp_dir_name)

include struct
  (* [Filename] wrappers. *)

  let realpath_relative_to_cwd path =
    In_thread.run (fun () -> Filesystem_core.realpath_relative_to_cwd path)
  ;;

  let realpath_absolute path =
    In_thread.run (fun () -> Filesystem_core.realpath_absolute path)
  ;;

  let realpath path ~relative_to =
    In_thread.run (fun () -> Filesystem_core.realpath path ~relative_to)
  ;;
end

include struct
  (* [Sys] wrappers *)

  let exists_exn ?follow_symlinks path =
    Sys.file_exists_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_directory_exn ?follow_symlinks path =
    Sys.is_directory_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_file_exn ?follow_symlinks path =
    Sys.is_file_exn ?follow_symlinks (File_path.to_string path)
  ;;

  let is_symlink_exn path = Sys.is_symlink_exn (File_path.to_string path)

  let exists ?follow_symlinks path =
    Sys.file_exists ?follow_symlinks (File_path.to_string path)
  ;;

  let is_directory ?follow_symlinks path =
    Sys.is_directory ?follow_symlinks (File_path.to_string path)
  ;;

  let is_file ?follow_symlinks path =
    Sys.is_file ?follow_symlinks (File_path.to_string path)
  ;;

  let is_symlink path = Sys.is_symlink (File_path.to_string path)

  let ls_dir path =
    let%map ls = Sys.ls_dir (File_path.to_string path) in
    ls
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
    Unix.mkdir
      ~perm:(File_permissions.to_int perm)
      ?p:(if parents then Some () else None)
      (File_path.to_string path)
  ;;

  let rmdir path = Unix.rmdir (File_path.to_string path)
  let chdir path = Unix.chdir (File_path.to_string path)
  let getcwd () = Unix.getcwd () >>| File_path.Absolute.of_string

  let chmod path ~perm =
    Unix.chmod (File_path.to_string path) ~perm:(File_permissions.to_int perm)
  ;;

  let of_async_file_stats
    ({ dev; ino; kind; perm; nlink; uid; gid; rdev; size; atime; mtime; ctime } :
      Unix.Stats.t)
    : File_stats.t
    =
    { host_device = dev
    ; inode = ino
    ; kind = File_kind.of_async_file_kind kind
    ; permissions = File_permissions.of_int_exn perm
    ; hard_links = nlink
    ; user_id = uid
    ; group_id = gid
    ; file_device = rdev
    ; size = Int63.of_int64_exn size
    ; access_time = Time_ns.of_time_float_round_nearest atime
    ; modify_time = Time_ns.of_time_float_round_nearest mtime
    ; status_time = Time_ns.of_time_float_round_nearest ctime
    }
  ;;

  let stat path = Unix.stat (File_path.to_string path) >>| of_async_file_stats
  let lstat path = Unix.lstat (File_path.to_string path) >>| of_async_file_stats

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
  (* additional helper functions *)
  let rm ?(force = true) ?(recursive = false) (path : File_path.t) =
    let args =
      [ Option.some_if force "-f"
      ; Option.some_if recursive "-r"
      ; (if String.is_prefix (path :> string) ~prefix:"-" then Some "--" else None)
      ; Some (File_path.to_string path)
      ]
      |> List.filter_opt
    in
    Process.run_expect_no_output_exn ~prog:"rm" ~args ()
  ;;
end

include struct
  (* current directory functions *)

  let make_absolute_under_cwd path =
    match File_path.to_variant path with
    | Absolute abspath -> return abspath
    | Relative relpath ->
      let%map cwd = getcwd () in
      File_path.Absolute.append cwd relpath
  ;;

  let make_relative_to_cwd path =
    match File_path.to_relative path with
    | Some _ as some -> return some
    | None ->
      let%map cwd = getcwd () in
      File_path.make_relative path ~if_under:cwd
  ;;

  let make_relative_to_cwd_exn path =
    match File_path.to_relative path with
    | Some relpath -> return relpath
    | None ->
      let%map cwd = getcwd () in
      File_path.make_relative_exn path ~if_under:cwd
  ;;

  let make_relative_to_cwd_if_possible path =
    if File_path.is_relative path
    then return path
    else (
      let%map cwd = getcwd () in
      File_path.make_relative_if_possible path ~if_under:cwd)
  ;;
end

include struct
  (* temporary file/directory functions *)

  (* We write [internal_] versions of functions that all take the same set of required
     arguments. *)

  let internal_create_temp_file ~in_dir ~perm ~prefix ~suffix () =
    In_thread.run (fun () ->
      File_path.Absolute.of_string
        (Filename_unix.temp_file
           ~perm:(File_permissions.to_int perm)
           ~in_dir:(File_path.Absolute.to_string in_dir)
           prefix
           suffix))
  ;;

  let internal_create_temp_dir ~in_dir ~perm ~prefix ~suffix () =
    In_thread.run (fun () ->
      File_path.Absolute.of_string
        (Filename_unix.temp_dir
           ~perm:(File_permissions.to_int perm)
           ~in_dir:(File_path.Absolute.to_string in_dir)
           prefix
           suffix))
  ;;

  let with_cleanup ~on_cleanup_error path ~f ~cleanup =
    Monitor.protect
      (fun () -> f path)
      ~finally:(fun () ->
        match%bind Monitor.try_with (fun () -> cleanup path) with
        | Ok () -> return ()
        | Error exn ->
          let exn, backtrace =
            match exn with
            | Monitor.Monitor_exn monitor_exn ->
              ( Monitor.Monitor_exn.extract_exn monitor_exn
              , Monitor.Monitor_exn.backtrace monitor_exn )
            | _ -> exn, None
          in
          On_cleanup_error.on_cleanup_error
            on_cleanup_error
            ~backtrace
            ~exn
            ~log_s:(fun sexp ->
              [%log.error_sexp sexp];
              return ())
            ~path
            ~return)
  ;;

  let internal_with_temp_file ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    let%bind path = internal_create_temp_file ~in_dir ~perm ~prefix ~suffix () in
    with_cleanup ~on_cleanup_error path ~f ~cleanup:(fun path ->
      (* delete the temp file, ok if it was already deleted *)
      match%bind
        Monitor.try_with ~extract_exn:true (fun () -> unlink (File_path.of_absolute path))
      with
      | Ok () | Error (Unix.Unix_error (ENOENT, _, _)) -> return ()
      | Error exn -> Exn.reraise exn "error deleting temporary file")
  ;;

  let internal_with_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    let%bind path = internal_create_temp_dir ~in_dir ~perm ~prefix ~suffix () in
    with_cleanup ~on_cleanup_error path ~f ~cleanup:(fun path ->
      File_path.of_absolute path |> rm ~recursive:true)
  ;;

  let internal_within_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix f =
    internal_with_temp_dir ~in_dir ~on_cleanup_error ~perm ~prefix ~suffix (fun path ->
      let%bind prev = getcwd () in
      let%bind () = chdir (File_path.of_absolute path) in
      Monitor.protect f ~finally:(fun () ->
        (* restore the previous working directory before cleaning up the temp dir *)
        chdir (File_path.of_absolute prev)))
  ;;

  (* We wrap the internal functions with optional arguments uniformly. *)

  let wrap f ~default_perm =
    stage (fun ?in_dir ?(perm = default_perm) ?(prefix = "") ?(suffix = "") arg ->
      let%bind in_dir =
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
         let%bind in_dir =
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

  let read_file path = Reader.file_contents (File_path.to_string path)

  let write_file ?(perm = File_permissions.u_rw) path ~contents =
    Writer.save ~perm:(File_permissions.to_int perm) (File_path.to_string path) ~contents
  ;;

  let load_as_sexp path ~of_sexp = Reader.load_sexp_exn (File_path.to_string path) of_sexp

  let load_as_sexps path ~of_sexp =
    Reader.load_sexps_exn (File_path.to_string path) of_sexp
  ;;

  let load_sexp path = load_as_sexp path ~of_sexp:Fn.id
  let load_sexps path = load_as_sexps path ~of_sexp:Fn.id

  let save_as_sexps ?perm path xs ~sexp_of =
    Writer.save_sexps_conv
      ?perm:(Option.map perm ~f:File_permissions.to_int)
      (File_path.to_string path)
      xs
      sexp_of
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
  let readlink path = readlink_raw path >>| File_path.of_string
end

include struct
  (* advisory file locks *)

  module Fd = Fd

  module Flock = struct
    type t =
      { fd : Fd.t
      ; close_upon_funlock : bool
      }
  end

  open Flock

  let flock_open path ~create =
    let perm = Option.map create ~f:File_permissions.to_int in
    let mode =
      (* We pass [O_CREAT] if (possibly) creating the file. We pass [O_RDONLY] because we
         have to choose to read, write, or both, even though we do neither. [O_CLOEXEC] is
         passed unconditionally by [Async.Unix.openfile]. *)
      List.concat [ (if Option.is_some create then [ `Creat ] else []); [ `Rdonly ] ]
    in
    Unix.openfile (File_path.to_string path) ~mode ?perm
  ;;

  let flock_command ~shared : Unix.Lock_mode.t = if shared then Shared else Exclusive

  let flock_of_fd_internal fd ~close_upon_funlock ~shared =
    let%bind () = Unix.flock fd (flock_command ~shared) in
    return { fd; close_upon_funlock }
  ;;

  let flock_internal ~create ~shared path =
    let%bind fd = flock_open path ~create in
    flock_of_fd_internal fd ~close_upon_funlock:true ~shared
  ;;

  let flock ?(shared = false) path = flock_internal ~create:None ~shared path

  let flock_of_fd ?(shared = false) fd =
    flock_of_fd_internal fd ~close_upon_funlock:false ~shared
  ;;

  let flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path =
    flock_internal ~create:(Some perm) ~shared path
  ;;

  let try_flock_of_fd_internal fd ~close_upon_funlock ~shared =
    (* [Async.Unix.try_flock] pretends to be non-blocking, but blocks on NFS. *)
    match%bind In_thread.run (fun () -> Unix.try_flock fd (flock_command ~shared)) with
    | true -> return (Some { fd; close_upon_funlock })
    | false ->
      (match close_upon_funlock with
       | false -> return None
       | true ->
         let%map () = Unix.close fd in
         None)
  ;;

  let try_flock_internal ~create ~shared path =
    let%bind fd = flock_open path ~create in
    try_flock_of_fd_internal fd ~close_upon_funlock:true ~shared
  ;;

  let try_flock ?(shared = false) path = try_flock_internal ~create:None ~shared path

  let try_flock_of_fd ?(shared = false) fd =
    try_flock_of_fd_internal fd ~close_upon_funlock:true ~shared
  ;;

  let try_flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path =
    try_flock_internal ~create:(Some perm) ~shared path
  ;;

  let funlock { fd; close_upon_funlock } =
    match close_upon_funlock with
    | true -> Unix.close fd
    | false ->
      (* [Async.Unix.funlock] pretends to be non-blocking, but blocks on NFS. *)
      In_thread.run (fun () -> Unix.funlock fd)
  ;;

  let flock_fd t = t.fd

  let with_flock_of_fd_internal fd ~close_upon_funlock ~shared ~f =
    let%bind t = flock_of_fd_internal fd ~close_upon_funlock ~shared in
    Monitor.protect (fun () -> f t) ~finally:(fun () -> funlock t)
  ;;

  let with_flock_internal ~create ~shared path ~f =
    let%bind fd = flock_open path ~create in
    with_flock_of_fd_internal fd ~close_upon_funlock:true ~shared ~f
  ;;

  let with_flock ?(shared = false) path ~f =
    with_flock_internal ~create:None ~shared path ~f
  ;;

  let with_flock_of_fd ?(shared = false) fd ~f =
    with_flock_of_fd_internal ~close_upon_funlock:false ~shared fd ~f
  ;;

  let with_flock_create ?(perm = File_permissions.u_rw) ?(shared = false) path ~f =
    with_flock_internal ~create:(Some perm) ~shared path ~f
  ;;
end
