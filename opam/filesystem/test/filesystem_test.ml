open! Core
open! Async
open Expect_test_helpers_core
open Expect_test_helpers_async
open Filesystem_types
open File_path.Operators

module Unhidden_file_stats = struct
  let rec strip_hidden sexp =
    match (sexp : Sexp.t) with
    | Atom _ -> sexp
    | List list ->
      List
        (List.filter_map list ~f:(function
          | List [ _; Atom "<hidden>" ] -> None
          | sexp -> Some (strip_hidden sexp)))
  ;;

  let sexp_of_t t = File_stats.sexp_of_t t |> strip_hidden
end

module type IO = sig
  type +'a t

  include Monad.S with type 'a t := 'a t

  (** Convert I/O computations to a deferred. *)
  val async : (unit -> 'a t) -> 'a Deferred.t

  (** Convert deferred computations to I/O. *)
  val of_async : (unit -> 'a Deferred.t) -> 'a t

  module Fd : sig
    type t
  end

  val with_fd : File_path.t -> f:(Fd.t -> 'a t) -> 'a t
end

module Test_filesystem
    (IO : IO)
    (Fs : Filesystem.S with module IO := IO and type Fd.t = IO.Fd.t) :
  Filesystem.S with module IO := IO = struct
  let executable_name = Fs.executable_name

  let%expect_test "[executable_name]" =
    require_equal
      (module String)
      Sys.executable_name
      (File_path.to_string (force Fs.executable_name));
    [%expect {| |}];
    return ()
  ;;

  let default_temp_dir = Fs.default_temp_dir

  let%expect_test "[default_temp_dir]" =
    require_equal
      (module String)
      Filename.temp_dir_name
      (File_path.to_string (force Fs.default_temp_dir));
    [%expect {| |}];
    return ()
  ;;

  let read_file = Fs.read_file
  let write_file = Fs.write_file

  let%expect_test "[read_file] and [write_file]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "file.txt" in
      let write_contents = "you do the hokey pokey and you turn yourself around" in
      let%bind () = IO.async (fun () -> write_file path ~contents:write_contents) in
      let%bind read_contents = IO.async (fun () -> read_file path) in
      require_equal (module String) write_contents read_contents;
      [%expect {| |}];
      return ())
  ;;

  open struct
    (* helpers for testing write atomicity *)

    let test_atomic_write_once ~size ~iteration =
      let path = File_path.of_string (sprintf "file.%d.%d.txt" size iteration) in
      let check_atomic =
        Deferred.repeat_until_finished () (fun () ->
          (* stat the file until it exists *)
          match%map
            Monitor.try_with ~extract_exn:true (fun () ->
              Unix.stat (File_path.to_string path))
          with
          (* if it exists, check its length *)
          | Ok stat ->
            if Int64.equal stat.size (Int64.of_int size)
            then (* full size? write appears to be atomic *)
              `Finished (Ok ())
            else
              (* partial size? write is not atomic *)
              `Finished
                (Or_error.error_s
                   [%message
                     "write was not atomic"
                       ~expect_size:(size : int)
                       ~actual_size:(stat.size : int64)])
          (* doesn't exist yet? retry *)
          | Error (Unix.Unix_error (ENOENT, _, _)) -> `Repeat ()
          (* fail on any other kind of error *)
          | Error exn ->
            `Finished (Or_error.error_s [%message "unexpected exception" (exn : exn)]))
      in
      let write_file =
        (* write to the file concurrently with checking atomicity *)
        IO.async (fun () -> write_file path ~contents:(String.make size '.'))
      in
      let%map result = check_atomic
      and () = write_file in
      result
    ;;
  end

  let%expect_test "[write_file] atomically" =
    (* try writing files of size 1024 through 1048576 until we hit a failure *)
    let min_size_pow2 = 10 in
    let max_size_pow2 = 20 in
    (* try each size ten times *)
    let max_iteration = 10 in
    (* this gives us around a hundred chances to fail, since failure (if present) is
       nondeterministic *)
    let%bind () =
      within_temp_dir (fun () ->
        Deferred.repeat_until_finished min_size_pow2 (fun size_pow2 ->
          let%map result =
            Deferred.repeat_until_finished 1 (fun iteration ->
              let%map result =
                let size = 1 lsl size_pow2 in
                test_atomic_write_once ~size ~iteration
              in
              (* repeat until failure or done with the current size *)
              match result with
              | Ok () when iteration < max_iteration -> `Repeat (iteration + 1)
              | result -> `Finished result)
          in
          (* repeat until failure or done with all sizes *)
          match result with
          | Ok () when size_pow2 < max_size_pow2 -> `Repeat (size_pow2 + 1)
          | result ->
            (* when finished, check for success *)
            require_ok result;
            `Finished ()))
    in
    [%expect {| |}];
    return ()
  ;;

  open struct
    (* Helpers for load/save tests. *)

    let test_load_save m original ~load ~save =
      within_temp_dir (fun () ->
        let path = File_path.of_string "data.sexp" in
        let%bind () = IO.async (fun () -> save path original) in
        let%bind contents = IO.async (fun () -> read_file path) in
        print_string contents;
        let%bind round_trip = IO.async (fun () -> load path) in
        require_equal m original round_trip;
        return ())
    ;;
  end

  let load_sexp = Fs.load_sexp
  let save_sexp = Fs.save_sexp

  let%expect_test "[load_sexp] and [save_sexp]" =
    let test string =
      test_load_save (module Sexp) ~load:load_sexp ~save:save_sexp (Sexp.of_string string)
    in
    let%bind () = test "()" in
    [%expect {| () |}];
    let%bind () =
      test
        "((key value) (other-key other-value) (another-key another-value) \
         (yet-another-key yet-another-value))"
    in
    [%expect
      {|
      ((key value) (other-key other-value) (another-key another-value)
       (yet-another-key yet-another-value))
      |}];
    return ()
  ;;

  let load_sexps = Fs.load_sexps
  let save_sexps = Fs.save_sexps

  let%expect_test "[load_sexps] and [save_sexps]" =
    let test string =
      test_load_save
        (module struct
          type t = Sexp.t list [@@deriving equal, sexp_of]
        end)
        ~load:load_sexps
        ~save:save_sexps
        (Sexp.of_string_many string)
    in
    let%bind () = test "" in
    [%expect {| |}];
    let%bind () =
      test
        "(key value) (other-key other-value) (another-key another-value) \
         (yet-another-key yet-another-value)"
    in
    [%expect
      {|
      (key value)
      (other-key other-value)
      (another-key another-value)
      (yet-another-key yet-another-value)
      |}];
    return ()
  ;;

  let load_as_sexp = Fs.load_as_sexp
  let save_as_sexp = Fs.save_as_sexp

  let%expect_test "[load_as_sexp] and [save_as_sexp]" =
    let module M = struct
      type t = string Int.Map.t [@@deriving equal, sexp]
    end
    in
    let test alist =
      test_load_save
        (module M)
        ~load:(load_as_sexp ~of_sexp:M.t_of_sexp)
        ~save:(save_as_sexp ~sexp_of:M.sexp_of_t)
        (Int.Map.of_alist_exn alist)
    in
    let%bind () = test [] in
    [%expect {| () |}];
    let%bind () =
      test
        [ 1, "a unit"; 2, "a prime number"; 3, "a prime number"; 4, "a composite number" ]
    in
    [%expect
      {|
      ((1 "a unit") (2 "a prime number") (3 "a prime number")
       (4 "a composite number"))
      |}];
    return ()
  ;;

  let load_as_sexps = Fs.load_as_sexps
  let save_as_sexps = Fs.save_as_sexps

  let%expect_test "[load_as_sexps] and [save_as_sexps]" =
    let module M = struct
      type t = string * string list [@@deriving equal, sexp]
    end
    in
    let test list =
      test_load_save
        (module struct
          type t = M.t list [@@deriving equal, sexp_of]
        end)
        ~load:(load_as_sexps ~of_sexp:M.t_of_sexp)
        ~save:(save_as_sexps ~sexp_of:M.sexp_of_t)
        list
    in
    let%bind () = test [] in
    [%expect {| |}];
    let%bind () =
      test
        [ "do", [ "a deer"; "a female deer" ]
        ; "re", [ "a drop of golden sun" ]
        ; "mi", [ "a name"; "I call myself" ]
        ]
    in
    [%expect
      {|
      (do ("a deer" "a female deer"))
      (re ("a drop of golden sun"))
      (mi ("a name" "I call myself"))
      |}];
    return ()
  ;;

  let exists = Fs.exists
  let exists_exn = Fs.exists_exn
  let is_directory = Fs.is_directory
  let is_directory_exn = Fs.is_directory_exn
  let is_file = Fs.is_file
  let is_file_exn = Fs.is_file_exn
  let is_symlink = Fs.is_symlink
  let is_symlink_exn = Fs.is_symlink_exn

  let%expect_test "[exists], [is_directory], [is_file], [is_symlink]" =
    let module M = struct
      type t =
        [ `Yes
        | `No
        | `Unknown
        ]
      [@@deriving equal, sexp_of]
    end
    in
    let test path =
      let%bind exists_exn = IO.async (fun () -> exists_exn path) in
      let%bind is_file_exn = IO.async (fun () -> is_file_exn path) in
      let%bind is_directory_exn = IO.async (fun () -> is_directory_exn path) in
      let%bind is_symlink_exn = IO.async (fun () -> is_symlink_exn path) in
      print_s
        [%sexp
          { path : File_path.t
          ; exists_exn : bool
          ; is_file_exn : bool
          ; is_directory_exn : bool
          ; is_symlink_exn : bool
          }];
      let%bind exists = IO.async (fun () -> exists path) in
      let%bind is_file = IO.async (fun () -> is_file path) in
      let%bind is_directory = IO.async (fun () -> is_directory path) in
      let%bind is_symlink = IO.async (fun () -> is_symlink path) in
      require_equal (module M) exists (if exists_exn then `Yes else `No);
      require_equal (module M) is_file (if is_file_exn then `Yes else `No);
      require_equal (module M) is_directory (if is_directory_exn then `Yes else `No);
      require_equal (module M) is_symlink (if is_symlink_exn then `Yes else `No);
      return ()
    in
    within_temp_dir (fun () ->
      let%bind () =
        let file = File_path.of_string "file" in
        let%bind () = run "touch" [ (file :> string) ] in
        test file
      in
      [%expect
        {|
        ((path             file)
         (exists_exn       true)
         (is_file_exn      true)
         (is_directory_exn false)
         (is_symlink_exn   false))
        |}];
      let%bind () =
        let directory = File_path.of_string "dir" in
        let%bind () = run "mkdir" [ (directory :> string) ] in
        test directory
      in
      [%expect
        {|
        ((path             dir)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn true)
         (is_symlink_exn   false))
        |}];
      let%bind () = test (File_path.of_string "nonexistent") in
      [%expect
        {|
        ((path             nonexistent)
         (exists_exn       false)
         (is_file_exn      false)
         (is_directory_exn false)
         (is_symlink_exn   false))
        |}];
      let%bind () = test (File_path.of_string "/dev/null") in
      [%expect
        {|
        ((path             /dev/null)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn false)
         (is_symlink_exn   false))
        |}];
      let%bind () =
        let symlink = File_path.of_string "symlink" in
        let%bind () = run "ln" [ "-s"; "/"; (symlink :> string) ] in
        test symlink
      in
      [%expect
        {|
        ((path             symlink)
         (exists_exn       true)
         (is_file_exn      false)
         (is_directory_exn true)
         (is_symlink_exn   true))
        |}];
      return ())
  ;;

  let stat = Fs.stat
  let lstat = Fs.lstat

  let%expect_test "[stat] and [lstat]" =
    let test path =
      let%bind lstats = IO.async (fun () -> lstat path) in
      print_s [%sexp (lstats : File_stats.t)];
      let%bind stats = IO.async (fun () -> stat path) in
      Expect_test_patdiff.print_patdiff_s
        ~context:0
        ~location_style:None
        [%sexp (lstats : File_stats.t)]
        [%sexp (stats : File_stats.t)];
      return ()
    in
    within_temp_dir (fun () ->
      let%bind () = run "touch" [ "file" ] in
      let%bind () = run "chmod" [ "600"; "file" ] in
      let%bind () = test ?/"file" in
      [%expect
        {|
        ((host_device <hidden>)
         (inode       <hidden>)
         (kind        Regular)
         (permissions -rw-------)
         (hard_links  1)
         (user_id     <hidden>)
         (group_id    <hidden>)
         (file_device <hidden>)
         (size        0)
         (access_time <hidden>)
         (modify_time <hidden>)
         (status_time <hidden>))
        |}];
      let%bind () = run "ln" [ "-s"; "file"; "link" ] in
      let%bind () = test ?/"link" in
      [%expect
        {|
        ((host_device <hidden>)
         (inode       <hidden>)
         (kind        Symlink)
         (permissions -rwxrwxrwx)
         (hard_links  1)
         (user_id     <hidden>)
         (group_id    <hidden>)
         (file_device <hidden>)
         (size        4)
         (access_time <hidden>)
         (modify_time <hidden>)
         (status_time <hidden>))

        -| (kind        Symlink)
        +| (kind        Regular)
        -| (permissions -rwxrwxrwx)
        +| (permissions -rw-------)

        -| (size        4)
        +| (size        0)
        |}];
      return ())
  ;;

  let chmod = Fs.chmod

  let%expect_test "[chmod]" =
    let test path ~perm =
      let%bind before = IO.async (fun () -> stat path) in
      let%bind () = IO.async (fun () -> chmod path ~perm) in
      let%bind after = IO.async (fun () -> stat path) in
      require_equal (module File_permissions) after.permissions perm;
      Expect_test_patdiff.print_patdiff_s
        ~context:0
        ~location_style:None
        [%sexp (before : File_stats.t)]
        [%sexp (after : File_stats.t)];
      return ()
    in
    within_temp_dir (fun () ->
      let%bind () = run "touch" [ "file" ] in
      let%bind () = run "chmod" [ "600"; "file" ] in
      let%bind () =
        test ?/"file" ~perm:File_permissions.all_including_special_mode_bits
      in
      [%expect
        {|
        -| (permissions -rw-------)
        +| (permissions -rwsrwsrwt)
        |}];
      let%bind () = test ?/"file" ~perm:File_permissions.empty in
      [%expect
        {|
        -| (permissions -rwsrwsrwt)
        +| (permissions ----------)
        |}];
      let%bind () = test ?/"file" ~perm:File_permissions.(u_rwx lor g_rx lor o_rx) in
      [%expect
        {|
        -| (permissions ----------)
        +| (permissions -rwxr-xr-x)
        |}];
      return ())
  ;;

  let update_timestamps = Fs.update_timestamps

  let%expect_test "[update_timestamps]" =
    (* We test the case with [~at] and not the case without. It's just equivalent to
       [~at:(Time_ns.now ())], and we don't need the ensuing nondeterminism. *)
    let test at =
      let%bind () = IO.async (fun () -> update_timestamps ?/"file" ~at) in
      let%bind stats = IO.async (fun () -> stat ?/"file") in
      require_equal (module Time_ns_unix) stats.access_time at;
      require_equal (module Time_ns_unix) stats.modify_time at;
      return ()
    in
    let on date_string =
      (* we choose start-of-day times to avoid rounding error *)
      Time_ns.of_date_ofday
        ~zone:Time_ns_unix.Zone.utc
        (Date.of_string date_string)
        Time_ns.Ofday.start_of_day
    in
    within_temp_dir (fun () ->
      let%bind () =
        require_does_raise_async (fun () ->
          IO.async (fun () -> update_timestamps ?/"file"))
      in
      [%expect {| (Unix.Unix_error "No such file or directory" utimes file) |}];
      let%bind () = IO.async (fun () -> write_file ?/"file" ~contents:"") in
      let%bind () = test (on "2013-10-07") in
      [%expect {| |}];
      let%bind () = test (on "2022-08-29") in
      [%expect {| |}];
      let%bind () = test (on "2019-10-14") in
      [%expect {| |}];
      return ())
  ;;

  let update_timestamps_separately = Fs.update_timestamps_separately

  let%expect_test "[update_timestamps_separately]" =
    let test access_time modify_time =
      let%bind () =
        IO.async (fun () ->
          update_timestamps_separately ?/"file" ~access_time ~modify_time)
      in
      let%bind stats = IO.async (fun () -> stat ?/"file") in
      require_equal (module Time_ns_unix) stats.access_time access_time;
      require_equal (module Time_ns_unix) stats.modify_time modify_time;
      return ()
    in
    let on date_string =
      (* we choose start-of-day times to avoid rounding error *)
      Time_ns.of_date_ofday
        ~zone:Time_ns_unix.Zone.utc
        (Date.of_string date_string)
        Time_ns.Ofday.start_of_day
    in
    within_temp_dir (fun () ->
      let%bind () =
        require_does_raise_async (fun () -> test (on "2013-01-03") (on "2013-10-07"))
      in
      [%expect {| (Unix.Unix_error "No such file or directory" utimes file) |}];
      let%bind () = IO.async (fun () -> write_file ?/"file" ~contents:"") in
      let%bind () = test (on "2013-01-03") (on "2013-10-07") in
      [%expect {| |}];
      let%bind () = test (on "2022-08-29") (on "2018-07-23") in
      [%expect {| |}];
      return ())
  ;;

  let unlink = Fs.unlink

  let%expect_test "[unlink]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "my-file" in
      let%bind () = run "touch" [ (path :> string) ] in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        my-file
        |}];
      let%bind () = IO.async (fun () -> unlink path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        |}];
      return ())
  ;;

  let rename = Fs.rename

  let%expect_test "[rename]" =
    within_temp_dir (fun () ->
      let src = File_path.of_string "source" in
      let dst = File_path.of_string "destination" in
      let%bind () = run "touch" [ (src :> string) ] in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        source
        |}];
      let%bind () = IO.async (fun () -> rename ~src ~dst) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        destination
        |}];
      return ())
  ;;

  let mkdir = Fs.mkdir
  let rmdir = Fs.rmdir

  let%expect_test "[mkdir] and [rmdir]" =
    within_temp_dir (fun () ->
      let path = File_path.of_string "etc" in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        |}];
      let%bind () = IO.async (fun () -> mkdir path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        etc/
        |}];
      let%bind () = IO.async (fun () -> rmdir path) in
      let%bind () = run "ls" [ "-aF" ] in
      [%expect
        {|
        ./
        ../
        |}];
      return ())
  ;;

  let chdir = Fs.chdir
  let getcwd = Fs.getcwd

  let%expect_test "[chdir] and [getcwd]" =
    within_temp_dir (fun () ->
      let subdir = File_path.Relative.of_string "subdir" in
      let%bind () = Unix.mkdir (subdir :> string) in
      let%bind tmp = IO.async (fun () -> getcwd ()) in
      let%bind () = IO.async (fun () -> chdir (File_path.of_relative subdir)) in
      let%bind tmp_slash_subdir = IO.async (fun () -> getcwd ()) in
      require_equal
        (module File_path.Absolute)
        tmp_slash_subdir
        (File_path.Absolute.append tmp subdir);
      [%expect {| |}];
      return ())
  ;;

  let ls_dir = Fs.ls_dir

  let%expect_test "[ls_dir]" =
    with_temp_dir (fun tmp ->
      let%bind () = run "touch" [ tmp ^/ "regular-file" ] in
      let%bind () = run "mkdir" [ tmp ^/ "subdirectory" ] in
      let%bind () = run "touch" [ tmp ^/ "subdirectory/another-file" ] in
      let%bind ls = IO.async (fun () -> ls_dir (File_path.of_string tmp)) in
      print_s [%sexp (ls : File_path.Part.t list)];
      [%expect {| (regular-file subdirectory) |}];
      return ())
  ;;

  let rm = Fs.rm

  let%expect_test "[rm], non-recursive" =
    within_temp_dir (fun () ->
      let%bind () = run "touch" [ "file1" ] in
      let%bind () = run "touch" [ "file2" ] in
      let%bind () = run "tree" [ "--noreport" ] in
      [%expect
        {|
        .
        |-- file1
        `-- file2
        |}];
      let%bind () = IO.async (fun () -> rm (File_path.of_string "file1")) in
      let%bind () = run "tree" [ "--noreport" ] in
      [%expect
        {|
        .
        `-- file2
        |}];
      return ())
  ;;

  let%expect_test "[rm], recursive" =
    within_temp_dir (fun () ->
      let%bind () = run "touch" [ "file1" ] in
      let%bind () = run "mkdir" [ "dir" ] in
      let%bind () = run "mkdir" [ "dir/subdir" ] in
      let%bind () = run "touch" [ "dir/file2" ] in
      let%bind () = run "touch" [ "dir/subdir/file3" ] in
      let%bind () = run "tree" [ "--noreport" ] in
      [%expect
        {|
        .
        |-- dir
        |   |-- file2
        |   `-- subdir
        |       `-- file3
        `-- file1
        |}];
      let%bind () = IO.async (fun () -> rm ~recursive:true (File_path.of_string "dir")) in
      let%bind () = run "tree" [ "--noreport" ] in
      [%expect
        {|
        .
        `-- file1
        |}];
      return ())
  ;;

  let make_absolute_under_cwd = Fs.make_absolute_under_cwd

  let%expect_test "[make_absolute_under_cwd]" =
    within_temp_dir (fun () ->
      let%bind tmp = Sys.getcwd () in
      let test string =
        let path = File_path.of_string string in
        let%bind abspath = IO.async (fun () -> make_absolute_under_cwd path) in
        File_path.Absolute.to_string abspath
        |> replace ~pattern:tmp ~with_:"$TMP"
        |> print_endline;
        return ()
      in
      let%bind () = test "a/relative/path" in
      [%expect {| $TMP/a/relative/path |}];
      let%bind () = test "/usr/share/dict/words" in
      [%expect {| /usr/share/dict/words |}];
      return ())
  ;;

  let make_relative_to_cwd = Fs.make_relative_to_cwd
  let make_relative_to_cwd_exn = Fs.make_relative_to_cwd_exn
  let make_relative_to_cwd_if_possible = Fs.make_relative_to_cwd_if_possible

  let%expect_test "[make_relative_to_cwd]" =
    within_temp_dir (fun () ->
      let test string =
        let path = File_path.of_string string in
        let%bind relative_to_cwd = IO.async (fun () -> make_relative_to_cwd path) in
        let%bind relative_to_cwd_exn =
          Deferred.Or_error.try_with (fun () ->
            IO.async (fun () -> make_relative_to_cwd_exn path))
        in
        let%bind relative_to_cwd_if_possible =
          IO.async (fun () -> make_relative_to_cwd_if_possible path)
        in
        print_s [%sexp (relative_to_cwd_if_possible : File_path.t)];
        require_equal
          (module struct
            type t = File_path.Relative.t option [@@deriving equal, sexp_of]
          end)
          relative_to_cwd
          (Or_error.ok relative_to_cwd_exn);
        require_equal
          (module File_path)
          relative_to_cwd_if_possible
          (Option.value_map relative_to_cwd ~f:File_path.of_relative ~default:path);
        return ()
      in
      let%bind tmp = Sys.getcwd () in
      let%bind () = test tmp in
      [%expect {| . |}];
      let%bind () = test (tmp ^/ "path/to/file") in
      [%expect {| path/to/file |}];
      let%bind () = test "/usr/share/dict/words" in
      [%expect {| /usr/share/dict/words |}];
      let%bind () = test "a/relative/path" in
      [%expect {| a/relative/path |}];
      return ())
  ;;

  let realpath = Fs.realpath
  let realpath_absolute = Fs.realpath_absolute
  let realpath_relative_to_cwd = Fs.realpath_relative_to_cwd

  let%expect_test "[realpath_relative_to_cwd]" =
    with_temp_dir (fun tmp ->
      (* helpers *)
      let in_dir dir f =
        let%bind cwd = Sys.getcwd () in
        Monitor.protect
          (fun () ->
            let%bind () = Unix.chdir dir in
            f ())
          ~finally:(fun () -> Unix.chdir cwd)
      in
      let abspath string = File_path.Absolute.of_string (tmp ^/ string) in
      let relpath string = File_path.Relative.of_string string in
      let irrelevant_dir = Filename.temp_dir_name in
      let fns =
        (* all the [realpath*] functions, called with both abspaths and relpaths *)
        let realpath_absolute_of_abspath string =
          IO.async (fun () -> realpath_absolute (abspath string))
        in
        let realpath_of_abspath string =
          IO.async (fun () ->
            realpath
              (abspath string :> File_path.t)
              ~relative_to:(File_path.Absolute.of_string irrelevant_dir))
        in
        let realpath_of_relpath string =
          IO.async (fun () ->
            realpath
              (relpath string :> File_path.t)
              ~relative_to:(File_path.Absolute.of_string tmp))
        in
        let realpath_relative_to_cwd_of_abspath string =
          in_dir irrelevant_dir (fun () ->
            IO.async (fun () -> realpath_relative_to_cwd (abspath string :> File_path.t)))
        in
        let realpath_relative_to_cwd_of_relpath string =
          in_dir tmp (fun () ->
            IO.async (fun () -> realpath_relative_to_cwd (relpath string :> File_path.t)))
        in
        [ realpath_absolute_of_abspath
        ; realpath_of_abspath
        ; realpath_of_relpath
        ; realpath_relative_to_cwd_of_abspath
        ; realpath_relative_to_cwd_of_relpath
        ]
      in
      let test string =
        let%bind results =
          Deferred.List.map ~how:`Sequential fns ~f:(fun fn ->
            Deferred.Or_error.try_with ~extract_exn:true (fun () -> fn string))
        in
        match
          List.all_equal
            results
            ~equal:[%equal: (File_path.Absolute.t, (Error.t[@equal.ignore])) Result.t]
        with
        | Some result ->
          [%sexp (result : File_path.Absolute.t Or_error.t)]
          |> replace_s ~pattern:tmp ~with_:"$TMP"
          |> print_s;
          return ()
        | None ->
          print_cr
            [%message
              "realpath results differ" (results : File_path.Absolute.t Or_error.t list)];
          return ()
      in
      (* test tmp directory itself *)
      let%bind () = test "." in
      [%expect {| (Ok $TMP) |}];
      (* test a nonexistent path *)
      let%bind () = test "nonexistent" in
      [%expect
        {| (Error (Unix.Unix_error "No such file or directory" realpath $TMP/nonexistent)) |}];
      (* test a normal file *)
      let%bind () = run "touch" [ tmp ^/ "real-file" ] in
      let%bind () = test "real-file" in
      [%expect {| (Ok $TMP/real-file) |}];
      (* test a symlink to a normal file *)
      let%bind () = Unix.symlink ~link_name:(tmp ^/ "link-file") ~target:"real-file" in
      let%bind () = test "link-file" in
      [%expect {| (Ok $TMP/real-file) |}];
      (* test a normal directory *)
      let%bind () = Unix.mkdir (tmp ^/ "a") in
      let%bind () = Unix.mkdir (tmp ^/ "a/b") in
      let%bind () = test "a/b" in
      [%expect {| (Ok $TMP/a/b) |}];
      (* test a symlink to a directory *)
      let%bind () = Unix.mkdir (tmp ^/ "c") in
      let%bind () = Unix.symlink ~link_name:(tmp ^/ "c/d") ~target:"../a" in
      let%bind () = test "c/d" in
      [%expect {| (Ok $TMP/a) |}];
      return ())
  ;;

  open struct
    let bash string = run "bash" [ "-c"; string ]

    let show path ~read_symlink =
      let%bind stats = IO.async (fun () -> lstat path) in
      let%bind symlink =
        Deferred.Or_error.try_with ~extract_exn:true (fun () ->
          IO.async (fun () -> read_symlink path))
      in
      let%bind contents =
        Deferred.Or_error.try_with ~extract_exn:true (fun () ->
          IO.async (fun () -> read_file path))
      in
      print_s
        [%sexp
          { stats : Unhidden_file_stats.t
          ; symlink : string Or_error.t
          ; contents : string Or_error.t
          }];
      return ()
    ;;
  end

  let hard_link = Fs.hard_link

  let%expect_test "[hard_link]" =
    within_temp_dir (fun () ->
      let show = show ~read_symlink:(fun _ -> IO.return "n/a") in
      let%bind () = bash "echo original > file && chmod 644 file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink  (Ok n/a))
         (contents (Ok "original\n")))
        |}];
      let%bind () = IO.async (fun () -> hard_link ?/"link" ~referring_to:?/"file") in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  2)
           (size        9)))
         (symlink  (Ok n/a))
         (contents (Ok "original\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  2)
           (size        9)))
         (symlink  (Ok n/a))
         (contents (Ok "original\n")))
        |}];
      let%bind () = bash "echo modified > file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  2)
           (size        9)))
         (symlink  (Ok n/a))
         (contents (Ok "modified\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  2)
           (size        9)))
         (symlink  (Ok n/a))
         (contents (Ok "modified\n")))
        |}];
      return ())
  ;;

  let symlink = Fs.symlink
  let readlink = Fs.readlink

  let%expect_test "[symlink]" =
    within_temp_dir (fun () ->
      let show = show ~read_symlink:(readlink :> File_path.t -> string IO.t) in
      let%bind () = bash "echo original > file && chmod 644 file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "original\n")))
        |}];
      let%bind () = IO.async (fun () -> symlink ?/"link" ~referring_to:?/"file") in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "original\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Symlink)
           (permissions -rwxrwxrwx)
           (hard_links  1)
           (size        4)))
         (symlink  (Ok file))
         (contents (Ok "original\n")))
        |}];
      let%bind () = bash "echo modified > file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "modified\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Symlink)
           (permissions -rwxrwxrwx)
           (hard_links  1)
           (size        4)))
         (symlink  (Ok file))
         (contents (Ok "modified\n")))
        |}];
      return ())
  ;;

  let symlink_raw = Fs.symlink_raw
  let readlink_raw = Fs.readlink_raw

  let%expect_test "[symlink_raw]" =
    within_temp_dir (fun () ->
      let show = show ~read_symlink:readlink_raw in
      let%bind () = bash "echo original > file && chmod 644 file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "original\n")))
        |}];
      let%bind () = IO.async (fun () -> symlink_raw ?/"link" ~referring_to:".//file") in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "original\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Symlink)
           (permissions -rwxrwxrwx)
           (hard_links  1)
           (size        7)))
         (symlink  (Ok .//file))
         (contents (Ok "original\n")))
        |}];
      let%bind () = bash "echo modified > file" in
      let%bind () = show ?/"file" in
      [%expect
        {|
        ((stats (
           (kind        Regular)
           (permissions -rw-r--r--)
           (hard_links  1)
           (size        9)))
         (symlink (
           Error (Unix.Unix_error "Invalid argument" readlink "((filename file))")))
         (contents (Ok "modified\n")))
        |}];
      let%bind () = show ?/"link" in
      [%expect
        {|
        ((stats (
           (kind        Symlink)
           (permissions -rwxrwxrwx)
           (hard_links  1)
           (size        7)))
         (symlink  (Ok .//file))
         (contents (Ok "modified\n")))
        |}];
      let%bind () =
        require_does_raise_async (fun () ->
          IO.async (fun () -> symlink_raw ?/"other" ~referring_to:""))
      in
      [%expect {| ("File_path.of_string: invalid string" "") |}];
      return ())
  ;;

  module Fd = struct
    type t = Fs.Fd.t
  end

  module Flock = struct
    type t = Fs.Flock.t
  end

  let flock = Fs.flock
  let flock_of_fd = Fs.flock_of_fd
  let flock_create = Fs.flock_create
  let try_flock = Fs.try_flock
  let try_flock_of_fd = Fs.try_flock_of_fd
  let try_flock_create = Fs.try_flock_create
  let funlock = Fs.funlock

  open struct
    (* helper for [flock] testing *)

    let test_flock ~flock ~try_flock ~exists ~shared1 ~shared2 =
      (* This function tests two locks interacting (or one failing).

         [create] is whether we allow the lock attempt to create the file.

         [exists] is whether the file exists before the lock attempt.

         [shared1] and [shared2] determine which locks are shared.

         We make sure to test [try_flock] for consistency with [flock] at least once in
         each scenario. *)
      within_temp_dir (fun () ->
        (* We make sure to test that the default for [~shared] behaves like [false]. *)
        let shared1 = Option.some_if shared1 true in
        let shared2 = Option.some_if shared2 true in
        let%bind () = if exists then run "touch" [ "file" ] else return () in
        match%bind
          (* try [flock], see what happens *)
          Deferred.Or_error.try_with ~extract_exn:true (fun () ->
            IO.async (fun () -> flock ?shared:shared1 ?/"file"))
        with
        | Error _ ->
          (* if [flock] fails, make sure [try_flock] is consistent *)
          print_s [%message "initial lock failed"];
          require_does_raise_async (fun () ->
            IO.async (fun () -> try_flock ?shared:shared1 ?/"file"))
        | Ok lock1 ->
          (* if the first lock succeeds, try [try_flock] *)
          (match%bind IO.async (fun () -> try_flock ?shared:shared2 ?/"file") with
           | Some lock2 ->
             (* if [try_flock] is concurrent, make sure [flock] is consistent *)
             print_s [%message "locks are concurrent"];
             let%bind lock3 = IO.async (fun () -> flock ?shared:shared2 ?/"file") in
             let%bind () = IO.async (fun () -> funlock lock1) in
             let%bind () = IO.async (fun () -> funlock lock2) in
             let%bind () = IO.async (fun () -> funlock lock3) in
             return ()
           | None ->
             (* if [try_flock] is exclusive, make sure [flock] is consistent *)
             print_s [%message "locks are mutually exclusive"];
             let lock2_deferred = IO.async (fun () -> flock ?shared:shared2 ?/"file") in
             (match%bind
                Clock_ns.with_timeout (Time_ns.Span.of_int_ms 100) lock2_deferred
              with
              | `Timeout ->
                (* make sure lock2 works after unlocking lock1 *)
                let%bind () = IO.async (fun () -> funlock lock1) in
                let%bind lock2 = lock2_deferred in
                let%bind () = IO.async (fun () -> funlock lock2) in
                return ()
              | `Result lock2 ->
                (* [flock] should not succeed when [try_flock] fails *)
                print_cr
                  [%message
                    "locks are not consistently mutually exclusive; this failure may be \
                     nondeterministic but it signifies a problem with [flock]"];
                let%bind () = IO.async (fun () -> funlock lock2) in
                return ())))
    ;;
  end

  let%expect_test "flock, sequential" =
    (* We test all possible inputs to [test_flock]. We capture output in one [%expect]
       block per expected outcome, making sure each case for that outcome produces the
       same output. *)
    Deferred.List.iter ~how:`Sequential Bool.all ~f:(fun create ->
      Deferred.List.iter ~how:`Sequential Bool.all ~f:(fun exists ->
        Deferred.List.iter ~how:`Sequential Bool.all ~f:(fun shared1 ->
          Deferred.List.iter ~how:`Sequential Bool.all ~f:(fun shared2 ->
            let flock, try_flock =
              if create
              then flock_create ?perm:None, try_flock_create ?perm:None
              else flock, try_flock
            in
            let%map () = test_flock ~flock ~try_flock ~exists ~shared1 ~shared2 in
            if not (create || exists)
            then
              (* fail if locking a nonexistent file *)
              [%expect
                {|
                "initial lock failed"
                (Unix.Unix_error
                 "No such file or directory"
                 open
                 "((filename file) (mode (O_RDONLY O_CLOEXEC)) (perm 0o644))")
                |}]
            else if shared1 && shared2
            then
              (* concurrent locks if both shared *)
              [%expect {| "locks are concurrent" |}]
            else
              (* mutally exclusive locks otherwise *)
              [%expect {| "locks are mutually exclusive" |}]))))
  ;;

  let%expect_test "flock, concurrent" =
    (* We test for race conditions in locking, especially with the [create] flag. *)
    let run_until_failure n f =
      Deferred.repeat_until_finished n (fun n ->
        match n with
        | 0 -> return (`Finished ())
        | _ ->
          (match%map f () with
           | Ok () -> `Repeat (n - 1)
           | Error error ->
             print_cr [%sexp (error : Error.t)];
             `Finished ()))
    in
    run_until_failure 100 (fun () ->
      within_temp_dir (fun () ->
        let attempts =
          List.init 10 ~f:(fun _ -> IO.async (fun () -> Fs.flock_create ?/"file"))
        in
        (* wait for at least one lock *)
        let%bind () = choose (List.map attempts ~f:(fun lock -> choice lock ignore)) in
        let%bind () = Scheduler.yield_until_no_jobs_remain () in
        (* find all successful locks *)
        let locks = List.filter_map attempts ~f:Deferred.peek in
        (* clean up after ourselves *)
        let%map () =
          List.map attempts ~f:(fun lock ->
            let%bind lock in
            IO.async (fun () -> Fs.funlock lock))
          |> Deferred.all_unit
        in
        (* check results *)
        match locks with
        | [] -> error_s [%message "did not lock anything"]
        | [ _ ] -> Ok ()
        | multiple ->
          let count = List.length multiple in
          error_s [%message "locked multiple times" (count : int)]))
  ;;

  open struct
    module With_flock_kind = struct
      type t =
        | With_flock
        | With_flock_create
        | With_flock_of_fd
      [@@deriving enumerate]

      let to_fn = function
        | With_flock -> Fs.with_flock
        | With_flock_create -> Fs.with_flock_create ?perm:None
        | With_flock_of_fd ->
          fun ?shared file_path ~f ->
            IO.with_fd file_path ~f:(fun fd -> Fs.with_flock_of_fd ?shared fd ~f)
      ;;
    end
  end

  let with_flock = Fs.with_flock
  let with_flock_create = Fs.with_flock_create
  let with_flock_of_fd = Fs.with_flock_of_fd

  let%expect_test "[with_flock]" =
    Deferred.List.iter ~how:`Sequential With_flock_kind.all ~f:(fun kind ->
      within_temp_dir (fun () ->
        let with_f = With_flock_kind.to_fn kind in
        let with_ name f =
          IO.async (fun () ->
            with_f ?/"file" ~f:(fun _ ->
              (* entering the critical section *)
              print_s [%message name "acquired"];
              IO.of_async (fun () ->
                let deferred = f () in
                match%map Clock_ns.with_timeout (Time_ns.Span.of_int_ms 100) deferred with
                | `Timeout ->
                  (* exiting the critical section *)
                  print_s [%message name "released"];
                  deferred
                | `Result value ->
                  (* oops, we entered the next critical section before exiting this one *)
                  print_cr [%message name "concurrent"];
                  return value)))
          |> Deferred.join
        in
        let%bind () =
          match kind with
          | With_flock | With_flock_of_fd -> run "touch" [ "file" ]
          | With_flock_create -> return ()
        in
        let%bind () =
          with_ "lock1" (fun _ ->
            with_ "lock2" (fun _ ->
              with_ "lock3" (fun _ ->
                IO.async (fun () ->
                  (* making sure there's a final mutually-exclusive critical section *)
                  let%bind.IO lock = flock ?/"file" in
                  print_s [%message "lock4" "acquired"];
                  funlock lock))))
        in
        [%expect
          {|
          (lock1 acquired)
          (lock1 released)
          (lock2 acquired)
          (lock2 released)
          (lock3 acquired)
          (lock3 released)
          (lock4 acquired)
          |}];
        return ()))
  ;;

  let flock_fd = Fs.flock_fd

  (* We have no generic operations on file descriptors currently, so there is nothing to
     test for [flock_fd]. *)

  open struct
    (* helpers for temp file tests *)

    (* Supply optional arguments to a temp file creating function. *)
    let wrap f ~in_dir ~perm ~prefix ~suffix arg =
      f
        ?in_dir:(Some (File_path.of_absolute in_dir))
        ?perm:(Some perm)
        ?prefix:(Some prefix)
        ?suffix:(Some suffix)
        arg
    ;;

    let apply_umask perm ~umask =
      let open File_permissions.Operators in
      perm land (umask lxor File_permissions.all_including_special_mode_bits)
    ;;

    (* Check a temporary file or directory after creation. *)
    let check path ~in_dir ~perm ~umask ~prefix ~suffix ~expect_directory =
      let exists = Sys_unix.file_exists_exn (File_path.Absolute.to_string path) in
      require exists;
      let is_directory = Sys_unix.is_directory_exn (File_path.Absolute.to_string path) in
      require_equal (module Bool) is_directory expect_directory;
      require_equal
        (module File_path.Absolute)
        (File_path.Absolute.dirname_exn path)
        in_dir;
      let name = File_path.Part.to_string (File_path.Absolute.basename_exn path) in
      require (String.is_prefix name ~prefix);
      require (String.is_suffix name ~suffix);
      let stats = Core_unix.lstat (File_path.Absolute.to_string path) in
      require_equal
        (module File_permissions)
        (File_permissions.of_int_exn stats.st_perm)
        (apply_umask perm ~umask)
    ;;

    let with_umask umask ~f =
      let original = Core_unix.umask (File_permissions.to_int umask) in
      let%map () = f ~umask in
      let (_ : int) = Core_unix.umask original in
      ()
    ;;

    let with_umasks f =
      Deferred.List.iter
        ~how:`Sequential
        [ File_permissions.of_int_exn 0o022; File_permissions.of_int_exn 0o077 ]
        ~f:(fun umask -> with_umask umask ~f)
    ;;

    (* Test a temp file creating function. *)
    let test f ~expect_directory =
      let f = wrap f in
      with_umasks (fun ~umask ->
        with_temp_dir (fun in_dir ->
          let in_dir = File_path.Absolute.of_string in_dir in
          let perm =
            if expect_directory then File_permissions.ug_rwx else File_permissions.ug_rw
          in
          let prefix = "the_prefix" in
          let suffix = "the_suffix" in
          (* create and check two files... *)
          let%bind path1 = IO.async (fun () -> f ~in_dir ~perm ~prefix ~suffix ()) in
          check path1 ~in_dir ~perm ~umask ~prefix ~suffix ~expect_directory;
          let%bind path2 = IO.async (fun () -> f ~in_dir ~perm ~prefix ~suffix ()) in
          check path2 ~in_dir ~perm ~umask ~prefix ~suffix ~expect_directory;
          (* ...and make sure they are unique. *)
          require
            (not (File_path.Absolute.equal path1 path2))
            ~if_false_then_print_s:
              (lazy
                [%message
                  "temp files not unique"
                    (path1 : File_path.Absolute.t)
                    (path2 : File_path.Absolute.t)]);
          return ()))
    ;;

    let modify path ~remove =
      (* Act on the temp file to make sure it gets cleaned up properly regardless of
         usage, allowing coverage for both keeping and deleting the file. *)
      let open IO.Let_syntax in
      let path = File_path.of_absolute path in
      let%bind is_dir = is_directory_exn path in
      match remove, is_dir with
      | false, false -> write_file path ~contents:"modified"
      | false, true -> write_file (path /?. ~."temp-file") ~contents:"added"
      | true, false -> unlink path
      | true, true -> rmdir path
    ;;

    (* Test a [with] function for temporary files. *)
    let test_with f ~expect_directory ~remove =
      let f = wrap (fun ?in_dir -> f ?in_dir ?on_cleanup_error:None) in
      with_umasks (fun ~umask ->
        (* Create an outer temporary directory using expect test helpers. *)
        with_temp_dir (fun in_dir ->
          let in_dir = File_path.Absolute.of_string in_dir in
          let perm =
            if expect_directory then File_permissions.ug_rwx else File_permissions.ug_rw
          in
          let prefix = "the_prefix" in
          let suffix = "the_suffix" in
          let%bind path =
            let open IO.Let_syntax in
            IO.async (fun () ->
              f ~in_dir ~perm ~prefix ~suffix (fun temp ->
                (* Check, then modify, the temporary file. *)
                check temp ~in_dir ~perm ~umask ~prefix ~suffix ~expect_directory;
                let%map () = modify temp ~remove in
                temp))
          in
          (* Make sure the temporary file gets cleaned up. *)
          require (not (Sys_unix.file_exists_exn (File_path.Absolute.to_string path)));
          return ()))
    ;;
  end

  let create_temp_file = Fs.create_temp_file

  let%expect_test "[create_temp_file]" =
    let%bind () = test create_temp_file ~expect_directory:false in
    [%expect {| |}];
    return ()
  ;;

  let create_temp_dir = Fs.create_temp_dir

  let%expect_test "[create_temp_dir]" =
    let%bind () = test create_temp_dir ~expect_directory:true in
    [%expect {| |}];
    return ()
  ;;

  let with_temp_file = Fs.with_temp_file

  let%expect_test "[with_temp_file]" =
    let%bind () = test_with with_temp_file ~expect_directory:false ~remove:false in
    [%expect {| |}];
    let%bind () = test_with with_temp_file ~expect_directory:false ~remove:true in
    [%expect {| |}];
    return ()
  ;;

  let with_temp_dir = Fs.with_temp_dir

  let%expect_test "[with_temp_dir]" =
    let%bind () = test_with with_temp_dir ~expect_directory:true ~remove:false in
    [%expect {| |}];
    let%bind () = test_with with_temp_dir ~expect_directory:true ~remove:true in
    [%expect {| |}];
    return ()
  ;;

  let within_temp_dir = Fs.within_temp_dir

  let%expect_test "[within_temp_dir]" =
    let test ~remove =
      test_with
        ~expect_directory:true
        ~remove
        (fun ?in_dir ?on_cleanup_error ?perm ?prefix ?suffix f ->
           let open IO.Let_syntax in
           let above = Sys_unix.getcwd () in
           let%map result =
             within_temp_dir ?on_cleanup_error ?in_dir ?perm ?prefix ?suffix (fun () ->
               f (File_path.Absolute.of_string (Sys_unix.getcwd ())))
           in
           let below = Sys_unix.getcwd () in
           require_equal (module String) above below;
           result)
    in
    let%bind () = test ~remove:false in
    [%expect {| |}];
    let%bind () = test ~remove:true in
    [%expect {| |}];
    return ()
  ;;

  (** Put new tests above temp dir tests, as they shadow [with_temp_dir] and
      [within_temp_dir] from [Expect_test_helpers_async]. *)
end

module Test_filesystem_core =
  Test_filesystem
    (struct
      include Monad.Ident

      let async f = In_thread.run f
      let of_async f = Thread_safe.block_on_async_exn f

      module Fd = Core_unix.File_descr

      let with_fd path ~f = Core_unix.with_file ?/$path ~mode:[ O_RDWR; O_CREAT ] ~f
    end)
    (Filesystem_core)

module Test_filesystem_async =
  Test_filesystem
    (struct
      include Deferred

      let async f = f ()
      let of_async f = f ()

      module Fd = Unix.Fd

      let with_fd path ~f = Unix.with_file ?/$path ~mode:[ `Rdwr; `Creat ] ~f
    end)
    (Filesystem_async)
