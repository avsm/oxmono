exception Error of string
exception Busy = Lock.Busy

let fail message = raise (Error message)
let regular stat = stat.Unix.st_kind = Unix.S_REG

let private_ stat =
  stat.Unix.st_uid = Unix.getuid () && stat.st_perm land 0o077 = 0

let same a b = a.Unix.st_dev = b.Unix.st_dev && a.st_ino = b.st_ino

let read ~private_file ~max_bytes path =
  if max_bytes < 0 || max_bytes = max_int then
    invalid_arg "Native: invalid read bound";
  let valid stat = regular stat && ((not private_file) || private_ stat) in
  let before = Unix.lstat path in
  if not (regular before) then fail "Not a regular file";
  if not (valid before) then fail "File must be owned and private (mode 0600)";
  let fd =
    Unix.openfile path [ Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC ] 0
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let opened = Unix.fstat fd in
      let current = Unix.lstat path in
      if
        not
          (valid opened && valid current && same before opened
         && same current opened)
      then fail "File changed while opening";
      if opened.st_size > max_bytes then fail "File exceeds size limit";
      let buffer = Buffer.create (min opened.st_size 4096) in
      let chunk = Bytes.create (min (max_bytes + 1) 4096) in
      let rec loop () =
        let remaining = max_bytes - Buffer.length buffer in
        let length = min (Bytes.length chunk) (remaining + 1) in
        match Unix.read fd chunk 0 length with
        | 0 -> Buffer.contents buffer
        | n when n > remaining -> fail "File exceeds size limit"
        | n ->
            Buffer.add_subbytes buffer chunk 0 n;
            loop ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      in
      loop ())

let read_regular ~max_bytes path = read ~private_file:false ~max_bytes path
let read_private ~max_bytes path = read ~private_file:true ~max_bytes path

let rec parents path =
  match Unix.stat path with
  | stat when stat.Unix.st_kind = Unix.S_DIR -> ()
  | _ -> fail "Not a directory"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
      let parent = Filename.dirname path in
      if parent = path then fail "Cannot create directory";
      parents parent;
      try Unix.mkdir path 0o700
      with Unix.Unix_error (Unix.EEXIST, _, _) -> parents path)

let mkdir_private path =
  parents path;
  if (Unix.lstat path).Unix.st_kind <> Unix.S_DIR then
    fail "Directory must not be a symlink"

let require_private_dir path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR || not (private_ stat) then
    fail "Directory must be owned and private (mode 0700)"

let fsync_dir path =
  let fd = Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let publish ~mode path data install =
  let directory = Filename.dirname path in
  let temp, channel =
    Filename.open_temp_file ~temp_dir:directory ".eioext-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr channel;
      try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      let fd = Unix.descr_of_out_channel channel in
      Unix.set_close_on_exec fd;
      Unix.fchmod fd mode;
      output_string channel data;
      flush channel;
      Unix.fsync fd;
      close_out channel;
      install temp;
      fsync_dir directory)

let atomic_write ?(mode = 0o600) ?(check = fun () -> ()) path data =
  publish ~mode path data (fun temp ->
      check ();
      Unix.rename temp path)

let create_private path data =
  publish ~mode:0o600 path data (fun temp -> Unix.link temp path)

let with_lock ?wait ~path f =
  let parent = Unix.realpath (Filename.dirname path) in
  let name = Filename.basename path in
  let path = Filename.concat parent name in
  let stat = Unix.stat parent in
  Lock.with_lock ?wait ~device:(Int64.of_int stat.st_dev)
    ~inode:(Int64.of_int stat.st_ino) ~name (fun () ->
      let retry () =
        match wait with Some wait -> wait () | None -> raise (Busy path)
      in
      let before =
        match Unix.lstat path with
        | stat ->
            if not (regular stat && private_ stat && stat.st_nlink = 1) then
              fail "Lock must be an owned private regular file";
            Some stat
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
      in
      let fd =
        Unix.openfile path
          [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_CLOEXEC ]
          0o600
      in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          let opened = Unix.fstat fd and current = Unix.lstat path in
          if
            not
              (regular opened && private_ opened && opened.st_nlink = 1
             && regular current && same current opened
              && Option.fold ~none:true ~some:(fun s -> same s opened) before)
          then fail "Lock changed while opening";
          let rec acquire () =
            match Unix.lockf fd Unix.F_TLOCK 0 with
            | () -> ()
            | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
                retry ();
                acquire ()
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> acquire ()
          in
          acquire ();
          Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) f))
