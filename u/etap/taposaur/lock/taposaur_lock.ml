external try_lock : Unix.file_descr -> bool = "taposaur_try_lock"

type owner = { pid : int; host : string; component : string; at : string }
type t = { fd : Unix.file_descr; mutable held : bool }
exception Busy of owner option

let owner_jsont =
  Jsont.Object.map (fun pid host component at -> { pid; host; component; at })
  |> Jsont.Object.mem "pid" Jsont.int ~enc:(fun o -> o.pid)
  |> Jsont.Object.mem "host" Jsont.string ~enc:(fun o -> o.host)
  |> Jsont.Object.mem "component" Jsont.string ~enc:(fun o -> o.component)
  |> Jsont.Object.mem "at" Jsont.string ~enc:(fun o -> o.at)
  |> Jsont.Object.finish

let () = Printexc.Safe.register_printer (function
  | Busy None -> Some "Taposaur_lock.Busy: lock is held"
  | Busy (Some o) -> Some (Printf.sprintf
      "Taposaur_lock.Busy: lock held by pid %d on %s (%s, since %s)"
      o.pid o.host o.component o.at)
  | _ -> None)

let now () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let read fd =
  (* The record is at most a few hundred bytes. Read a bounded window. *)
  let buffer = Bytes.create 4096 in
  let rec loop total =
    if total >= Bytes.length buffer then total else
    match Unix.read fd buffer total (Bytes.length buffer - total) with
    | 0 -> total
    | n -> loop (total + n) in
  Bytes.sub_string buffer 0 (loop 0)

let decode text =
  if String.trim text = "" then None else
  match Jsont_bytesrw.decode_string owner_jsont text with
  | Ok o -> Some o | Error _ -> None

(* OCaml's Unix module has no O_NOFOLLOW, so a symbolic link is refused
   by inspecting the path first and checking the opened file is the same
   regular inode afterwards. *)
let refuse_link path = match Unix.lstat path with
  | { st_kind = Unix.S_LNK; _ } ->
      failwith ("Lock path is a symbolic link: " ^ path)
  | { st_kind = Unix.S_REG; _ } as st -> Some st
  | _ -> failwith ("Lock path is not a regular file: " ^ path)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
let same_file (a : Unix.stats) (b : Unix.stats) =
  a.st_dev = b.st_dev && a.st_ino = b.st_ino
let check_opened path before fd = match Unix.fstat fd with
  | { st_kind = Unix.S_REG; _ } as after when
      Option.fold ~none:true ~some:(same_file after) before -> ()
  | _ -> Unix.close fd; failwith ("Lock path is not a regular file: " ^ path)
  | exception e -> Unix.close fd; raise e

let holder path =
  match refuse_link path with
  | exception Failure _ -> None
  | None -> None
  | before ->
      match Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 with
      | exception Unix.Unix_error _ -> None
      | fd ->
          match check_opened path before fd with
          | exception Failure _ -> None
          | () -> Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
              try decode (read fd) with Unix.Unix_error _ -> None)

let acquire ?(component = "taposaur") path =
  let before = refuse_link path in
  let fd = Unix.openfile path
    [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o600 in
  check_opened path before fd;
  match try_lock fd with
  | exception e -> Unix.close fd; raise e
  | false ->
      let owner = try decode (read fd) with Unix.Unix_error _ -> None in
      Unix.close fd;
      raise (Busy owner)
  | true ->
      (try
        let owner = { pid = Unix.getpid (); host = Unix.gethostname ();
          component; at = now () } in
        let text = match Jsont_bytesrw.encode_string owner_jsont owner with
          | Ok s -> s ^ "\n" | Error e -> failwith e in
        Unix.ftruncate fd 0;
        ignore (Unix.lseek fd 0 Unix.SEEK_SET);
        let rec write i = if i < String.length text then
          write (i + Unix.write_substring fd text i (String.length text - i)) in
        write 0
      with e -> Unix.close fd; raise e);
      { fd; held = true }

let release t =
  if t.held then begin
    t.held <- false;
    (try Unix.ftruncate t.fd 0 with Unix.Unix_error _ -> ());
    Unix.close t.fd
  end

let with_lock ?component path f =
  let t = acquire ?component path in
  Fun.protect ~finally:(fun () -> release t) f
