(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Byte_range = Zarrz.Byte_range
module Error = Zarrz.Error
module Store = Zarrz.Store
module I63 = Optint.Int63

let err fmt = Printf.ksprintf (fun m -> Error.raise_ (Error.Store m)) fmt

(* {1 Keys}

   A key is joined to the root as a relative path. [Eio.Path.( / )]
   replaces the path when the step is absolute and keeps [..] as it
   finds it, so a key carrying either would name a file outside the
   directory the caller granted. The specification's keys never do, and
   checking here is what makes that a property of the store rather than
   of its callers. *)

let bad_component = function "" | "." | ".." -> true | _ -> false

let check_key ~op key =
  if key = "" then err "%s: the empty key names no object" op;
  if List.exists bad_component (String.split_on_char '/' key) then
    err "%s: %S is not a relative path" op key

(* A prefix is a key with its last component cut short, so its last
   component may be empty, from a trailing ['/'], or a partial name. *)
let check_prefix prefix =
  if prefix <> "" then
    let cs = String.split_on_char '/' prefix in
    let rec check = function
      | [] -> ()
      | [ ("." | "..") ] -> err "list: %S is not a relative path" prefix
      | [ _ ] -> ()
      | c :: cs ->
          if bad_component c then err "list: %S is not a relative path" prefix
          else check cs
    in
    check cs

(* {1 Failures}

   Eio reports every filesystem failure as [Eio.Io]. Not-found is the
   one case a store answers with [None]. Everything else, a permission
   or an I/O failure, is a store failure. *)

let none_if_missing ~op f =
  try f () with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | Eio.Io _ as e -> err "%s: %s" op (Printexc.to_string e)

let wrap ~op f =
  try f () with Eio.Io _ as e -> err "%s: %s" op (Printexc.to_string e)

(* {1 Reading}

   The size comes from the open descriptor rather than a prior [stat],
   so the bytes read are the bytes of the file that was opened. A
   directory opens for reading on POSIX, hence the kind test: it is
   what makes a chunk directory an absent key rather than an [EISDIR]
   from the first read. *)

let with_file path f =
  Eio.Switch.run @@ fun sw ->
  let fd = Eio.Path.open_in ~sw path in
  let st = Eio.File.stat fd in
  match st.Eio.File.Stat.kind with
  | `Regular_file -> f fd (I63.to_int st.Eio.File.Stat.size)
  | _ -> None

let pread fd ~off ~len =
  let b = Base_bigstring.create len in
  if len > 0 then
    Eio.File.pread_exact fd ~file_offset:(I63.of_int off)
      [ Cstruct.of_bigarray b ];
  b

let get root ~key =
  check_key ~op:"get" key;
  none_if_missing ~op:"get" @@ fun () ->
  with_file Eio.Path.(root / key) (fun fd n -> Some (pread fd ~off:0 ~len:n))

let range fd ~size r =
  let off, len = Byte_range.resolve ~size r in
  pread fd ~off ~len

let get_range root ~key r =
  check_key ~op:"get_range" key;
  none_if_missing ~op:"get_range" @@ fun () ->
  with_file Eio.Path.(root / key) (fun fd n -> Some (range fd ~size:n r))

let get_ranges root ~key rs =
  check_key ~op:"get_ranges" key;
  none_if_missing ~op:"get_ranges" @@ fun () ->
  with_file Eio.Path.(root / key) (fun fd n ->
      Some (List.map (range fd ~size:n) rs))

let size root ~key =
  check_key ~op:"size" key;
  none_if_missing ~op:"size" @@ fun () ->
  let st = Eio.Path.stat ~follow:true Eio.Path.(root / key) in
  match st.Eio.File.Stat.kind with
  | `Regular_file -> Some (I63.to_int st.Eio.File.Stat.size)
  | _ -> None

(* {1 Writing} *)

let set root ~key b =
  check_key ~op:"set" key;
  wrap ~op:"set" @@ fun () ->
  (match String.rindex_opt key '/' with
  | None -> ()
  | Some i ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(root / String.sub key 0 i));
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
    Eio.Path.(root / key)
    (fun fd ->
      Eio.File.pwrite_all fd ~file_offset:I63.zero [ Cstruct.of_bigarray b ])

let erase root ~key =
  check_key ~op:"erase" key;
  wrap ~op:"erase" @@ fun () ->
  Eio.Path.unlink ~missing_ok:true Eio.Path.(root / key)

(* {1 Listing}

   The walk starts at the deepest directory the prefix names in full
   and descends only where a matching key can be. A directory key is
   worth entering when it and the prefix agree as far as the shorter of
   the two runs: a longer directory key must carry the whole prefix,
   and a shorter one must be a prefix of it. *)

let worth_entering ~prefix key =
  let n = String.length prefix and m = String.length key in
  if m >= n then String.starts_with ~prefix key
  else String.starts_with ~prefix:key prefix

let list root ~prefix =
  check_prefix prefix;
  wrap ~op:"list" @@ fun () ->
  let base =
    match String.rindex_opt prefix '/' with
    | None -> ""
    | Some i -> String.sub prefix 0 i
  in
  let acc = ref [] in
  let rec walk dir =
    let path = if dir = "" then root else Eio.Path.(root / dir) in
    let entries =
      try Eio.Path.read_dir_entries path
      with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []
    in
    List.iter
      (fun (kind, name) ->
        let key = if dir = "" then name else dir ^ "/" ^ name in
        (* [readdir] reports no kind on some filesystems, and a symlink
           by the link rather than its target. A read follows the link,
           so the listing must too, and a dangling one is nothing. *)
        let kind =
          match kind with
          | `Unknown | `Symbolic_link -> (
              match Eio.Path.kind ~follow:true Eio.Path.(root / key) with
              | `Regular_file -> `Regular_file
              | `Directory -> `Directory
              | _ -> `Unknown)
          | k -> k
        in
        match kind with
        | `Regular_file ->
            if String.starts_with ~prefix key then acc := key :: !acc
        | `Directory -> if worth_entering ~prefix key then walk key
        | _ -> ())
      entries
  in
  walk base;
  List.sort String.compare !acc

let store ?(writable = false) root =
  {
    Store.get = get root;
    get_range = get_range root;
    get_ranges = get_ranges root;
    size = size root;
    ranged = true;
    set = (if writable then Some (set root) else None);
    erase = (if writable then Some (erase root) else None);
    list = Some (list root);
  }
