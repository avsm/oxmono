(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

exception Error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt
let get_ok = function Ok x -> x | Error e -> fail "%s" e

type value = Yamlrw.value

let str s : value = `String s
let int n : value = `Float (float_of_int n)
let obj xs : value = `O xs
let arr xs : value = `A xs
let string = function `String s -> s | _ -> fail "expected a string"
let assoc = function `O xs -> xs | _ -> fail "expected an object"
let list = function `A xs -> xs | _ -> fail "expected an array"

let number = function
  | `Float n when Float.is_integer n -> int_of_float n
  | _ -> fail "expected an integer"

let bool = function `Bool b -> b | _ -> fail "expected a boolean"
let find k v = List.assoc_opt k (assoc v)

let get k v =
  match find k v with Some x -> x | None -> fail "missing field: %s" k

let field k v = string (get k v)
let items k v = match find k v with None -> [] | Some x -> list x

let set k x v =
  let xs = assoc v in
  if List.mem_assoc k xs then
    obj (List.map (fun (n, y) -> (n, if n = k then x else y)) xs)
  else obj (xs @ [ (k, x) ])

let remove k v = obj (List.remove_assoc k (assoc v))

let rec canonical : value -> value = function
  | `O xs ->
      obj (List.sort compare (List.map (fun (k, v) -> (k, canonical v)) xs))
  | `A xs -> arr (List.map canonical xs)
  | x -> x

let equal a b = canonical a = canonical b

let rec unique : value -> unit = function
  | `O xs ->
      let seen = Hashtbl.create 16 in
      List.iter
        (fun (k, v) ->
          if Hashtbl.mem seen k then fail "duplicate key: %s" k;
          Hashtbl.add seen k ();
          unique v)
        xs
  | `A xs -> List.iter unique xs
  | _ -> ()

let yaml s =
  let v = Yamlrw.of_string s in
  unique v;
  v

let read path = In_channel.with_open_bin path In_channel.input_all
let load_yaml path = yaml (read path)
let digest s = Digestif.SHA256.(to_hex (digest_string s))

let exists p =
  try
    ignore (Unix.lstat p);
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec absolute p =
  if exists p then Unix.realpath p
  else
    let parent = Filename.dirname p in
    if parent = p then p
    else Filename.concat (absolute parent) (Filename.basename p)

let under ~root p = p = root || String.starts_with ~prefix:(root ^ "/") p

let separate output protected =
  let output = absolute output in
  List.iter
    (fun p ->
      let p = absolute p in
      if under ~root:p output || under ~root:output p then
        fail "output must be outside the source and saved contact/journal trees")
    protected

let fresh p = if exists p then fail "output directory must be new: %s" p

let safe_path root relative =
  let parts = String.split_on_char '/' relative in
  if
    (not (Filename.is_relative relative))
    || relative = ""
    || List.exists (fun s -> s = ".." || s = ".") parts
  then fail "not a relative path within the root: %s" relative;
  let target = Filename.concat root relative in
  if not (under ~root:(absolute root) (absolute target)) then
    fail "path escapes root: %s" relative;
  target

let rec mkdir p =
  if not (exists p) then (
    mkdir (Filename.dirname p);
    Unix.mkdir p 0o700)

let write ?(mode = 0o600) path data =
  let fd =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] mode
  in
  let oc = Unix.out_channel_of_descr fd in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc data;
      flush oc;
      Unix.fsync fd)

let atomic_write ?(mode = 0o600) ?(check = fun () -> ()) path data =
  let temp, oc =
    Filename.open_temp_file ~temp_dir:(Filename.dirname path) ".sortal-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      if exists temp then Unix.unlink temp)
    (fun () ->
      Unix.fchmod (Unix.descr_of_out_channel oc) mode;
      output_string oc data;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      check ();
      Unix.rename temp path)

let inventory root =
  let rec walk relative =
    let path = if relative = "" then root else Filename.concat root relative in
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_REG -> [ relative ]
    | Unix.S_DIR ->
        Sys.readdir path |> Array.to_list |> List.sort String.compare
        |> List.concat_map (fun n ->
            walk (if relative = "" then n else relative ^ "/" ^ n))
    | _ -> fail "cannot archive special file or symlink: %s" path
  in
  walk "" |> List.sort String.compare

let rec remove_tree p =
  if (Unix.lstat p).Unix.st_kind = Unix.S_DIR then (
    Array.iter (fun n -> remove_tree (Filename.concat p n)) (Sys.readdir p);
    Unix.rmdir p)
  else Unix.unlink p

let check_keys value allowed location =
  List.iter
    (fun (key, _) ->
      if not (List.mem key allowed) then
        fail "unmapped Sortal field at %s: %s" location key)
    (assoc value)

let percent ?(safe = "") s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
          Buffer.add_char b c
      | _ when String.contains safe c -> Buffer.add_char b c
      | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let uri s = percent ~safe:":/?#[]@!$&'()*+,;=%" (String.trim s)

let uuid_bytes s =
  let hex = String.concat "" (String.split_on_char '-' s) in
  if String.length hex <> 32 then fail "invalid store UUID";
  try
    String.init 16 (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
  with _ -> fail "invalid store UUID"

let uuid version s =
  let b = Bytes.of_string (String.sub s 0 16) in
  Bytes.set b 6
    (Char.chr (Char.code (Bytes.get b 6) land 15 lor (version lsl 4)));
  Bytes.set b 8 (Char.chr (Char.code (Bytes.get b 8) land 63 lor 128));
  let hex =
    String.concat ""
      (List.init 16 (fun i -> Printf.sprintf "%02x" (Char.code (Bytes.get b i))))
  in
  String.concat "-"
    (List.map
       (fun (a, n) -> String.sub hex a n)
       [ (0, 8); (8, 4); (12, 4); (16, 4); (20, 12) ])

let new_uuid () =
  let bytes =
    In_channel.with_open_bin "/dev/urandom" (fun ic ->
        really_input_string ic 16)
  in
  uuid 4 bytes

let contact_uuid store handle =
  uuid 5
    Digestif.SHA1.(to_raw_string (digest_string (uuid_bytes store ^ handle)))

let today () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let rec to_json : value -> Jsont.json = function
  | `Null -> Jsont.Json.null ()
  | `Bool b -> Jsont.Json.bool b
  | `Float n -> Jsont.Json.number n
  | `String s -> Jsont.Json.string s
  | `A xs -> Jsont.Array (List.map to_json xs, Jsont.Meta.none)
  | `O xs ->
      Jsont.Object
        ( List.map (fun (k, v) -> ((k, Jsont.Meta.none), to_json v)) xs,
          Jsont.Meta.none )

let rec of_json : Jsont.json -> value = function
  | Jsont.Null _ -> `Null
  | Jsont.Bool (b, _) -> `Bool b
  | Jsont.Number (n, _) -> `Float n
  | Jsont.String (s, _) -> `String s
  | Jsont.Array (xs, _) -> arr (List.map of_json xs)
  | Jsont.Object (xs, _) ->
      obj (List.map (fun ((k, _), v) -> (k, of_json v)) xs)

let json_string ?(pretty = false) v =
  get_ok
    (Jsont_bytesrw.encode_string
       ~format:(if pretty then Jsont.Indent else Jsont.Minify)
       Jsont.json (to_json v))

let json s =
  let v = of_json (get_ok (Jsont_bytesrw.decode_string Jsont.json s)) in
  unique v;
  v

let load_json p = json (read p)
let save_json p v = atomic_write p (json_string ~pretty:true v ^ "\n")
