(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
exception Error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt
let get_ok = function Ok x -> x | Error e -> fail "%s" e

type value = Yamlrw.value

let str s : value = `String s
let obj xs : value = `O xs
let arr xs : value = `A xs
let string = function `String s -> s | _ -> fail "expected a string"
let assoc = function `O xs -> xs | _ -> fail "expected an object"
let list = function `A xs -> xs | _ -> fail "expected an array"
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
  | `Float n when not (Float.is_finite n) -> fail "non-finite number"
  | _ -> ()

let yaml s =
  (* Inspect events before conversion so aliases and custom tags cannot be
     expanded away or silently discarded. *)
  let events = Yamlrw.Parser.to_list (Yamlrw.Parser.of_string s) in
  List.iter
    (fun e ->
      match e.Yamlrw.Event.event with
      | Alias _ -> fail "YAML aliases are not supported"
      | Scalar { anchor = Some _; _ }
      | Sequence_start { anchor = Some _; _ }
      | Mapping_start { anchor = Some _; _ } ->
          fail "YAML anchors are not supported"
      | Scalar { tag = Some _; _ }
      | Sequence_start { tag = Some _; _ }
      | Mapping_start { tag = Some _; _ } ->
          fail "explicit YAML tags are not supported"
      | _ -> ())
    events;
  let v =
    Yamlrw.of_string ~resolve_aliases:false ~max_nodes:100_000 ~max_depth:64 s
  in
  unique v;
  v

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
  | Jsont.String (s, _) -> str s
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

let digest s = Digestif.SHA256.(to_hex (digest_string s))

let exists p =
  try
    ignore (Unix.lstat p);
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let regular p =
  if (Unix.lstat p).Unix.st_kind <> Unix.S_REG then
    fail "not a regular file: %s" p

let read ?(limit = 16 * 1024 * 1024) p =
  regular p;
  let st = Unix.stat p in
  if st.st_size > limit then fail "file exceeds size limit: %s" p;
  In_channel.with_open_bin p (fun ic ->
      let data = In_channel.input_all ic in
      if String.length data > limit then fail "file exceeds size limit: %s" p;
      data)

let read_opt p = if exists p then Some (read p) else None

let rec mkdir p =
  if exists p then (
    if (Unix.lstat p).Unix.st_kind <> Unix.S_DIR then
      fail "not a directory: %s" p)
  else (
    mkdir (Filename.dirname p);
    Unix.mkdir p 0o700)

let fsync_dir p =
  let fd = Unix.openfile p [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let atomic_write ?(check = fun () -> ()) p data =
  let tmp, oc =
    Filename.open_temp_file ~temp_dir:(Filename.dirname p) ".dooit-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      if exists tmp then Unix.unlink tmp)
    (fun () ->
      Unix.fchmod (Unix.descr_of_out_channel oc) 0o600;
      output_string oc data;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      check ();
      Unix.rename tmp p;
      fsync_dir (Filename.dirname p))

let create_file p data =
  let tmp, oc =
    Filename.open_temp_file ~temp_dir:(Filename.dirname p) ".dooit-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      if exists tmp then Unix.unlink tmp)
    (fun () ->
      Unix.fchmod (Unix.descr_of_out_channel oc) 0o600;
      output_string oc data;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      (* Linking a complete temporary file publishes it without a
         check-then-rename race against another creator. *)
      Unix.link tmp p;
      fsync_dir (Filename.dirname p))

let save_json p v = atomic_write p (json_string ~pretty:true v ^ "\n")
let load_json p = json (read p)

let rec absolute p =
  if exists p then Unix.realpath p
  else
    let parent = Filename.dirname p in
    if parent = p then p
    else Filename.concat (absolute parent) (Filename.basename p)

let under ~root p = p = root || String.starts_with ~prefix:(root ^ "/") p

let separate a b =
  let a = absolute a and b = absolute b in
  if under ~root:a b || under ~root:b a then
    fail "report must be outside the note store"

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

let valid_uuid s =
  String.length s = 36
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' | '-' -> true | _ -> false)
       s
  && List.for_all (fun i -> s.[i] = '-') [ 8; 13; 18; 23 ]
  && String.length (String.concat "" (String.split_on_char '-' s)) = 32

let check_uuid s = if not (valid_uuid s) then fail "invalid UUID: %s" s

let new_uuid () =
  uuid 4
    (In_channel.with_open_bin "/dev/urandom" (fun ic ->
         really_input_string ic 16))

let uuid5 namespace name =
  check_uuid namespace;
  let hex = String.concat "" (String.split_on_char '-' namespace) in
  let raw =
    String.init 16 (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
  in
  uuid 5 Digestif.SHA1.(to_raw_string (digest_string (raw ^ name)))

let now () =
  Ptime.to_rfc3339 (Option.get (Ptime.of_float_s (Unix.gettimeofday ())))

let instant s =
  match Ptime.of_rfc3339 s with
  | Ok (_, _, n) when n = String.length s -> ()
  | _ -> fail "invalid RFC 3339 timestamp"

let keys v = List.map fst (assoc v)
let opt_value = function None -> `Null | Some v -> v
let opt_string = function None -> `Null | Some s -> str s
let string_opt = function `Null -> None | v -> Some (string v)

let normalize s =
  (* Unicode folding expands characters such as sharp s. *)
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then fail "invalid UTF-8";
      let u = Uchar.utf_decode_uchar d in
      (match Uucp.Case.Fold.fold u with
      | `Self -> Buffer.add_utf_8_uchar b u
      | `Uchars us -> List.iter (Buffer.add_utf_8_uchar b) us);
      loop (i + Uchar.utf_decode_length d))
  in
  loop 0;
  Uunf_string.normalize_utf_8 `NFC (Buffer.contents b)
