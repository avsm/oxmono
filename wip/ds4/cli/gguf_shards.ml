(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The alignment the V4.1 loader needs, which is also the alignment the model
   had before it was split. *)
let alignment = 16384
let chunk = 16 * 1024 * 1024

(* A header larger than this is not a model file. *)
let header_limit = 128 * 1024 * 1024
let fail fmt = Printf.ksprintf failwith fmt
let align n a = (n + a - 1) / a * a

(* ---- reading ----------------------------------------------------------- *)

(* The fixed-width GGUF value kinds and their sizes. Strings are kind 8 and
   arrays kind 9. *)
let scalar_size = function
  | 0 | 1 | 7 -> Some 1
  | 2 | 3 -> Some 2
  | 4 | 5 | 6 -> Some 4
  | 10 | 11 | 12 -> Some 8
  | _ -> None

(* The tensor kinds a V4.1 MXFP4 model holds: F32, F16, Q8_0, I8 and MXFP4, as
   the number of elements per block and the bytes a block takes. *)
let tensor_kind = function
  | 0 -> Some (1, 4)
  | 1 -> Some (1, 2)
  | 8 -> Some (32, 34)
  | 24 -> Some (1, 1)
  | 39 -> Some (32, 17)
  | _ -> None

let split_keys = [ "split.no"; "split.count"; "split.tensors.count" ]

type value = Int of int | Str of string | Other

type tensor = {
  tname : string;
  shape : int list;
  kind : int;
  offset : int;  (** from the start of the shard's data *)
  nbytes : int;
}

type header = {
  kvs : (string * string) list;  (** each key with its raw encoding *)
  values : (string * value) list;
  kinds : (string * int) list;
  tensors : tensor list;
  data_offset : int;
}

let really_read ic n =
  match In_channel.really_input_string ic n with
  | Some s -> s
  | None -> fail "a shard ends before its header says it does"

let pos ic = Int64.to_int (In_channel.pos ic)
let seek ic n = In_channel.seek ic (Int64.of_int n)

let int_of_u64 v =
  if Int64.compare v 0L < 0 || Int64.compare v (Int64.of_int max_int) > 0 then
    fail "GGUF length out of range";
  Int64.to_int v

let read_u32 ic =
  Int32.to_int (String.get_int32_le (really_read ic 4) 0) land 0xFFFF_FFFF

let read_u64 ic = int_of_u64 (String.get_int64_le (really_read ic 8) 0)

let read_string ic =
  let n = read_u64 ic in
  if n > header_limit then fail "GGUF string exceeds the header limit";
  really_read ic n

(* The value at the channel, for the kinds a merge inspects. *)
let read_value ic kind =
  match kind with
  | 0 -> Int (Char.code (really_read ic 1).[0])
  | 1 -> Int (String.get_int8 (really_read ic 1) 0)
  | 2 -> Int (String.get_uint16_le (really_read ic 2) 0)
  | 3 -> Int (String.get_int16_le (really_read ic 2) 0)
  | 4 -> Int (read_u32 ic)
  | 5 -> Int (Int32.to_int (String.get_int32_le (really_read ic 4) 0))
  | 10 -> Int (read_u64 ic)
  | 11 -> Int (Int64.to_int (String.get_int64_le (really_read ic 8) 0))
  | 8 -> Str (read_string ic)
  | _ -> Other

let rec skip_value ic ~size ~depth kind =
  if depth > 4 then fail "nested GGUF metadata exceeds the limit";
  (match (kind, scalar_size kind) with
  | 8, _ ->
      let len = read_u64 ic in
      seek ic (pos ic + len)
  | 9, _ -> (
      let elt = read_u32 ic in
      let count = read_u64 ic in
      if count > size then fail "invalid GGUF array length";
      match scalar_size elt with
      | Some n -> seek ic (pos ic + (count * n))
      | None ->
          for _ = 1 to count do
            skip_value ic ~size ~depth:(depth + 1) elt
          done)
  | _, Some n -> seek ic (pos ic + n)
  | _, None -> fail "unsupported GGUF metadata kind %d" kind);
  if pos ic > min size header_limit then fail "invalid GGUF metadata extent"

let read_header ic =
  let size = Int64.to_int (In_channel.length ic) in
  if really_read ic 4 <> "GGUF" || read_u32 ic <> 3 then fail "expected GGUF v3";
  let nt = read_u64 ic in
  let nk = read_u64 ic in
  if nt > 100_000 || nk > 100_000 then fail "invalid GGUF directory count";
  let kvs = ref [] and values = ref [] and kinds = ref [] in
  for _ = 1 to nk do
    let start = pos ic in
    let key = read_string ic in
    let kind = read_u32 ic in
    if List.mem_assoc key !kinds then fail "duplicate GGUF metadata key %s" key;
    kinds := (key, kind) :: !kinds;
    let value_start = pos ic in
    skip_value ic ~size ~depth:0 kind;
    let stop = pos ic in
    seek ic value_start;
    values := (key, read_value ic kind) :: !values;
    seek ic start;
    kvs := (key, really_read ic (stop - start)) :: !kvs
  done;
  let values = List.rev !values and kinds = List.rev !kinds in
  let shard_alignment =
    match List.assoc_opt "general.alignment" values with
    | None -> 32
    | Some (Int a) when 1 <= a && a <= alignment && a land (a - 1) = 0 -> a
    | Some _ -> fail "invalid GGUF alignment"
  in
  let offset = ref 0 in
  let tensors =
    List.init nt (fun _ ->
        let tname = read_string ic in
        let rank = read_u32 ic in
        if rank < 1 || rank > 4 then fail "invalid rank for tensor %s" tname;
        let shape = List.init rank (fun _ -> read_u64 ic) in
        let kind = read_u32 ic in
        let relative = read_u64 ic in
        let block, bytes =
          match tensor_kind kind with
          | Some k when not (List.mem 0 shape) -> k
          | _ -> fail "unsupported tensor %s in a V4.1 MXFP4 model" tname
        in
        if List.hd shape mod block <> 0 || relative <> !offset then
          fail "invalid shape or offset for tensor %s" tname;
        let nbytes = List.fold_left ( * ) 1 shape / block * bytes in
        offset := !offset + align nbytes shard_alignment;
        { tname; shape; kind; offset = relative; nbytes })
  in
  let data_offset = align (pos ic) shard_alignment in
  if data_offset + !offset <> size then
    fail "a shard is %d bytes where its header gives %d" size
      (data_offset + !offset);
  { kvs = List.rev !kvs; values; kinds; tensors; data_offset }

(* ---- planning ---------------------------------------------------------- *)

let enc_u32 n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.unsafe_to_string b

let enc_u64 n =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int n);
  Bytes.unsafe_to_string b

let enc_string s = enc_u64 (String.length s) ^ s

(* The merged header and the length of the merged file. The shards must be
   the whole set in order, carrying their split metadata as the splitter
   wrote it: [split.no] and [split.count] as u16 and [split.tensors.count] as
   i32. Only the first shard carries the model's metadata. *)
let plan headers =
  let n = List.length headers in
  let tensors =
    List.concat
      (List.mapi
         (fun i h ->
           if
             List.assoc_opt "split.no" h.kinds <> Some 2
             || List.assoc_opt "split.count" h.kinds <> Some 2
             || List.assoc_opt "split.tensors.count" h.kinds <> Some 5
             || List.assoc_opt "split.no" h.values <> Some (Int i)
             || List.assoc_opt "split.count" h.values <> Some (Int n)
           then
             fail "shard %d is out of order or not part of a set of %d" (i + 1)
               n;
           if
             i > 0
             && List.exists (fun (k, _) -> not (List.mem k split_keys)) h.kvs
           then
             fail "shard %d carries metadata that belongs in the first" (i + 1);
           h.tensors)
         headers)
  in
  let names = List.map (fun t -> t.tname) tensors in
  if List.length (List.sort_uniq String.compare names) <> List.length names then
    fail "a tensor appears in more than one shard";
  List.iter
    (fun h ->
      if
        List.assoc_opt "split.tensors.count" h.values
        <> Some (Int (List.length tensors))
      then fail "the shards do not hold every tensor of the model")
    headers;
  let first = List.hd headers in
  if
    List.assoc_opt "general.architecture" first.values
    <> Some (Str "deepseek41")
    || List.assoc_opt "general.alignment" first.kinds <> Some 4
  then
    fail "the shards are not a DeepSeek V4.1 model with an explicit alignment";
  let kvs =
    List.filter_map
      (fun (key, raw) ->
        if List.mem key split_keys then None
        else if String.starts_with ~prefix:"split." key then
          fail "unexpected split metadata %s" key
        else if key = "general.alignment" then
          Some (enc_string key ^ enc_u32 4 ^ enc_u32 alignment)
        else Some raw)
      first.kvs
  in
  let b = Buffer.create 4096 in
  Buffer.add_string b "GGUF";
  Buffer.add_string b (enc_u32 3);
  Buffer.add_string b (enc_u64 (List.length tensors));
  Buffer.add_string b (enc_u64 (List.length kvs));
  List.iter (fun kv -> Buffer.add_string b kv) kvs;
  let offset = ref 0 in
  List.iter
    (fun t ->
      Buffer.add_string b (enc_string t.tname);
      Buffer.add_string b (enc_u32 (List.length t.shape));
      List.iter (fun d -> Buffer.add_string b (enc_u64 d)) t.shape;
      Buffer.add_string b (enc_u32 t.kind);
      Buffer.add_string b (enc_u64 !offset);
      offset := !offset + align t.nbytes alignment)
    tensors;
  let header_len = align (Buffer.length b) alignment in
  Buffer.add_string b (String.make (header_len - Buffer.length b) '\000');
  (Buffer.contents b, header_len + !offset)

(* ---- writing ----------------------------------------------------------- *)

let file_size path =
  Int64.to_int (Unix.LargeFile.stat path).Unix.LargeFile.st_size

let verify ~path ~expected_bytes ~sha256 =
  let size = file_size path in
  if size <> expected_bytes then
    fail "%s is %d bytes where %d were expected" path size expected_bytes;
  let buf = Bytes.create chunk in
  let digest =
    In_channel.with_open_bin path (fun ic ->
        let rec go ctx =
          match In_channel.input ic buf 0 chunk with
          | 0 -> Digestif.SHA256.get ctx
          | len -> go (Digestif.SHA256.feed_bytes ctx ~len buf)
        in
        go Digestif.SHA256.empty)
  in
  let got = Digestif.SHA256.to_hex digest in
  if got <> String.lowercase_ascii sha256 then
    fail "%s has SHA-256 %s where %s was expected. It was not accepted." path
      got sha256

let rec really_read_fd fd buf off len =
  if len > 0 then
    match Unix.read fd buf off len with
    | 0 -> fail "the partial assembly ended early"
    | n -> really_read_fd fd buf (off + n) (len - n)

let rec really_write_fd fd s off len =
  if len > 0 then
    let n = Unix.write_substring fd s off len in
    really_write_fd fd s (off + n) (len - n)

(* Hold an exclusive lock on [path ^ ".lock"] around [f], so that two
   downloads of the same model cannot both assemble it. *)
let with_lock path f =
  let fd = Unix.openfile (path ^ ".lock") [ O_WRONLY; O_CREAT ] 0o644 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd F_LOCK 0;
      f ())

let merge ?(progress = fun ~written:_ ~total:_ -> ()) ~out ~expected_bytes
    ~sha256 shards =
  let pending = out ^ ".assembling" in
  with_lock pending @@ fun () ->
  if Sys.file_exists out then verify ~path:out ~expected_bytes ~sha256
  else
    let headers =
      List.map (fun p -> In_channel.with_open_bin p read_header) shards
    in
    let header, total = plan headers in
    if total <> expected_bytes then
      fail "the merged layout is %d bytes where the original is %d" total
        expected_bytes;
    let completed = if Sys.file_exists pending then file_size pending else 0 in
    if completed > total then
      fail "%s is larger than the model. Move it aside before trying again."
        pending;
    let fd = Unix.openfile pending [ O_RDWR; O_CREAT ] 0o644 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        let written = ref 0 in
        let cmp = Bytes.create chunk in
        (* An existing prefix is read back and compared rather than
           overwritten, so an interrupted assembly of these shards continues
           and anything else is refused. *)
        let append s =
          let len = String.length s in
          let overlap = min len (max 0 (completed - !written)) in
          if overlap > 0 then (
            really_read_fd fd cmp 0 overlap;
            if Bytes.sub_string cmp 0 overlap <> String.sub s 0 overlap then
              fail
                "%s was not assembled from these shards. Move it aside before \
                 trying again."
                pending);
          really_write_fd fd s overlap (len - overlap);
          written := !written + len;
          progress ~written:!written ~total
        in
        append header;
        List.iter2
          (fun path h ->
            In_channel.with_open_bin path (fun ic ->
                List.iter
                  (fun t ->
                    seek ic (h.data_offset + t.offset);
                    let left = ref t.nbytes in
                    while !left > 0 do
                      let n = min chunk !left in
                      append (really_read ic n);
                      left := !left - n
                    done;
                    append
                      (String.make (align t.nbytes alignment - t.nbytes) '\000'))
                  h.tensors))
          shards headers;
        Unix.fsync fd);
    verify ~path:pending ~expected_bytes ~sha256;
    (* Linking rather than renaming never replaces an output another process
       published first. A filesystem without hard links, such as exFAT on an
       external drive, gets the rename, which the lock still guards. *)
    match Unix.link pending out with
    | () -> Unix.unlink pending
    | exception Unix.Unix_error ((EPERM | EOPNOTSUPP | ENOSYS), _, _) ->
        Unix.rename pending out
