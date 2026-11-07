(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Merging GGUF split files into one page-aligned model, over shards this test
   writes. The merged bytes are compared with a file built independently, so
   the test holds the layout fixed rather than the merge's view of it. *)

module Shards = Ds4_cli.Gguf_shards

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else (
    incr failures;
    Printf.printf "FAIL - %s\n" name)

let fails name f =
  match f () with
  | () ->
      incr failures;
      Printf.printf "FAIL - %s (no error)\n" name
  | exception Failure msg -> Printf.printf "ok   - %s: %s\n" name msg

let u16 n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_le b 0 n;
  Bytes.to_string b

let u32 n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.to_string b

let u64 n =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int n);
  Bytes.to_string b

let str s = u64 (String.length s) ^ s
let kv_string k v = str k ^ u32 8 ^ str v
let kv_u16 k v = str k ^ u32 2 ^ u16 v
let kv_u32 k v = str k ^ u32 4 ^ u32 v
let kv_i32 k v = str k ^ u32 5 ^ u32 v
let kv_u64 k v = str k ^ u32 10 ^ u64 v
let kv_bool k v = str k ^ u32 7 ^ String.make 1 (if v then '\001' else '\000')

let kv_strings k vs =
  str k ^ u32 9 ^ u32 8
  ^ u64 (List.length vs)
  ^ String.concat "" (List.map str vs)

let align n a = (n + a - 1) / a * a

let pad s a =
  s ^ String.make (align (String.length s) a - String.length s) '\000'

(* A tensor: its name, shape, kind and the bytes it holds. *)
type tensor = { name : string; shape : int list; kind : int; data : string }

let bytes n seed = String.init n (fun i -> Char.chr ((i * 7) + (seed land 255)))

let info t offset =
  str t.name
  ^ u32 (List.length t.shape)
  ^ String.concat "" (List.map u64 t.shape)
  ^ u32 t.kind ^ u64 offset

(* A GGUF file with [alignment], holding [kvs] and [tensors] laid out in
   order. *)
let gguf ~alignment kvs tensors =
  let infos, _ =
    List.fold_left
      (fun (acc, off) t ->
        (acc ^ info t off, off + align (String.length t.data) alignment))
      ("", 0) tensors
  in
  let header =
    "GGUF" ^ u32 3
    ^ u64 (List.length tensors)
    ^ u64 (List.length kvs)
    ^ String.concat "" kvs ^ infos
  in
  pad header alignment
  ^ String.concat "" (List.map (fun t -> pad t.data alignment) tensors)

let model_kvs =
  [
    kv_string "general.architecture" "deepseek41";
    kv_u32 "general.alignment" 4;
    kv_strings "tokenizer.ggml.tokens" [ "a"; "bc"; "" ];
    kv_u64 "deepseek41.engram.rows" 1234567;
    kv_bool "general.quantized" true;
  ]

let split no count tensors =
  [
    kv_u16 "split.no" no;
    kv_u16 "split.count" count;
    kv_i32 "split.tensors.count" tensors;
  ]

let t0 = { name = "blk.0.w"; shape = [ 3 ]; kind = 0; data = bytes 12 1 }
let t1 = { name = "blk.1.exp"; shape = [ 32; 2 ]; kind = 39; data = bytes 34 2 }
let t2 = { name = "output"; shape = [ 5 ]; kind = 1; data = bytes 10 3 }
let shard0 = gguf ~alignment:4 (model_kvs @ split 0 2 3) [ t0; t1 ]

(* A later shard carries only the split keys, so its alignment is the GGUF
   default of 32 rather than the 4 the first shard declares. *)
let shard1 = gguf ~alignment:32 (split 1 2 3) [ t2 ]

(* What the merge must produce: the first shard's metadata without the split
   keys and with the alignment rewritten, then every tensor at 16 KiB. *)
let expected =
  let kvs =
    [
      kv_string "general.architecture" "deepseek41";
      kv_u32 "general.alignment" 16384;
      kv_strings "tokenizer.ggml.tokens" [ "a"; "bc"; "" ];
      kv_u64 "deepseek41.engram.rows" 1234567;
      kv_bool "general.quantized" true;
    ]
  in
  gguf ~alignment:16384 kvs [ t0; t1; t2 ]

let sha256 s = Digestif.SHA256.(to_hex (digest_string s))

let () =
  let dir = Filename.temp_file "ds4-shards-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
  @@ fun () ->
  let path name = Filename.concat dir name in
  let write name body =
    Out_channel.with_open_bin (path name) (fun oc -> output_string oc body)
  in
  let read name = In_channel.with_open_bin (path name) In_channel.input_all in
  write "s0.gguf" shard0;
  write "s1.gguf" shard1;
  let out = path "model.gguf" in
  let shards = [ path "s0.gguf"; path "s1.gguf" ] in
  let expected_bytes = String.length expected and sha = sha256 expected in
  let merge ?(shards = shards) ?(sha256 = sha) () =
    Shards.merge ~out ~expected_bytes ~sha256 shards
  in
  check "the merged file has a whole number of pages"
    (expected_bytes = 4 * 16384);
  merge ();
  check "the merge restores the original page-aligned file"
    (read "model.gguf" = expected);
  check "the shards are left in place"
    (read "s0.gguf" = shard0 && read "s1.gguf" = shard1);
  check "nothing is left of the assembly"
    (not (Sys.file_exists (out ^ ".assembling")));
  merge ();
  check "merging again verifies the file that is there"
    (read "model.gguf" = expected);
  Sys.remove out;
  write "model.gguf.assembling" (String.sub expected 0 20000);
  merge ();
  check "an interrupted assembly resumes to the same file"
    (read "model.gguf" = expected);
  Sys.remove out;
  let wrong = Bytes.of_string (String.sub expected 0 20000) in
  Bytes.set wrong 16400 (Char.chr (Char.code (Bytes.get wrong 16400) lxor 1));
  write "model.gguf.assembling" (Bytes.to_string wrong);
  fails "an assembly that differs from the shards is refused" merge;
  check "a refused assembly is kept and nothing is published"
    (Sys.file_exists (out ^ ".assembling") && not (Sys.file_exists out));
  Sys.remove (out ^ ".assembling");
  fails "a digest that does not match is refused" (fun () ->
      merge ~sha256:(String.make 64 '0') ());
  check "a file that fails to verify is not published"
    (not (Sys.file_exists out));
  Sys.remove (out ^ ".assembling");
  fails "shards out of order are refused" (fun () ->
      merge ~shards:(List.rev shards) ());
  write "s1only.gguf" (gguf ~alignment:32 (split 1 2 2) [ t2 ]);
  fails "a set missing a tensor is refused" (fun () ->
      merge ~shards:[ path "s0.gguf"; path "s1only.gguf" ] ());
  write "glm.gguf"
    (gguf ~alignment:4
       (kv_string "general.architecture" "glm-dsa"
       :: kv_u32 "general.alignment" 4
       :: split 0 1 1)
       [ t0 ]);
  fails "another architecture is refused" (fun () ->
      merge ~shards:[ path "glm.gguf" ] ());
  write "short.gguf" (String.sub shard1 0 (String.length shard1 - 1));
  fails "a truncated shard is refused" (fun () ->
      merge ~shards:[ path "s0.gguf"; path "short.gguf" ] ());
  write "model.gguf" expected;
  Shards.verify ~path:out ~expected_bytes ~sha256:sha;
  fails "verify reports a wrong length" (fun () ->
      Shards.verify ~path:out ~expected_bytes:(expected_bytes - 1) ~sha256:sha);
  if !failures > 0 then exit 1 else print_endline "All tests passed."
