(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Reading a GGUF's metadata for a draft head, over headers this test writes.

   The negative cases matter most. A model the engine would refuse to open
   with its draft head armed must read as having none, since arming it is the
   default and the refusal comes only after the model is mapped. *)

module V4 = Ds4.V4

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

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
let kv_u32 k v = str k ^ u32 4 ^ u32 v

(* A tokenizer-like array of strings, which the reader has to step over. *)
let kv_strings k vs =
  str k ^ u32 9 ^ u32 8
  ^ u64 (List.length vs)
  ^ String.concat "" (List.map str vs)

let gguf kvs =
  "GGUF" ^ u32 3 ^ u64 0 ^ u64 (List.length kvs) ^ String.concat "" kvs

let run env =
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "ds4-gguf" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
  @@ fun () ->
  let file name body =
    let path = Eio.Path.(fs / Filename.concat dir name) in
    Eio.Path.save ~create:(`Or_truncate 0o644) path body;
    path
  in
  let has name body = V4.has_draft_head (file name body) in
  check "a Qwen model with prediction layers has a draft head"
    (has "qwen.gguf"
       (gguf
          [
            kv_string "general.architecture" "qwen4exp";
            kv_strings "tokenizer.ggml.tokens" [ "a"; "b"; "c" ];
            kv_u32 "qwen4exp.nextn_predict_layers" 1;
          ]));
  check "a GLM 5.3 model with prediction layers has a draft head"
    (has "glm.gguf"
       (gguf
          [
            kv_string "general.architecture" "glm5-next";
            kv_u32 "glm5-next.nextn_predict_layers" 1;
          ]));
  check "a Qwen model without prediction layers has none"
    (not
       (has "qwen0.gguf"
          (gguf
             [
               kv_string "general.architecture" "qwen4exp";
               kv_u32 "qwen4exp.nextn_predict_layers" 0;
             ])));
  check "a model that names no prediction layers has none"
    (not
       (has "glm-none.gguf"
          (gguf [ kv_string "general.architecture" "glm-dsa" ])));
  check "a DeepSeek model has none, whatever its keys say"
    (not
       (has "deepseek.gguf"
          (gguf
             [
               kv_string "general.architecture" "deepseek4";
               kv_u32 "deepseek4.nextn_predict_layers" 1;
             ])));
  check "a file that is not a GGUF has none"
    (not (has "text.gguf" "this is not a model"));
  check "a truncated header has none"
    (not
       (has "short.gguf"
          (String.sub
             (gguf [ kv_string "general.architecture" "qwen4exp" ])
             0 30)));
  check "a file that is not there has none"
    (not (V4.has_draft_head Eio.Path.(fs / Filename.concat dir "absent.gguf")));
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d test(s) failed.\n" !failures;
    exit 1
  end

let () = Eio_main.run run
