(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Eio mock test for agent tools.

   Tools do real I/O through Eio capabilities, so they can be tested against
   mock resources instead of the filesystem. Here a fake [read] tool is backed
   by an {!Eio_mock.Flow} with scripted (mocked) responses, and a fake [write]
   tool by an {!Eio.Flow.buffer_sink} that captures what it is given. Driving
   them through {!Ds4.Tool.invoke} with JSON arguments exercises the codec
   decode + dispatch path with deterministic I/O, under the mock backend. *)

module Tool = Ds4.Tool

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

(* A fake [read] tool: decodes {"path"} and returns the bytes of [src]. *)
let read_tool src =
  let codec =
    let open Dsml.Codec in
    Invoke.map "read" (fun path -> path)
    |> Invoke.param ~enc:Fun.id "path" string ~description:"file to read"
    |> Invoke.seal
  in
  Tool.v ~description:"mock read" codec (fun _path ->
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow src ~max_size:1_000_000))

(* A fake [write] tool: decodes {"path","content"} and writes to [sink]. *)
let write_tool sink =
  let codec =
    let open Dsml.Codec in
    Invoke.map "write" (fun path content -> (path, content))
    |> Invoke.param ~enc:fst "path" string ~description:"destination"
    |> Invoke.param ~enc:snd "content" string ~description:"bytes to write"
    |> Invoke.seal
  in
  Tool.v ~description:"mock write" codec (fun (path, content) ->
      Eio.Flow.copy_string content sink;
      Printf.sprintf "wrote %d bytes to %s" (String.length content) path)

let call name arguments = Dsml.tool_call ~name ~arguments ()

let () =
  Eio_mock.Backend.run @@ fun () ->
  (* read: the flow returns scripted, mocked content. *)
  let file = Eio_mock.Flow.make "file" in
  Eio_mock.Flow.on_read file [ `Return "hello from mock\n"; `Raise End_of_file ];
  let read_result =
    Tool.invoke (read_tool file) (call "read" {|{"path":"notes.txt"}|})
  in
  check "read returns the mocked content" (read_result = "hello from mock\n");

  (* write: the sink captures the bytes into a buffer we can assert on. *)
  let buf = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buf in
  let wt = write_tool sink in
  let write_result =
    Tool.invoke wt (call "write" {|{"path":"out.txt","content":"payload"}|})
  in
  check "write reports the byte count"
    (write_result = "wrote 7 bytes to out.txt");
  check "write reaches the sink" (Buffer.contents buf = "payload");

  (* a missing required argument is decoded into an "Error: …" result, not an
     exception that escapes the turn. *)
  let err = Tool.invoke wt (call "write" {|{"path":"out.txt"}|}) in
  check "missing argument yields an Error result"
    (String.length err >= 6 && String.sub err 0 6 = "Error:");

  check "tool printer includes the name"
    (String.starts_with ~prefix:{|{ name = "write"|}
       (Format.asprintf "%a" Tool.pp wt));
  check "result printer includes image count"
    (String.ends_with ~suffix:"images = 0 }"
       (Format.asprintf "%a" Tool.pp_result (Tool.text "ok")));

  let cancelled = Eio.Cancel.Cancelled Exit in
  let cancel_codec =
    let open Dsml.Codec in
    Invoke.map "cancel" () |> Invoke.seal
  in
  let cancel_tool =
    Tool.v ~description:"cancel" cancel_codec (fun () -> raise cancelled)
  in
  check "tool invocation preserves Eio cancellation"
    (match Tool.invoke cancel_tool (call "cancel" "{}") with
    | exception Eio.Cancel.Cancelled Exit -> true
    | _ -> false);

  if !failures = 0 then Printf.printf "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d test(s) failed.\n" !failures;
    exit 1
  end
