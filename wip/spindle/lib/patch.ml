(* SPDX-License-Identifier: ISC *)
open Json

let revision system compressed =
  Eio.Time.with_timeout_exn system#clock 15. @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let read, write = Eio.Process.pipe ~sw system#process_mgr in
  let process =
    Eio.Process.spawn ~sw system#process_mgr
      ~stdin:(Eio.Flow.string_source compressed)
      ~stdout:write ~stderr:Eio.Flow.null [ "gzip"; "-dc" ]
  in
  Eio.Flow.close write;
  let reader = Eio.Buf_read.of_flow ~max_size:1048576 read in
  let rec lines total revision =
    match Eio.Buf_read.line reader with
    | line ->
        let total = total + String.length line + 1 in
        if total > 64 * 1024 * 1024 then invalid "patch exceeds 64 MiB";
        let revision =
          if
            String.length line = 70
            && String.starts_with ~prefix:"From " line
            && String.ends_with ~suffix:" Mon Sep 17 00:00:00 2001" line
          then Some (sha (String.sub line 5 40))
          else revision
        in
        lines total revision
    | exception End_of_file -> revision
  in
  let revision =
    try lines 0 None
    with Eio.Buf_read.Buffer_limit_exceeded ->
      invalid "patch line exceeds 1 MiB"
  in
  (match Eio.Process.await process with
  | `Exited 0 -> ()
  | `Exited _ -> invalid "invalid gzip patch"
  | `Signaled _ -> failwith "patch decoder was interrupted");
  match revision with Some sha -> sha | None -> invalid "patch has no commits"

let pull network system actor record =
  let rounds = list (required "rounds" record) in
  let round =
    match List.rev rounds with
    | round :: _ -> round
    | [] -> invalid "pull request has no rounds"
  in
  let blob = required "patchBlob" round in
  let cid = get "$link" (required "ref" blob) in
  let expected =
    match Atp.Cid.of_string_result cid with
    | Ok cid -> cid
    | Error _ -> invalid "invalid patch CID"
  in
  if get "mimeType" blob <> "application/gzip" then invalid "patch must be gzip";
  let raw =
    Network.read
      ~limit:(16 * 1024 * 1024)
      network
      (Network.query
         (Network.pds network actor ^ "/xrpc/com.atproto.sync.getBlob")
         [ ("did", actor); ("cid", cid) ])
  in
  if not (Atp.Cid.equal expected (Atp.Cid.create `Raw raw)) then
    invalid "patch blob does not match CID";
  revision system raw
