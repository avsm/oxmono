(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let mib = 1024 * 1024
let chunk = String.init mib (fun i -> Char.chr (32 + (i mod 95)))

let generate fs path ~size =
  Eio.Path.with_open_out ~create:(`Or_truncate 0o600) Eio.Path.(fs / path)
  @@ fun file ->
  let rec fill written =
    if written < size then begin
      let n = min mib (size - written) in
      Eio.Flow.copy_string (String.sub chunk 0 n) file;
      fill (written + n)
    end
  in
  fill 0

let size_of fs path =
  Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun file ->
  Optint.Int63.to_int64 (Eio.File.size file)

let max_size_upload client =
  match Proto.Session.core_capability (Client.session client) with
  (* Cyrus answers 0, to which RFC 8620 Section 2 gives no meaning. *)
  | Some { max_size_upload = 0L; _ } | None -> None
  | Some c -> Some c.max_size_upload

let stream (ctx : Cli.context) ~content_type ~source ~target =
  let client = ctx.client and account_id = ctx.account_id in
  let fs = Eio.Stdenv.fs ctx.env in
  let length = size_of fs source in
  let limit = max_size_upload client in
  Fmt.pr "file      %s@." source;
  Fmt.pr "          %Ld bytes, maxSizeUpload %s@." length
    (Option.fold ~none:"unstated" ~some:Int64.to_string limit);
  Option.iter
    (fun l ->
      if length > l then Fmt.failwith "the session accepts %Ld bytes at most" l)
    limit;
  let blob =
    Eio.Path.with_open_in Eio.Path.(fs / source) @@ fun file ->
    Client.upload_flow_exn client ~account_id ~content_type ~length file
  in
  let blob_id = blob.blob_id in
  Fmt.pr "upload    blobId=%a type=%s size=%Ld@." Proto.Id.pp blob_id
    (Cli.terminal_text blob.type_)
    blob.size;
  if blob.size <> length then
    Fmt.failwith "the server stored %Ld of the %Ld bytes sent" blob.size length;
  let served =
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600) Eio.Path.(fs / target)
    @@ fun file ->
    Client.download_to_exn client ~account_id ~blob_id
      ~name:(Filename.basename source) ~accept:content_type file
  in
  let back = size_of fs target in
  Fmt.pr "download  %s@." target;
  Fmt.pr "          %Ld bytes, asked for %s, served as %s@." back
    (Cli.terminal_text content_type)
    (Cli.terminal_text served);
  if back <> length then
    Fmt.failwith "the download is %Ld bytes and the upload was %Ld" back length;
  let sent = Digest.file source and got = Digest.file target in
  if not (String.equal sent got) then
    Fmt.failwith "the streamed round trip changed the bytes";
  Fmt.pr "digest    %s, identical end to end@." (Digest.to_hex sent)

let run (ctx : Cli.context) (file, content_type) =
  let fs = Eio.Stdenv.fs ctx.env in
  let source =
    match file with
    | Some path -> path
    | None ->
        let path = Filename.temp_file "jmap-stream-" ".bin" in
        generate fs path ~size:(4 * mib);
        path
  in
  let target = Filename.temp_file "jmap-stream-back-" ".bin" in
  let remove path = try Sys.remove path with Sys_error _ -> () in
  Fun.protect
    ~finally:(fun () ->
      if Option.is_none file then remove source;
      remove target)
    (fun () -> stream ctx ~content_type ~source ~target)

let args =
  let open Cmdliner in
  let file =
    let doc = "Stream $(docv) instead of a generated file of four megabytes." in
    Arg.(value & opt (some file) None & info [ "file"; "f" ] ~docv:"PATH" ~doc)
  in
  let content_type =
    let doc = "Upload with the media type $(docv) and ask for it back." in
    Arg.(
      value
      & opt string "application/octet-stream"
      & info [ "type"; "t" ] ~docv:"MIME" ~doc)
  in
  Term.(const (fun f t -> (f, t)) $ file $ content_type)

let () =
  Cli.main' "stream" ~args
    ~doc:"Move a file to the server and back without holding it in memory" run
