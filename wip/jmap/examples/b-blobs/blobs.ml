(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let () =
  Cli.main "blobs" ~doc:"Upload a blob, download it again, and stream both ways"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let account = Proto.Id.to_string account_id in
  let data = Printf.sprintf "Uploaded by b-blobs, run %d.\n" (Unix.getpid ()) in
  let expansion = function
    | Ok url -> url
    | Error error ->
        Fmt.failwith "URI template: %a" Httpz_uri.Template.pp_error error
  in

  Fmt.pr "upload    %s@." (Client.upload_url client);
  Fmt.pr "          %s@."
    (expansion
       (Proto.Template.expand
          ~vars:[ ("accountId", account) ]
          (Client.upload_url client)));
  let up =
    Client.upload_exn client ~account_id ~content_type:"text/plain" ~data
  in
  let blob_id = up.blob_id in
  Fmt.pr "          blobId=%a type=%s size=%Ld@." Proto.Id.pp blob_id
    (Cli.terminal_text up.type_)
    up.size;

  let name = "a blob.txt" in
  Fmt.pr "download  %s@." (Client.download_url client);
  Fmt.pr "          %s@."
    (expansion
       (Proto.Blob.expand_download_url
          ~template:(Client.download_url client)
          { account_id; blob_id; type_ = "text/plain"; name }));
  let back =
    Client.download_exn client ~account_id ~blob_id ~name ~accept:"text/plain"
      ()
  in
  if not (String.equal back data) then
    Fmt.failwith "the %d bytes downloaded differ from the %d uploaded"
      (String.length back) (String.length data);
  Fmt.pr "          %d bytes, identical to what went up@." (String.length back);

  let length = Int64.of_int (String.length data) in
  let streamed =
    Client.upload_flow_exn client ~account_id ~content_type:"text/plain" ~length
      (Eio.Flow.string_source data)
  in
  let buffer = Buffer.create (String.length data) in
  let served =
    Client.download_to_exn client ~account_id ~blob_id:streamed.blob_id
      ~name:"streamed.txt"
      (Eio.Flow.buffer_sink buffer)
  in
  if not (String.equal (Buffer.contents buffer) data) then
    Fmt.failwith "the streamed round trip changed the bytes";
  Fmt.pr "stream    blobId=%a served as %s, identical@." Proto.Id.pp
    streamed.blob_id (Cli.terminal_text served)
