(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Feed = Sortal_schema.Feed

type t = {
  feed_url : string;
  feed_type : Feed.feed_type;
  last_sync : Ptime.t option;
  etag : string option;
  last_modified : string option;
  entry_count : int;
}

let ptime_jsont =
  let open Jsont in
  map ~kind:"Ptime" ~dec:(fun s ->
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> t
    | Error _ -> failwith ("Invalid RFC3339 date: " ^ s)
  ) ~enc:(fun t ->
    Ptime.to_rfc3339 t
  ) string

let json_t =
  let open Jsont in
  let open Jsont.Object in
  let make feed_type feed_url last_sync etag last_modified entry_count =
    let feed_type = match Feed.feed_type_of_string feed_type with
      | Some ft -> ft
      | None -> failwith ("Invalid feed type: " ^ feed_type)
    in
    { feed_url; feed_type; last_sync; etag; last_modified; entry_count }
  in
  map ~kind:"FeedMeta" make
  |> mem "feed_type" string ~enc:(fun m -> Feed.feed_type_to_string m.feed_type)
  |> mem "feed_url" string ~enc:(fun m -> m.feed_url)
  |> opt_mem "last_sync" ptime_jsont ~enc:(fun m -> m.last_sync)
  |> opt_mem "etag" string ~enc:(fun m -> m.etag)
  |> opt_mem "last_modified" string ~enc:(fun m -> m.last_modified)
  |> mem "entry_count" int ~dec_absent:0 ~enc:(fun m -> m.entry_count)
  |> finish

let save path meta =
  let buf = Buffer.create 1024 in
  let writer = Bytesrw.Bytes.Writer.of_buffer buf in
  match Jsont_bytesrw.encode json_t meta ~eod:true writer with
  | Ok () -> Eio.Path.save ~create:(`Or_truncate 0o644) path (Buffer.contents buf)
  | Error err -> failwith ("Failed to encode feed meta: " ^ err)

let load path =
  try
    let data = Eio.Path.load path in
    let reader = Bytesrw.Bytes.Reader.of_string data in
    match Jsont_bytesrw.decode json_t reader with
    | Ok meta -> Some meta
    | Error msg ->
      Logs.warn (fun m -> m "Failed to decode feed meta: %s" msg);
      None
  with _ -> None
