(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** PeerTube metadata and thumbnail client. *)

let src = Logs.Src.create "bushel.peertube" ~doc:"PeerTube video sync"
module Log = (val Logs.src_log src : Logs.LOG)

type video = {
  id : int;
  uuid : string;
  name : string;
  description : string option;
  url : string;
  embed_path : string;
  published_at : Ptime.t;
  originally_published_at : Ptime.t option;
  thumbnail_path : string option;
  tags : string list;
}

type fetch_result =
  | Ok of string
  | Skipped of string
  | Error of string

module PT = Peer_tube

let int_of_json (json : Jsont.json) : int =
  match json with
  | Jsont.Number (f, _) -> int_of_float f
  | _ -> 0

let string_of_json (json : Jsont.json) : string =
  match json with
  | Jsont.String (s, _) -> s
  | _ -> ""

let video_of_peertube (pt : PT.Video.T.t) : video =
  let id = match PT.Video.T.id pt with
    | Some id_json -> int_of_json id_json
    | None -> 0
  in
  let uuid = match PT.Video.T.uuid pt with
    | Some uuid_json -> string_of_json uuid_json
    | None -> ""
  in
  {
    id;
    uuid;
    name = Option.value ~default:"" (PT.Video.T.name pt);
    description = PT.Video.T.truncated_description pt;
    url = "";
    embed_path = Option.value ~default:"" (PT.Video.T.embed_path pt);
    published_at = Option.value ~default:Ptime.epoch (PT.Video.T.published_at pt);
    originally_published_at = PT.Video.T.originally_published_at pt;
    thumbnail_path = PT.Video.T.thumbnail_path pt;
    tags = [];
  }

type channel_response = {
  total : int;
  data : video list;
}

let decode_video json_str =
  match Jsont_bytesrw.decode_string PT.Video.T.jsont json_str with
  | Ok pt -> Result.Ok (video_of_peertube pt)
  | Error e -> Result.Error e

let decode_channel_response json_str =
  match Jsont_bytesrw.decode_string PT.VideoList.Response.jsont json_str with
  | Ok r ->
    let total = Option.value ~default:0 (PT.VideoList.Response.total r) in
    let data = match PT.VideoList.Response.data r with
      | Some videos -> List.map video_of_peertube videos
      | None -> []
    in
    Result.Ok { total; data }
  | Error e -> Result.Error e

(** [uuid_of_url url] is the UUID in a [/w/UUID] or [/videos/watch/UUID]
    PeerTube URL. *)
let uuid_of_url url =
  let uri = Uri.of_string url in
  let path = Uri.path uri in
  let segments = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  match segments with
  | ["w"; uuid] -> Some uuid
  | ["videos"; "watch"; uuid] -> Some uuid
  | _ -> None

(** [origin_of_url url] is the scheme and authority of [url], if present. *)
let origin_of_url url =
  let uri = Uri.of_string url in
  match Uri.scheme uri, Uri.host uri with
  | Some scheme, Some host ->
    let port = match Uri.port uri with
      | Some p -> Printf.sprintf ":%d" p
      | None -> ""
    in
    Some (Printf.sprintf "%s://%s%s" scheme host port)
  | _ -> None

let normalize_endpoint endpoint =
  let endpoint = String.lowercase_ascii endpoint in
  let len = String.length endpoint in
  if len > 0 && endpoint.[len - 1] = '/' then String.sub endpoint 0 (len - 1)
  else endpoint

(** [find_server_for_url servers url] is the server whose endpoint matches
    [url]. *)
let find_server_for_url servers url =
  match origin_of_url url with
  | None -> None
  | Some origin ->
    let origin = normalize_endpoint origin in
    List.find_opt (fun (s : Bushel_config.peertube_server) ->
      normalize_endpoint s.endpoint = origin
    ) servers

let fetch_video_details ~http ~endpoint uuid =
  let url = Printf.sprintf "%s/api/v1/videos/%s" endpoint uuid in
  match Bushel_http.get ~http url with
  | Result.Error e -> Result.Error e
  | Result.Ok body ->
    match decode_video body with
    | Result.Ok v -> Result.Ok v
    | Result.Error e ->
      Log.warn (fun m -> m "Failed to decode video %s: %s" uuid e);
      Log.debug (fun m -> m "Response body: %s" (String.sub body 0 (min 500 (String.length body))));
      Result.Error e

let fetch_channel_videos ~http ~endpoint ~channel ?(count=20) ?(start=0) () =
  let url = Printf.sprintf "%s/api/v1/video-channels/%s/videos?count=%d&start=%d"
    endpoint channel count start in
  match Bushel_http.get ~http url with
  | Result.Error _ -> (0, [])
  | Result.Ok body ->
    match decode_channel_response body with
    | Result.Ok r -> (r.total, r.data)
    | Result.Error _ -> (0, [])

let fetch_all_channel_videos ~http ~endpoint ~channel ?(page_size=20) () =
  let rec fetch_pages start acc =
    let (total, videos) = fetch_channel_videos ~http ~endpoint ~channel ~count:page_size ~start () in
    let all = acc @ videos in
    let fetched = start + List.length videos in
    if fetched < total && List.length videos > 0 then
      fetch_pages fetched all
    else
      all
  in
  fetch_pages 0 []

let thumbnail_url endpoint video =
  match video.thumbnail_path with
  | Some path -> Some (endpoint ^ path)
  | None -> None

let download_thumbnail ~http ~endpoint video output_path =
  match thumbnail_url endpoint video with
  | None ->
    Log.warn (fun m -> m "No thumbnail for video %s" video.uuid);
    Result.Error "No thumbnail available"
  | Some url ->
    match Bushel_http.get ~http url with
    | Result.Error e -> Result.Error e
    | Result.Ok body ->
      try
        let dir = Filename.dirname output_path in
        if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
        Out_channel.with_open_bin output_path (fun oc -> output_string oc body);
        Result.Ok output_path
      with e ->
        Result.Error (Printf.sprintf "Failed to write: %s" (Printexc.to_string e))

module VideoIndex = struct
  type t = (string, string) Hashtbl.t

  let empty () = Hashtbl.create 64

  let load_file path =
    let index = empty () in
    if Sys.file_exists path then begin
      try
        In_channel.with_open_bin path (fun ic ->
          let rec read_lines () =
            match input_line ic with
            | line ->
            (match Astring.String.cut ~sep:":" line with
             | Some (uuid, server) ->
               Hashtbl.add index (String.trim uuid) (String.trim server)
             | None -> ());
            read_lines ()
            | exception End_of_file -> ()
          in
          read_lines ())
      with _ -> ()
    end;
    index

  let save_file path index =
    Out_channel.with_open_bin path (fun oc ->
      output_string oc "# UUID -> PeerTube server name mapping\n";
      Hashtbl.iter (fun uuid server ->
        output_string oc (Printf.sprintf "%s: %s\n" uuid server)
      ) index)

  let add index ~uuid ~server =
    Hashtbl.replace index uuid server

  let find index uuid =
    Hashtbl.find_opt index uuid
end

let try_fetch_from_server ~http ~endpoint ~output_path uuid =
  match fetch_video_details ~http ~endpoint uuid with
  | Result.Error _ -> None
  | Result.Ok video ->
    match download_thumbnail ~http ~endpoint video output_path with
    | Result.Ok path -> Some (Ok path)
    | Result.Error _ -> None

let try_all_servers ~http ~servers ~output_path uuid =
  let rec try_next = function
    | [] -> None
    | (server : Bushel_config.peertube_server) :: rest ->
      Log.debug (fun m -> m "Trying server %s for video %s" server.name uuid);
      match try_fetch_from_server ~http ~endpoint:server.endpoint ~output_path uuid with
      | Some result -> Some (server, result)
      | None -> try_next rest
  in
  try_next servers

let fetch_thumbnails ~http ~servers ~output_dir ~videos ~index =
  if not (Sys.file_exists output_dir) then
    Unix.mkdir output_dir 0o755;

  let results = List.filter_map (fun (video : Bushel.Video.t) ->
    let uuid = Bushel.Video.uuid video in
    let url = Bushel.Video.url video in
    let output_path = Filename.concat output_dir (uuid ^ ".jpg") in

    if Sys.file_exists output_path then begin
      Log.debug (fun m -> m "Skipping %s: thumbnail exists" uuid);
      Some (uuid, Skipped output_path)
    end else begin
      let server_from_url =
        if url <> "" then find_server_for_url servers url
        else None
      in

      let server_from_index =
        match VideoIndex.find index uuid with
        | Some server_name ->
          List.find_opt (fun (s : Bushel_config.peertube_server) ->
            s.name = server_name) servers
        | None -> None
      in

      let search_all_servers () =
        Log.info (fun m -> m "Searching all servers for video %s" uuid);
        match try_all_servers ~http ~servers ~output_path uuid with
        | Some (server, result) ->
          Log.info (fun m -> m "Found video %s on server %s" uuid server.name);
          VideoIndex.add index ~uuid ~server:server.name;
          Some (uuid, result)
        | None ->
          Log.warn (fun m -> m "Video %s not found on any server" uuid);
          Some (uuid, Error "Not found on any configured server")
      in

      let preferred =
        match server_from_url with
        | Some server -> Some (server, true, "URL")
        | None -> Option.map (fun server -> (server, false, "index")) server_from_index
      in
      match preferred with
      | None -> search_all_servers ()
      | Some (server, update_index, source) ->
        Log.info (fun m ->
          m "Fetching thumbnail for %s from %s (from %s)" uuid server.name source);
        (match try_fetch_from_server ~http ~endpoint:server.endpoint ~output_path uuid with
         | Some result ->
           if update_index then VideoIndex.add index ~uuid ~server:server.name;
           Some (uuid, result)
         | None ->
           Log.info (fun m -> m "Preferred server failed, trying others...");
           search_all_servers ())
    end
  ) videos in

  let ok_count, skipped_count, error_count =
    List.fold_left
      (fun (ok, skipped, error) (_, result) ->
        match result with
        | Ok _ -> (ok + 1, skipped, error)
        | Skipped _ -> (ok, skipped + 1, error)
        | Error _ -> (ok, skipped, error + 1))
      (0, 0, 0) results
  in

  Log.info (fun m -> m "Video thumbnails: %d ok, %d skipped, %d errors"
    ok_count skipped_count error_count);
  results
