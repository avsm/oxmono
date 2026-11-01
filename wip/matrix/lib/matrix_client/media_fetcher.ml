open Result.Syntax
module Client = Client
module Media = Media
module Store = Media_store
module Attachment = Encrypted_attachment

type t = {
  fetch : Client.t -> Media.request -> (string, Media.encrypted_error) result;
}

let create fetch = { fetch }

let default_fetch client (request : Media.request) =
  match (request.source, request.format) with
  | Media.Plain uri, Media.File ->
      Result.map_error (fun error -> Media.Media_error error)
      @@
      let* content =
        Media.download client
          ~server_name:(Media.Mxc.server_name uri)
          ~media_id:(Media.Mxc.media_id uri)
      in
      Ok content.body
  | Media.Plain uri, Media.Thumbnail { width; height; resize } ->
      Result.map_error (fun error -> Media.Media_error error)
      @@
      let* content =
        Media.thumbnail client
          ~server_name:(Media.Mxc.server_name uri)
          ~media_id:(Media.Mxc.media_id uri) ~width ~height ?resize ()
      in
      Ok content.body
  | Media.Encrypted file, _ -> Media.download_encrypted client file

let default = { fetch = default_fetch }
let now () = Ptime_clock.now ()

let source_uri = function
  | Media.Plain uri -> Ok uri
  | Media.Encrypted file -> (
      match Media.Mxc.of_string file.url with
      | Ok uri -> Ok uri
      | Error (`Msg message) ->
          Error
            (Media.Attachment_error
               (Attachment.Malformed_metadata ("encrypted file url: " ^ message)))
      )

let format_key = function
  | Media.File -> Store.File
  | Media.Thumbnail { width; height; resize } ->
      Store.Thumbnail { width; height; resize }

let encrypted_identity file =
  match Attachment.Metadata.of_event_file file with
  | Error error -> Error (Media.Attachment_error error)
  | Ok metadata ->
      Ok (file.url ^ "\000" ^ Attachment.Metadata.to_json_string metadata)

let cache_key (request : Media.request) uri =
  match request.source with
  | Media.Plain _ -> Ok Store.{ uri; format = format_key request.format }
  | Media.Encrypted file ->
      let* identity = encrypted_identity file in
      Ok
        (Store.derived_key ~namespace:"encrypted-plaintext" ~identity
           (format_key request.format))

let local_cache_key (request : Media.request) uri =
  Store.{ uri; format = format_key request.format }

let invalid_format = function
  | Media.File -> None
  | Media.Thumbnail { width; height; _ } when width >= 0 && height >= 0 -> None
  | Media.Thumbnail _ ->
      Some
        (Error.Policy_denied "media thumbnail dimensions must be non-negative")

let get_content ?(use_cache = true) ?store ?fetcher client
    (request : Media.request) =
  let* () =
    match invalid_format request.format with
    | None -> Ok ()
    | Some error -> Error (Media.Media_error error)
  in
  let* uri = source_uri request.source in
  let local = Store.is_local_uri uri in
  let* key =
    if local then Ok (local_cache_key request uri) else cache_key request uri
  in
  let get_cached key =
    match store with
    | None -> Ok None
    | Some store ->
        Result.map_error
          (fun error -> Media.Media_error error)
          (Store.get ~now:(now ()) store key)
  in
  let add_cached key data =
    match store with
    | None -> Ok ()
    | Some store ->
        Result.map_error
          (fun error -> Media.Media_error error)
          (Store.add store key ~data)
  in
  let decrypt_local source data =
    match source with
    | Media.Plain _ -> Ok data
    | Media.Encrypted file -> (
        match Attachment.Metadata.of_event_file file with
        | Error error -> Error (Media.Attachment_error error)
        | Ok metadata ->
            Result.map_error
              (fun error -> Media.Attachment_error error)
              (Attachment.decrypt_verified metadata data))
  in
  let local_missing () =
    Error
      (Media.Media_error
         (Error.Policy_denied "local media content is missing from the store"))
  in
  match (local, use_cache) with
  | true, _ -> (
      match get_cached key with
      | Error _ as error -> error
      | Ok None -> local_missing ()
      | Ok (Some bytes) -> decrypt_local request.source bytes)
  | false, true -> (
      match get_cached key with
      | Error _ as error -> error
      | Ok (Some bytes) -> Ok bytes
      | Ok None -> (
          let try_queue_ciphertext () =
            match request.source with
            | Media.Plain _ -> Ok None
            | Media.Encrypted _ -> (
                let queue_key = Store.{ uri; format = File } in
                match get_cached queue_key with
                | Ok (Some ciphertext) -> (
                    match decrypt_local request.source ciphertext with
                    | Ok plaintext ->
                        let+ () = add_cached key plaintext in
                        Some plaintext
                    | Error _ -> Ok None)
                | Ok None -> Ok None
                | Error _ as error -> error)
          in
          let* queue_result = try_queue_ciphertext () in
          match queue_result with
          | Some plaintext -> Ok plaintext
          | None ->
              let fetcher = Option.value fetcher ~default in
              let* bytes = fetcher.fetch client request in
              let+ () = add_cached key bytes in
              bytes))
  | false, false ->
      let fetcher = Option.value fetcher ~default in
      fetcher.fetch client request
