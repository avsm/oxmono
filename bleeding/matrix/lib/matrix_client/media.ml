open Result.Syntax
module String_map = Map.MakePortable (String)

module Mxc = struct
  type t = { server_name : Matrix_proto.Id.Server_name.t; media_id : string }

  let scheme = "mxc://"

  let valid_media_id_char = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' -> true
    | _ -> false

  let of_string s =
    if not (String.starts_with ~prefix:scheme s) then
      Error (`Msg "not an mxc:// URI")
    else
      let n = String.length scheme in
      let rest = String.sub s n (String.length s - n) in
      match String.index_opt rest '/' with
      | None -> Error (`Msg "no media id")
      | Some i -> (
          let server = String.sub rest 0 i in
          let media_id = String.sub rest (i + 1) (String.length rest - i - 1) in
          if media_id = "" then Error (`Msg "empty media id")
          else if not (String.for_all valid_media_id_char media_id) then
            Error (`Msg "malformed media id")
          else
            match Matrix_proto.Id.Server_name.of_string server with
            | Error _ as e -> e
            | Ok server_name -> Ok { server_name; media_id })

  let to_string t =
    scheme
    ^ Matrix_proto.Id.Server_name.to_string t.server_name
    ^ "/" ^ t.media_id

  let server_name t = t.server_name
  let media_id t = t.media_id
  let equal a b = String.equal (to_string a) (to_string b)
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"mxc" ~enc:to_string (fun s ->
        match of_string s with Ok t -> Ok t | Error (`Msg m) -> Error m)
end

(* A server that never had an avatar for a user omits the member, but one
   whose avatar was cleared sends [""], which is not an mxc URI. *)
let mxc_or_empty_jsont =
  Jsont.of_of_string ~kind:"mxc"
    ~enc:(function Some m -> Mxc.to_string m | None -> "")
    (fun s ->
      if s = "" then Ok None
      else
        match Mxc.of_string s with
        | Ok t -> Ok (Some t)
        | Error (`Msg m) -> Error m)

let mxc_option_jsont =
  Jsont.map ~kind:"mxc"
    ~dec:(function None -> None | Some v -> v)
    ~enc:(fun v -> match v with None -> None | Some _ -> Some v)
    (Jsont.option mxc_or_empty_jsont)

(* Uploads still go to the unauthenticated media base: the specification has
   not moved [POST /upload] under /_matrix/client. *)
let upload_base = "/_matrix/media/v3"

let preallocated_upload_route =
  Route.v "/_matrix/media/v3/upload/{server_name}/{media_id}"

(* Downloads, thumbnails and the config endpoint moved to the authenticated
   client base in Matrix 1.11 (MSC3916). *)
let media_base = "/_matrix/client/v1/media"
let legacy_media_base = "/_matrix/media/v3"

type upload_response = { content_uri : Mxc.t }

let upload_response_jsont =
  Jsont.Object.(
    map (fun content_uri -> { content_uri })
    |> mem "content_uri" Mxc.jsont ~enc:(fun t -> t.content_uri)
    |> finish)

let upload client ~content_type ~data ?filename () =
  let query =
    match filename with Some f -> [ ("filename", f) ] | None -> []
  in
  let* body =
    Client.Http.post_bytes client ~path:(upload_base ^ "/upload") ~query
      ~content_type ~body:data ()
  in
  let+ resp = Client.Http.decode_response upload_response_jsont body in
  resp.content_uri

let upload_stream client ~content_type ~source ?length ?filename () =
  let query = Option.map (fun f -> [ ("filename", f) ]) filename in
  let* body =
    Client.Http.post_stream client ~path:(upload_base ^ "/upload") ~content_type
      ?length ~body:source ?query ()
  in
  let+ resp = Client.Http.decode_response upload_response_jsont body in
  resp.content_uri

type preallocated = {
  uri : Mxc.t;
  unused_expires_at : Matrix_proto.Event.Timestamp.t option;
}

let preallocated_jsont =
  Jsont.Object.(
    map (fun uri unused_expires_at -> { uri; unused_expires_at })
    |> mem "content_uri" Mxc.jsont ~enc:(fun t -> t.uri)
    |> opt_mem "unused_expires_at" Matrix_proto.Event.Timestamp.jsont
         ~enc:(fun t -> t.unused_expires_at)
    |> finish)

let create_content_uri client =
  let* body =
    Client.Http.post_empty client ~path:"/_matrix/media/v1/create" ()
  in
  Client.Http.decode_response preallocated_jsont body

type preallocated_upload_error =
  | Preallocated_expired
  | Cannot_overwrite
  | Upload_error of Error.t

let pp_preallocated_upload_error ppf = function
  | Preallocated_expired ->
      Format.pp_print_string ppf "the preallocated media URI has expired"
  | Cannot_overwrite ->
      Format.pp_print_string ppf "the preallocated media already has content"
  | Upload_error e -> Error.pp ppf e

let string_contains ~needle haystack =
  let needle_len = String.length needle
  and haystack_len = String.length haystack in
  let rec loop at =
    at + needle_len <= haystack_len
    && (String.equal (String.sub haystack at needle_len) needle || loop (at + 1))
  in
  needle_len = 0 || loop 0

let preallocated_path media =
  Route.expand_exn preallocated_upload_route
    [
      ( "server_name",
        Matrix_proto.Id.Server_name.to_string (Mxc.server_name media.uri) );
      ("media_id", Mxc.media_id media.uri);
    ]

let upload_preallocated ?now client media ~content_type ~data ?filename () =
  let now_ms =
    match now with
    | Some now -> Matrix_proto.Event.Timestamp.to_ms now
    | None ->
        Matrix_proto.Event.Timestamp.(Ptime_clock.now () |> of_ptime |> to_ms)
  in
  match media.unused_expires_at with
  | Some expires when now_ms >= Matrix_proto.Event.Timestamp.to_ms expires ->
      Error Preallocated_expired
  | _ -> (
      let query = Option.map (fun value -> [ ("filename", value) ]) filename in
      match
        Client.Http.put_bytes client ~path:(preallocated_path media) ?query
          ~content_type ~body:data ()
      with
      | Ok _ -> Ok ()
      | Error
          (Error.Matrix_error { errcode = Error.M_CANNOT_OVERWRITE_MEDIA; _ })
        ->
          Error Cannot_overwrite
      (* Synapse versions predating the specified M_NOT_FOUND response used
         M_UNKNOWN with an "expired" message. Match the compatibility case in
         matrix-rust-sdk without treating every ambiguous 404 as expiry. *)
      | Error (Error.Matrix_error { errcode = Error.M_UNKNOWN; error; _ })
        when string_contains ~needle:"expired" (String.lowercase_ascii error) ->
          Error Preallocated_expired
      | Error e -> Error (Upload_error e))

type encrypted_file = Matrix_proto.Event.Media_message_content.encrypted_file
type source = Plain of Mxc.t | Encrypted of encrypted_file

type format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }

type request = { source : source; format : format }

type encrypted_error =
  | Media_error of Error.t
  | Attachment_error of Encrypted_attachment.error

let pp_encrypted_error ppf = function
  | Media_error e -> Format.fprintf ppf "media request: %a" Error.pp e
  | Attachment_error e ->
      Format.fprintf ppf "encrypted attachment: %a"
        Encrypted_attachment.pp_error e

let upload_encrypted client ~data ?filename () =
  let encrypted =
    Encrypted_attachment.encrypt ~random:(Client.random client) data
  in
  match
    upload client ~content_type:"application/octet-stream"
      ~data:encrypted.ciphertext ?filename ()
  with
  | Error e -> Error (Media_error e)
  | Ok mxc ->
      let file =
        Encrypted_attachment.Metadata.to_event_file ~url:(Mxc.to_string mxc)
          encrypted.metadata
      in
      Ok (mxc, file)

type encrypting_source = {
  source : Eio.Flow.source_ty Eio.Resource.t;
  encryptor : Encrypted_attachment.Encryptor.t;
  length : int64 option;
  mutable seen : int64;
}

module Encrypting_source = struct
  type t = encrypting_source

  let read_methods = []

  let single_read t (buf @ local) =
    let buf =
      match t.length with
      | None -> buf
      | Some length ->
          let remaining = Int64.sub length t.seen in
          if remaining <= 0L then raise End_of_file
          else if remaining < Int64.of_int (Cstruct.length buf) then
            Cstruct.sub_local buf 0 (Int64.to_int remaining)
          else buf
    in
    let n =
      Io_context.with_context "reading plaintext for encrypted media upload"
        (fun () -> Eio.Flow.single_read t.source buf)
    in
    let plaintext = Cstruct.to_string ~off:0 ~len:n buf in
    let ciphertext =
      Encrypted_attachment.Encryptor.feed t.encryptor plaintext
    in
    Cstruct.blit_from_string ciphertext 0 buf 0 n;
    t.seen <- Int64.add t.seen (Int64.of_int n);
    n
end

let upload_encrypted_stream client ~source ?length ?filename () =
  let encryptor =
    Encrypted_attachment.Encryptor.create ~random:(Client.random client) ()
  in
  let state = { source; encryptor; length; seen = 0L } in
  let encrypted_source =
    Eio.Resource.T (state, Eio.Flow.Pi.source (module Encrypting_source))
  in
  let query = Option.map (fun f -> [ ("filename", f) ]) filename in
  match
    Client.Http.post_stream client ~path:(upload_base ^ "/upload")
      ~content_type:"application/octet-stream" ?length ~body:encrypted_source
      ?query ()
  with
  | Error e -> Error (Media_error e)
  | Ok body -> (
      match Client.Http.decode_response upload_response_jsont body with
      | Error e -> Error (Media_error e)
      | Ok response ->
          let metadata = Encrypted_attachment.Encryptor.finish encryptor in
          let mxc = response.content_uri in
          let file =
            Encrypted_attachment.Metadata.to_event_file ~url:(Mxc.to_string mxc)
              metadata
          in
          Ok (mxc, file))

type content = { body : string; content_type : string option }
type media_route = Authenticated | Legacy
type media_operation = Download | Thumbnail

let authenticated_download_route =
  Route.v "/_matrix/client/v1/media/download/{server_name}/{media_id}"

let authenticated_thumbnail_route =
  Route.v "/_matrix/client/v1/media/thumbnail/{server_name}/{media_id}"

let legacy_download_route =
  Route.v "/_matrix/media/v3/download/{server_name}/{media_id}"

let legacy_thumbnail_route =
  Route.v "/_matrix/media/v3/thumbnail/{server_name}/{media_id}"

let media_path route operation ~server_name ~media_id =
  let template =
    match (route, operation) with
    | Authenticated, Download -> authenticated_download_route
    | Authenticated, Thumbnail -> authenticated_thumbnail_route
    | Legacy, Download -> legacy_download_route
    | Legacy, Thumbnail -> legacy_thumbnail_route
  in
  Route.expand_exn template
    [
      ("server_name", Matrix_proto.Id.Server_name.to_string server_name);
      ("media_id", media_id);
    ]

let media_route client =
  let+ versions = Server.get_versions client in
  if
    Server.supports_version_at_least versions ~major:1 ~minor:11
    || Server.has_unstable_feature versions "org.matrix.msc3916.stable"
  then Authenticated
  else Legacy

let media_base_of_route = function
  | Authenticated -> media_base
  | Legacy -> legacy_media_base

let get_bytes_route client route ~path ?query () =
  match route with
  | Authenticated -> Client.Http.get_bytes client ~path ?query ()
  | Legacy -> Client.Http.get_bytes_unauthenticated client ~path ?query ()

let get_stream_route client route ~path ?query ~on_response () =
  match route with
  | Authenticated -> Client.Http.get_stream client ~path ?query ~on_response ()
  | Legacy ->
      Client.Http.get_stream_unauthenticated client ~path ?query ~on_response ()

let download client ~server_name ~media_id =
  let* route = media_route client in
  let+ body, content_type =
    get_bytes_route client route
      ~path:(media_path route Download ~server_name ~media_id)
      ()
  in
  { body; content_type }

let download_encrypted client (file : encrypted_file) =
  match Encrypted_attachment.Metadata.of_event_file file with
  | Error e -> Error (Attachment_error e)
  | Ok metadata -> (
      match Mxc.of_string file.url with
      | Error (`Msg msg) ->
          Error
            (Attachment_error
               (Encrypted_attachment.Malformed_metadata
                  ("encrypted file url: " ^ msg)))
      | Ok mxc -> (
          match
            download client ~server_name:(Mxc.server_name mxc)
              ~media_id:(Mxc.media_id mxc)
          with
          | Error e -> Error (Media_error e)
          | Ok content ->
              Encrypted_attachment.decrypt metadata content.body
              |> Result.map_error (fun e -> Attachment_error e)))

let download_encrypted_stream client (file : encrypted_file) ~spool ~output =
  (* Do all metadata and URL checks before touching the network. *)
  match Encrypted_attachment.Metadata.of_event_file file with
  | Error e -> Error (Attachment_error e)
  | Ok metadata -> (
      match Mxc.of_string file.url with
      | Error (`Msg msg) ->
          Error
            (Attachment_error
               (Encrypted_attachment.Malformed_metadata
                  ("encrypted file url: " ^ msg)))
      | Ok mxc ->
          let network_error exn =
            (* The caught [Eio.Io] may carry Fetch's full request URL, including
               a query. Keep the typed cause but do not render that context. *)
            let msg =
              match exn with
              | Eio.Io (error, _) ->
                  Fmt.str "downloading encrypted media: %a" Eio.Exn.pp
                    (Eio.Exn.create error)
              | _ -> "downloading encrypted media failed"
            in
            Media_error (Error.Network_error msg)
          in
          let rewind () =
            Io_context.with_context "rewinding encrypted-media spool" (fun () ->
                ignore (Eio.File.seek spool Optint.Int63.zero `Set))
          in
          let reset () =
            try rewind ()
            with Eio.Io _ as exn ->
              let contextual =
                Eio.Exn.add_context exn
                  "rewinding encrypted-media spool after failure"
              in
              Logs.debug (fun m ->
                  m "could not rewind encrypted-media spool during cleanup: %a"
                    Eio.Exn.pp contextual)
          in
          let protect f =
            try f ()
            with Eio.Io _ as exn ->
              reset ();
              Error (network_error exn)
          in
          protect @@ fun () ->
          Io_context.with_context "truncating encrypted-media spool" (fun () ->
              Eio.File.truncate spool Optint.Int63.zero);
          rewind ();
          let first_digest_error = ref None in
          let first_pass source =
            match Encrypted_attachment.Decryptor.create metadata with
            | Error e -> first_digest_error := Some e
            | Ok decryptor ->
                let offset = ref Optint.Int63.zero in
                let buf = Cstruct.create (64 * 1024) in
                let rec loop () =
                  match
                    try
                      Some
                        (Io_context.with_context
                           "reading encrypted media response" (fun () ->
                             Eio.Flow.single_read source buf))
                    with End_of_file -> None
                  with
                  | None -> (
                      first_digest_error :=
                        match
                          Encrypted_attachment.Decryptor.finish decryptor
                        with
                        | Ok () -> None
                        | Error e -> Some e)
                  | Some n ->
                      let chunk = Cstruct.to_string ~off:0 ~len:n buf in
                      Io_context.with_context "writing encrypted-media spool"
                        (fun () ->
                          Eio.File.pwrite_all spool ~file_offset:!offset
                            [ Cstruct.sub buf 0 n ]);
                      offset := Optint.Int63.add !offset (Optint.Int63.of_int n);
                      ignore
                        (Encrypted_attachment.Decryptor.feed decryptor chunk);
                      loop ()
                in
                loop ()
          in
          let result =
            let route_result = media_route client in
            match route_result with
            | Error e -> Error (Media_error e)
            | Ok route -> (
                match
                  get_stream_route client route
                    ~path:
                      (media_path route Download
                         ~server_name:(Mxc.server_name mxc)
                         ~media_id:(Mxc.media_id mxc))
                    ~on_response:(fun ~content_type:_ source ->
                      first_pass source)
                    ()
                with
                | Error e -> Error (Media_error e)
                | Ok () -> (
                    match !first_digest_error with
                    | Some e -> Error (Attachment_error e)
                    | None ->
                        Encrypted_attachment.decrypt_spooled metadata ~spool
                          ~output
                        |> Result.map_error (fun e -> Attachment_error e)))
          in
          rewind ();
          result)

let thumbnail client ~server_name ~media_id ~width ~height ?resize () =
  let query =
    [ ("width", string_of_int width); ("height", string_of_int height) ]
    @
    match resize with
    | Some `Crop -> [ ("method", "crop") ]
    | Some `Scale -> [ ("method", "scale") ]
    | None -> []
  in
  let* route = media_route client in
  let+ body, content_type =
    get_bytes_route client route
      ~path:(media_path route Thumbnail ~server_name ~media_id)
      ~query ()
  in
  { body; content_type }

let mxc_to_http_with_route client ~route ~mxc ?width ?height () =
  let server_name = Mxc.server_name mxc and media_id = Mxc.media_id mxc in
  let path, query =
    match (width, height) with
    | Some w, Some h ->
        ( media_path route Thumbnail ~server_name ~media_id,
          [ ("width", string_of_int w); ("height", string_of_int h) ] )
    | _ -> (media_path route Download ~server_name ~media_id, [])
  in
  Client.endpoint_uri client ~path ~query ()

let mxc_to_http client ~mxc ?width ?height () =
  mxc_to_http_with_route client ~route:Authenticated ~mxc ?width ?height ()

let mxc_to_http_unauthenticated client ~mxc ?width ?height () =
  mxc_to_http_with_route client ~route:Legacy ~mxc ?width ?height ()

let mxc_to_http_resolved client ~mxc ?width ?height () =
  let+ route = media_route client in
  mxc_to_http_with_route client ~route ~mxc ?width ?height ()

type preview = (string * Jsont.json) list

let preview_jsont =
  Matrix_proto.Json.Codec.string_map Matrix_proto.Json.Codec.json

let get_url_preview client ~url ?ts () =
  let* route = media_route client in
  let query =
    [ ("url", url) ]
    @
    match ts with
    | None -> []
    | Some timestamp ->
        [
          ("ts", Int64.to_string (Matrix_proto.Event.Timestamp.to_ms timestamp));
        ]
  in
  let* body, _content_type =
    get_bytes_route client route
      ~path:(media_base_of_route route ^ "/preview_url")
      ~query ()
  in
  Client.Http.decode_response preview_jsont body

type config = { upload_size : int option; custom : (string * Jsont.json) list }

let config_jsont =
  Jsont.Object.(
    map (fun upload_size custom ->
        { upload_size; custom = String_map.bindings custom })
    |> opt_mem "m.upload.size" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.upload_size)
    |> keep_unknown
         (Matrix_proto.Json.Codec.string_map_mems Matrix_proto.Json.Codec.json)
         ~enc:(fun t -> String_map.of_seq (List.to_seq t.custom))
    |> finish)

let get_config client =
  let* route = media_route client in
  let* body, _content_type =
    get_bytes_route client route
      ~path:(media_base_of_route route ^ "/config")
      ()
  in
  Client.Http.decode_response config_jsont body
