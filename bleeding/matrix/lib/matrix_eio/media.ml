module Mxc = Matrix_client.Media.Mxc

type encrypted_file = Matrix_client.Media.encrypted_file

type source = Matrix_client.Media.source =
  | Plain of Mxc.t
  | Encrypted of encrypted_file

type format = Matrix_client.Media.format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }

type request = Matrix_client.Media.request = {
  source : source;
  format : format;
}

type encrypted_error = Matrix_client.Media.encrypted_error =
  | Media_error of Matrix_client.Error.t
  | Attachment_error of Matrix_client.Encrypted_attachment.error

let get_content ?use_cache ?store client request =
  Error.with_context "getting Matrix media content" (fun () ->
      Matrix_client.Media_fetcher.get_content ?use_cache ?store
        ~fetcher:(Client.media_fetcher client)
        (Client.base client) request)

let upload client ~content_type ~data ?filename () =
  Error.unwrap ~context:"uploading Matrix media"
    (Matrix_client.Media.upload (Client.base client) ~content_type ~data
       ?filename ())

type preallocated = Matrix_client.Media.preallocated = {
  uri : Mxc.t;
  unused_expires_at : Matrix_proto.Event.Timestamp.t option;
}

let create_content_uri client =
  Error.unwrap ~context:"reserving a Matrix media URI"
    (Matrix_client.Media.create_content_uri (Client.base client))

type preallocated_upload_error = Matrix_client.Media.preallocated_upload_error =
  | Preallocated_expired
  | Cannot_overwrite
  | Upload_error of Matrix_client.Error.t

let pp_preallocated_upload_error =
  Matrix_client.Media.pp_preallocated_upload_error

let upload_preallocated ?now client media ~content_type ~data ?filename () =
  Error.with_context "uploading reserved Matrix media" (fun () ->
      Matrix_client.Media.upload_preallocated ?now (Client.base client) media
        ~content_type ~data ?filename ())

type content = Matrix_client.Media.content = {
  body : string;
  content_type : string option;
}

let download client ~server_name ~media_id =
  Error.unwrap ~context:"downloading Matrix media"
    (Matrix_client.Media.download (Client.base client) ~server_name ~media_id)

let thumbnail client ~server_name ~media_id ~width ~height ?resize () =
  Error.unwrap ~context:"downloading a Matrix media thumbnail"
    (Matrix_client.Media.thumbnail (Client.base client) ~server_name ~media_id
       ~width ~height ?resize ())

let mxc_to_http client ~mxc ?width ?height () =
  Matrix_client.Media.mxc_to_http (Client.base client) ~mxc ?width ?height ()

let mxc_to_http_unauthenticated client ~mxc ?width ?height () =
  Matrix_client.Media.mxc_to_http_unauthenticated (Client.base client) ~mxc
    ?width ?height ()

let mxc_to_http_resolved client ~mxc ?width ?height () =
  Error.unwrap ~context:"resolving a Matrix media URL"
    (Matrix_client.Media.mxc_to_http_resolved (Client.base client) ~mxc ?width
       ?height ())

type preview = Matrix_client.Media.preview

let get_url_preview client ~url ?ts () =
  Error.unwrap ~context:"fetching a Matrix URL preview"
    (Matrix_client.Media.get_url_preview (Client.base client) ~url ?ts ())

type config = Matrix_client.Media.config = {
  upload_size : int option;
  custom : (string * Jsont.json) list;
}

let get_config client =
  Error.unwrap ~context:"fetching Matrix media configuration"
    (Matrix_client.Media.get_config (Client.base client))
