module M = Matrix_eio

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let content_type_of_filename name =
  match Filename.extension name with
  | ".txt" -> "text/plain"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | _ -> "application/octet-stream"

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  let room_id = Matrix_proto.Id.Room_id.of_string_exn Sys.argv.(1) in
  let path = Sys.argv.(2) in
  let content_type = content_type_of_filename path in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let data = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
  let mxc =
    M.Media.upload client ~content_type ~data ~filename:(Filename.basename path)
      ()
  in
  Printf.printf "Uploaded %s\n%!" (M.Media.Mxc.to_string mxc);
  let send =
    if String.starts_with ~prefix:"image/" content_type then
      M.Messages.send_image
    else M.Messages.send_file
  in
  let event_id =
    send client ~room_id ~body:(Filename.basename path) ~url:mxc ()
  in
  Printf.printf "Sent %s\n%!" (Matrix_proto.Id.Event_id.to_string event_id);
  let content =
    M.Media.download client
      ~server_name:(M.Media.Mxc.server_name mxc)
      ~media_id:(M.Media.Mxc.media_id mxc)
  in
  Printf.printf "Downloaded %d bytes (uploaded %d)\n%!"
    (String.length content.body)
    (String.length data)
