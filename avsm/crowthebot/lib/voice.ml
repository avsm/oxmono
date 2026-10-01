module Media = Matrix_eio.Media
module Content = Matrix_proto.Event.Media_message_content

type source = Media.source

let marker = "[voice message] "
let max_bytes = 20 * 1024 * 1024
let max_milliseconds = 10 * 60 * 1000

let decode content =
  match
    Jsont_bytesrw.encode_string Jsont.json content
    |> Result.to_option
    |> Option.map (Jsont_bytesrw.decode_string Content.jsont)
  with
  | Some (Ok c) -> Ok c
  | _ -> Error "not a media message"

(* The sender states the size and duration, so they only screen out obvious
   abuse before download. The body is checked again once it arrives. *)
let source content =
  Result.bind (decode content) (fun (c : Content.t) ->
      let info = c.info in
      let claimed f = Option.bind info f in
      if
        Option.fold ~none:false
          ~some:(fun n -> n > max_bytes)
          (claimed (fun i -> i.size))
      then Error "voice message is larger than 20 MiB"
      else if
        Option.fold ~none:false
          ~some:(fun n -> n > max_milliseconds)
          (claimed (fun i -> i.duration))
      then Error "voice message is longer than ten minutes"
      else
        match (c.file, c.url) with
        | Some file, _ -> Ok (Media.Encrypted file)
        | None, Some url -> (
            match Media.Mxc.of_string url with
            | Ok mxc -> Ok (Media.Plain mxc)
            | Error (`Msg m) -> Error m)
        | None, None -> Error "voice message has no media")

let download client source =
  match Media.get_content client { source; format = Media.File } with
  | Ok body -> body
  | Error e ->
      failwith (Format.asprintf "%a" Matrix_client.Media.pp_encrypted_error e)

let with_temp body f =
  let path = Filename.temp_file "crow-voice-" ".audio" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      Out_channel.with_open_bin path (fun oc -> output_string oc body);
      f path)

let transcribe ~download ?locale content =
  match source content with
  | Error _ as e -> e
  | Ok src -> (
      try
        let body = download src in
        if String.length body > max_bytes then
          Error "voice message is larger than 20 MiB"
        else
          let text =
            with_temp body (fun path ->
                Apple_speech.text (Apple_speech.transcribe ?locale path))
          in
          if String.trim text = "" then Error "no speech recognised"
          else Ok (marker ^ text)
      with
      | Apple_speech.Error e ->
          Error (Format.asprintf "%a" Apple_speech.pp_error e)
      | Failure m -> Error m)
