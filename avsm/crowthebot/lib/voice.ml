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

let with_file suffix f =
  let path = Filename.temp_file "crow-voice-" suffix in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path)

let with_temp body f =
  with_file ".audio" (fun path ->
      Out_channel.with_open_bin path (fun oc -> output_string oc body);
      f path)

type note = {
  audio : string;
  content_type : string;
  filename : string;
  duration : int;
  waveform : int list;
}

(* [say] writes a RIFF file with padding chunks before the samples, so the
   chunks are walked rather than assuming a 44-byte header. *)
let pcm wav =
  let len = String.length wav in
  let u32 i = Int32.to_int (String.get_int32_le wav i) land 0xffff_ffff in
  if len < 12 || String.sub wav 0 4 <> "RIFF" || String.sub wav 8 4 <> "WAVE"
  then failwith "synthesised audio is not WAV";
  let rec walk pos rate =
    if pos + 8 > len then failwith "synthesised WAV has no samples"
    else
      let id = String.sub wav pos 4 and size = u32 (pos + 4) in
      let body = pos + 8 in
      match id with
      | "fmt " ->
          if
            String.get_uint16_le wav (body + 2) <> 1
            || String.get_uint16_le wav (body + 14) <> 16
          then failwith "synthesised WAV is not 16-bit mono";
          walk (body + size + (size land 1)) (Some (u32 (body + 4)))
      | "data" -> (
          match rate with
          | None -> failwith "synthesised WAV has no format"
          | Some rate -> (rate, body, min size (len - body) / 2))
      | _ -> walk (body + size + (size land 1)) rate
  in
  walk 12 None

(* Voice players draw up to about a hundred bars, each 0 to 1024. *)
let waveform wav ~start ~samples =
  let bars = max 1 (min 100 samples) in
  List.init bars (fun bar ->
      let first = bar * samples / bars and last = (bar + 1) * samples / bars in
      let peak = ref 0 in
      for i = first to last - 1 do
        peak := max !peak (abs (String.get_int16_le wav (start + (2 * i))))
      done;
      min 1024 (!peak * 1024 / 32767))

let ffmpeg =
  List.find_opt Sys.file_exists
    [ "/opt/homebrew/bin/ffmpeg"; "/usr/local/bin/ffmpeg"; "/usr/bin/ffmpeg" ]

let run process_mgr args =
  try Eio.Process.run process_mgr args
  with Eio.Io _ as e ->
    failwith ("audio encoding failed: " ^ Printexc.to_string e)

let speak ~process_mgr ~voice text =
  try
    with_file ".wav" (fun wav_path ->
        Apple_speech.synthesize process_mgr ~voice ~format:Apple_speech.Wav
          ~text wav_path;
        let wav = In_channel.with_open_bin wav_path In_channel.input_all in
        let rate, start, samples = pcm wav in
        let duration = samples * 1000 / max 1 rate in
        let waveform = waveform wav ~start ~samples in
        (* Clients expect Opus in Ogg for a voice message. macOS writes Opus
           only into CAF, so Ogg needs ffmpeg. AAC is the fallback. *)
        let suffix, content_type, args =
          match ffmpeg with
          | Some ffmpeg ->
              ( ".ogg",
                "audio/ogg",
                fun out ->
                  [ ffmpeg; "-nostdin"; "-loglevel"; "error"; "-y"; "-i";
                    wav_path; "-c:a"; "libopus"; "-b:a"; "32k"; "-ar"; "48000";
                    "-ac"; "1"; out ] )
          | None ->
              ( ".m4a",
                "audio/mp4",
                fun out ->
                  [ "/usr/bin/afconvert"; wav_path; out; "-f"; "m4af"; "-d";
                    "aac" ] )
        in
        with_file suffix (fun out ->
            run process_mgr (args out);
            let audio = In_channel.with_open_bin out In_channel.input_all in
            if String.length audio > max_bytes then
              failwith "voice note is larger than 20 MiB";
            {
              audio;
              content_type;
              filename = "Voice message" ^ suffix;
              duration;
              waveform;
            }))
  with
  | Apple_speech.Error e ->
      failwith (Format.asprintf "%a" Apple_speech.pp_error e)
  | Invalid_argument m -> failwith m

let image ~download content =
  match source content with
  | Error _ as e -> e
  | Ok src -> (
      match download src with
      | exception Failure m -> Error m
      | body when String.length body > max_bytes ->
          Error "image is larger than 20 MiB"
      | body -> (
          match Agentkit.Chat.image_of_string body with
          | Some image -> Ok image
          | None -> Error "not a PNG, JPEG, WebP or GIF image"))

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
