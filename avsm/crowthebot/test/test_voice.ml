open Crowthebot

let check name value = if not value then failwith name
let json text = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json text)

let temp suffix =
  let path = Filename.temp_file "crow-voice-test-" suffix in
  at_exit (fun () -> if Sys.file_exists path then Sys.remove path);
  path

let speech =
  lazy
    (let path = temp ".wav" in
     if
       Sys.command
         (Printf.sprintf
            "say -o %s --data-format=LEI16@16000 'Remind me to buy tea \
             tomorrow' >/dev/null 2>&1"
            (Filename.quote path))
       <> 0
     then failwith "say failed";
     In_channel.with_open_bin path In_channel.input_all)

let audio ?(info = {|{"mimetype":"audio/ogg","size":1000,"duration":3000}|})
    source =
  json
    (Printf.sprintf
       {|{"msgtype":"m.audio","body":"Voice message.ogg","info":%s,%s,"org.matrix.msc3245.voice":{}}|}
       info source)

let plain = audio {|"url":"mxc://example.org/voice"|}

let encrypted =
  audio
    {|"file":{"url":"mxc://example.org/secret","key":{"kty":"oct","alg":"A256CTR","ext":true,"k":"AAAA","key_ops":["encrypt","decrypt"]},"iv":"AAAAAAAAAAAAAAAAAAAAAA","hashes":{"sha256":"AAAA"},"v":"v2"}|}

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let () =
  Eio_main.run @@ fun _ ->
  check "plain media source"
    (match Voice.source plain with
    | Ok (Matrix_eio.Media.Plain _) -> true
    | _ -> false);
  check "encrypted media source"
    (match Voice.source encrypted with
    | Ok (Matrix_eio.Media.Encrypted _) -> true
    | _ -> false);
  List.iter
    (fun (label, content) ->
      check label (Result.is_error (Voice.source content)))
    [
      ( "oversized claim refused before download",
        audio ~info:{|{"size":30000000}|} {|"url":"mxc://example.org/v"|} );
      ( "long recording refused before download",
        audio ~info:{|{"duration":700000}|} {|"url":"mxc://example.org/v"|} );
      ("media without a source", audio {|"external":true|});
      ("malformed URL", audio {|"url":"https://example.org/voice.ogg"|});
      ("text is not media", json {|{"msgtype":"m.text","body":"hello"}|});
    ];
  let fetched = ref 0 in
  let serve body _ =
    incr fetched;
    body
  in
  (match
     Voice.transcribe ~download:(serve (Lazy.force speech)) ~locale:"en-GB"
       plain
   with
  | Ok text ->
      check "transcript is marked and recognised"
        (String.starts_with ~prefix:"[voice message] " text
        && contains (String.lowercase_ascii text) "tea")
  | Error e -> failwith ("transcription failed: " ^ e));
  let silence = String.make 32044 '\000' in
  let header = Bytes.of_string (String.sub (Lazy.force speech) 0 44) in
  Bytes.set_int32_le header 40 32000l;
  Bytes.set_int32_le header 4 32036l;
  let silence = Bytes.to_string header ^ String.sub silence 0 32000 in
  check "silence is an error, not an empty request"
    (Result.is_error
       (Voice.transcribe ~download:(serve silence) ~locale:"en-GB" plain));
  check "a body that is not audio is an error"
    (Result.is_error
       (Voice.transcribe ~download:(serve "not audio") ~locale:"en-GB" plain));
  check "download failures are errors"
    (Result.is_error
       (Voice.transcribe
          ~download:(fun _ -> failwith "M_NOT_FOUND")
          ~locale:"en-GB" plain));
  let before = !fetched in
  check "refused claims are never downloaded"
    (Result.is_error
       (Voice.transcribe ~download:(serve "")
          (audio ~info:{|{"size":30000000}|} {|"url":"mxc://example.org/v"|}))
    && !fetched = before);
  check "oversized bodies are refused after download"
    (Result.is_error
       (Voice.transcribe
          ~download:(serve (String.make (21 * 1024 * 1024) 'x'))
          plain));
  print_endline "crowthebot: voice message transcription passed"
