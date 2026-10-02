(* These tests transcribe real audio. [say] synthesises it, so they need only
   macOS and an installed English model, which [transcribe] fetches. *)

let check name value = Alcotest.(check bool) name true value
let dir = Filename.get_temp_dir_name ()

let created = ref []

let file name =
  let path =
    Filename.concat dir
      (Printf.sprintf "apple-speech-%d-%s" (Unix.getpid ()) name)
  in
  created := path :: !created;
  path

let () =
  at_exit (fun () ->
      List.iter (fun p -> if Sys.file_exists p then Sys.remove p) !created)

let run command =
  if Sys.command (command ^ " >/dev/null 2>&1") <> 0 then
    Alcotest.failf "command failed: %s" command

let speech =
  lazy
    (let path = file "speech.wav" in
     run
       (Printf.sprintf
          "say -o %s --data-format=LEI16@16000 'Remind me to buy tea tomorrow \
           at nine'"
          (Filename.quote path));
     path)

(* A WAV header followed by one second of 16 kHz mono zeros. *)
let silence () =
  let path = file "silence.wav" in
  let samples = 16000 in
  let data = samples * 2 in
  let b = Buffer.create (44 + data) in
  let u32 n = Buffer.add_int32_le b (Int32.of_int n)
  and u16 n = Buffer.add_uint16_le b n in
  Buffer.add_string b "RIFF";
  u32 (36 + data);
  Buffer.add_string b "WAVEfmt ";
  u32 16;
  u16 1;
  u16 1;
  u32 16000;
  u32 32000;
  u16 2;
  u16 16;
  Buffer.add_string b "data";
  u32 data;
  Buffer.add_string b (String.make data '\000');
  Out_channel.with_open_bin path (fun oc -> Buffer.output_buffer oc b);
  path

let mentions_tea segments =
  let text = String.lowercase_ascii (Apple_speech.text segments) in
  let rec find i =
    i + 3 <= String.length text && (String.sub text i 3 = "tea" || find (i + 1))
  in
  find 0

let error f =
  match f () with _ -> None | exception Apple_speech.Error e -> Some e

let test_available () =
  check "device can transcribe" (Apple_speech.available ())

let test_locales () =
  let supported = Apple_speech.supported_locales () in
  check "English is supported"
    (List.exists (fun l -> String.starts_with ~prefix:"en" l) supported);
  check "installed locales are supported"
    (List.for_all
       (fun l -> List.mem l supported)
       (Apple_speech.installed_locales ()))

let test_transcribe () =
  let path = Lazy.force speech in
  let segments = Apple_speech.transcribe ~locale:"en-GB" path in
  check "speech becomes text" (mentions_tea segments);
  check "segments are timed and ordered"
    (List.for_all
       (fun (s : Apple_speech.segment) -> s.start >= 0. && s.duration > 0.)
       segments);
  check "an installed model needs no download"
    (Apple_speech.status ~locale:"en-GB" () = Apple_speech.Installed
    && mentions_tea
         (Apple_speech.transcribe ~locale:"en-GB" ~install:false path))

let test_ogg () =
  if Sys.command "command -v ffmpeg >/dev/null" <> 0 then
    Alcotest.skip ()
  else begin
    let ogg = file "speech.ogg" in
    run
      (Printf.sprintf "ffmpeg -y -i %s -c:a libopus %s"
         (Filename.quote (Lazy.force speech))
         (Filename.quote ogg));
    check "Opus in Ogg, as Matrix voice messages use"
      (mentions_tea (Apple_speech.transcribe ~locale:"en-GB" ogg))
  end

let test_silence () =
  check "silence has no segments"
    (Apple_speech.transcribe ~locale:"en-GB" (silence ()) = [])

let test_errors () =
  check "missing file"
    (match error (fun () -> Apple_speech.transcribe (file "missing.wav")) with
    | Some (Unreadable _) -> true
    | _ -> false);
  let text = file "not-audio.wav" in
  Out_channel.with_open_bin text (fun oc -> output_string oc "not audio");
  check "file that is not audio"
    (match error (fun () -> Apple_speech.transcribe text) with
    | Some (Unreadable _) -> true
    | _ -> false);
  check "unsupported locale"
    (match
       error (fun () ->
           Apple_speech.transcribe ~locale:"xx-ZZ" (Lazy.force speech))
     with
    | Some (Unsupported_locale _) -> true
    | _ -> false)

let test_text () =
  check "text joins trimmed segments"
    (Apple_speech.text
       [
         { start = 0.; duration = 1.; text = " Hello. " };
         { start = 1.; duration = 1.; text = "" };
         { start = 2.; duration = 1.; text = "World." };
       ]
    = "Hello. World.")

let mgr = ref None

let process () =
  match !mgr with Some m -> m | None -> Alcotest.fail "no process manager"

let test_voices () =
  let voices = Apple_speech.voices (process ()) in
  check "English voices are installed"
    (List.exists
       (fun (v : Apple_speech.voice) ->
         String.starts_with ~prefix:"en" v.locale && v.name <> "")
       voices);
  check "names with spaces keep their locale apart"
    (List.for_all
       (fun (v : Apple_speech.voice) ->
         String.length v.locale >= 4 && not (String.contains v.locale ' '))
       voices)

let test_round_trip () =
  List.iter
    (fun (format, suffix) ->
      let path = file ("synth." ^ suffix) in
      Apple_speech.synthesize (process ()) ~format
        ~text:"Remind me to buy tea at nine." path;
      check
        ("synthesised " ^ suffix ^ " has a plausible duration")
        (let d = Apple_speech.duration path in
         d > 0.5 && d < 10.);
      check
        ("synthesised " ^ suffix ^ " transcribes back")
        (mentions_tea (Apple_speech.transcribe ~locale:"en-GB" path)))
    [ (Apple_speech.M4a, "m4a"); (Apple_speech.Wav, "wav") ]

let test_synthesis_input () =
  let path = file "dash.m4a" and decoy = file "decoy.m4a" in
  Apple_speech.synthesize (process ()) ~text:("-o " ^ decoy ^ " hello") path;
  check "leading dashes are text, not options"
    (Sys.file_exists path && not (Sys.file_exists decoy));
  check "unknown voices are refused"
    (match
       error (fun () ->
           Apple_speech.synthesize (process ()) ~voice:"No Such Voice"
             ~text:"hi" (file "x.m4a"))
     with
    | Some (Failed _) -> true
    | _ -> false);
  List.iter
    (fun (label, f) ->
      check label
        (match f () with
        | () -> false
        | exception Invalid_argument _ -> true))
    [
      ( "blank text refused",
        fun () ->
          Apple_speech.synthesize (process ()) ~text:" " (file "x.m4a") );
      ( "rate out of range refused",
        fun () ->
          Apple_speech.synthesize (process ()) ~rate:5 ~text:"hi"
            (file "x.m4a") );
    ]

let () =
  Eio_main.run @@ fun env ->
  mgr := Some (Eio.Stdenv.process_mgr env);
  Alcotest.run "apple-speech"
    [
      ( "speech",
        [
          ("available", `Quick, test_available);
          ("locales", `Quick, test_locales);
          ("transcribe", `Quick, test_transcribe);
          ("ogg", `Quick, test_ogg);
          ("silence", `Quick, test_silence);
          ("errors", `Quick, test_errors);
          ("text", `Quick, test_text);
          ("voices", `Quick, test_voices);
          ("round trip", `Quick, test_round_trip);
          ("synthesis input", `Quick, test_synthesis_input);
        ] );
    ]
