open Cmdliner

module C = Console
let accent = C.Style.(bold + fg (C.Color.rgb 0x4b 0xc9 0xc3))
let muted = C.Style.fg C.Color.bright_black
let styled style value = C.Span.sanitize (C.Span.styled style value)

let locale =
  Arg.(
    value
    & opt (some string) None
    & info [ "l"; "locale" ] ~docv:"LOCALE"
        ~doc:"Locale such as $(b,en-GB). Defaults to the user's locale.")

let run f =
  Eio_main.run @@ fun _ ->
  try
    f ();
    0
  with Apple_speech.Error e ->
    Format.eprintf "apple-speech: %a@." Apple_speech.pp_error e;
    1

let transcribe =
  let files =
    Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc:"Audio file.")
  and no_install =
    Arg.(
      value & flag
      & info [ "no-install" ]
          ~doc:"Fail instead of downloading a missing language model.")
  and timings =
    Arg.(
      value & flag
      & info [ "t"; "timings" ] ~doc:"Print each segment with its start time.")
  in
  let go locale no_install timings files =
    run (fun () ->
        List.iter
          (fun file ->
            let segments =
              Apple_speech.transcribe ?locale ~install:(not no_install) file
            in
            if timings then
              List.iter
                (fun (s : Apple_speech.segment) ->
                  Printf.printf "%7.2f %s\n" s.start s.text)
                segments
            else print_endline (Apple_speech.text segments))
          files)
  in
  Cmd.v
    (Cmd.info "transcribe" ~doc:"Transcribe audio files to text.")
    Term.(const go $ locale $ no_install $ timings $ files)

let locales =
  let installed =
    Arg.(value & flag & info [ "installed" ] ~doc:"Only installed locales.")
  in
  let go installed =
    run (fun () ->
        let locales =
          if installed then Apple_speech.installed_locales ()
          else Apple_speech.supported_locales () in
        if Console_eio.is_tty () then begin
          Format.printf "%a  %a@." C.Span.pp (styled accent "Locales")
            C.Span.pp (styled muted
              (Printf.sprintf "%d total" (List.length locales)));
          List.iter (fun locale ->
            Format.printf "  %a@." C.Span.pp (styled accent locale)) locales
        end else List.iter print_endline locales)
  in
  Cmd.v
    (Cmd.info "locales" ~doc:"List locales with a transcription model.")
    Term.(const go $ installed)

let install =
  let go locale =
    run (fun () -> print_endline (Apple_speech.install ?locale ()))
  in
  Cmd.v
    (Cmd.info "install" ~doc:"Download a locale's transcription model.")
    Term.(const go $ locale)

let run_env f =
  Eio_main.run @@ fun env ->
  try
    f (Eio.Stdenv.process_mgr env);
    0
  with Apple_speech.Error e ->
    Format.eprintf "apple-speech: %a@." Apple_speech.pp_error e;
    1

let say =
  let text =
    Arg.(
      required & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc:"Text.")
  and output =
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"FILE" ~doc:"Audio file to write.")
  and voice =
    Arg.(
      value
      & opt (some string) None
      & info [ "v"; "voice" ] ~docv:"VOICE" ~doc:"Voice name from $(b,voices).")
  and rate =
    Arg.(
      value
      & opt (some int) None
      & info [ "r"; "rate" ] ~docv:"WPM" ~doc:"Words per minute, 50 to 700.")
  and format =
    Arg.(
      value
      & opt
          (enum
             [
               ("m4a", Apple_speech.M4a);
               ("wav", Apple_speech.Wav);
               ("aiff", Apple_speech.Aiff);
             ])
          Apple_speech.M4a
      & info [ "f"; "format" ] ~docv:"FORMAT" ~doc:"m4a, wav or aiff.")
  in
  let go text output voice rate format =
    run_env (fun mgr ->
        Apple_speech.synthesize mgr ?voice ?rate ~format ~text output)
  in
  Cmd.v
    (Cmd.info "say" ~doc:"Synthesise speech into an audio file.")
    Term.(const go $ text $ output $ voice $ rate $ format)

let voices =
  let go () =
    run_env (fun mgr ->
        let voices = Apple_speech.voices mgr in
        if Console_eio.is_tty () then begin
          let rows = List.map (fun (v : Apple_speech.voice) ->
            [styled accent v.name; styled muted v.locale]) voices in
          Format.printf "%a  %a@.%a@." C.Span.pp (styled accent "Voices")
            C.Span.pp (styled muted
              (Printf.sprintf "%d total" (List.length voices)))
            C.Table.pp (C.Table.of_rows ~border:C.Border.rounded
              C.Table.[column "Name"; column "Locale"] rows)
        end else List.iter
          (fun (v : Apple_speech.voice) ->
            Printf.printf "%-28s %s\n" v.name v.locale) voices)
  in
  Cmd.v
    (Cmd.info "voices" ~doc:"List speech voices.")
    Term.(const go $ const ())

let () =
  Console_eio.setup ();
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "apple-speech" ~doc:"On-device speech transcription.")
          [ transcribe; locales; install; say; voices ]))
