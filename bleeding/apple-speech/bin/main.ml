open Cmdliner

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
        List.iter print_endline
          (if installed then Apple_speech.installed_locales ()
           else Apple_speech.supported_locales ()))
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

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "apple-speech" ~doc:"On-device speech transcription.")
          [ transcribe; locales; install ]))
