(* SPDX-License-Identifier: ISC *)
(* [diagnose URL MODEL PROJECT LOG...] asks the model at URL, as the
   [diagnose] workflow would, about builds of the checkout PROJECT that
   failed with the given log files, and prints its answer.
   OPENROUTER_API_KEY is sent when set. *)
open Ocaml_ci_spindle

let () =
  let url = Sys.argv.(1) and model = Sys.argv.(2) and project = Sys.argv.(3) in
  let logs = Array.to_list (Array.sub Sys.argv 4 (Array.length Sys.argv - 4)) in
  let files : Spindle.Executor.files =
    {
      list =
        (fun dir -> Array.to_list (Sys.readdir (Filename.concat project dir)));
      read =
        (fun path ->
          let path = Filename.concat project path in
          if Sys.file_exists path && not (Sys.is_directory path) then
            Some (In_channel.with_open_bin path In_channel.input_all)
          else None);
    }
  in
  let failures =
    List.map
      (fun path ->
        {
          Diagnose.workflow = Filename.basename path;
          error = None;
          log = In_channel.with_open_bin path In_channel.input_all;
        })
      logs
  in
  Eio_main.run @@ fun env ->
  let key =
    match Sys.getenv_opt "OPENROUTER_API_KEY" with
    | Some "" | None -> None
    | key -> key
  in
  let d =
    Diagnose.v ?key ~model ~clock:env#mono_clock
      (Fetch_httpz.v env#net ~clock:env#mono_clock ~https:Httpz_tls.system ())
      url
  in
  let prompt = Diagnose.prompt ~files ~passed:[] failures in
  Printf.eprintf "Asking %s with %d KiB.\n%!" model (String.length prompt / 1024);
  match Diagnose.ask d ~prompt with
  | Ok answer -> print_endline answer
  | Error why ->
      prerr_endline why;
      exit 1
