module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Push = Jmap_eio.Push

let seconds =
  let parse text =
    match float_of_string_opt text with
    | Some seconds when Float.is_finite seconds && seconds >= 0. -> Ok seconds
    | _ -> Error (`Msg "expected a finite, non-negative number of seconds")
  in
  Cmdliner.Arg.conv (parse, Fmt.float)

let args =
  let open Cmdliner in
  let poll =
    Arg.(
      value & opt seconds 2.
      & info [ "poll" ] ~docv:"SECONDS"
          ~doc:"Close each connection after this interval; 0 keeps it open.")
  in
  let timeout =
    Arg.(
      value & opt seconds 5.
      & info [ "timeout" ] ~docv:"SECONDS"
          ~doc:"Stop consuming events after this interval.")
  in
  Term.(const (fun poll timeout -> (poll, timeout)) $ poll $ timeout)

let rec consume sub =
  match Push.next sub with
  | `Event event ->
      Fmt.pr "%a@." Push.pp_event event;
      consume sub
  | `End -> (
      match Eio.Promise.await (Push.result sub) with
      | Ok () -> Fmt.pr "The subscription ended.@."
      | Error error -> Fmt.failwith "push: %a" Client.pp_error error)

let () =
  Cli.main' "events" ~doc:"Consume push events and observe completion" ~args
  @@ fun ctx (poll, seconds) ->
  let timeout =
    Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock ctx.env) seconds
  in
  let sub =
    Push.subscribe ~sw:ctx.sw ctx.client ~types:[ "Email"; "Mailbox" ]
      ~capacity:16 ~ping:30
      ?poll:(if poll = 0. then None else Some poll)
      ~last_event_id:"1" ()
  in
  Fun.protect
    ~finally:(fun () -> Push.close sub)
    (fun () ->
      try Eio.Time.Timeout.run_exn timeout (fun () -> consume sub)
      with Eio.Time.Timeout ->
        Push.close sub;
        consume sub;
        Fmt.pr "Stopped after %.0f seconds.@." seconds)
