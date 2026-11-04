(* SPDX-License-Identifier: ISC *)
(* [live.exe HOMESERVER USER PASSWORD-FILE STATE-DIR ROOM] connects the bot to
   a real homeserver, sends a message to ROOM and edits it, then connects
   again to check that the device is kept. It prints the event IDs. *)
open Tangled_matrix

let () =
  match Sys.argv with
  | [| _; homeserver; user; password_file; state; room |] ->
      Eio_main.run @@ fun env ->
      let config =
        {
          Bot.user;
          password =
            String.trim
              (In_channel.with_open_bin password_file In_channel.input_all);
          homeserver = Some homeserver;
          recovery_key =
            (let saved = Filename.concat state "recovery-key" in
             if Sys.file_exists saved then
               Some
                 (String.trim
                    (In_channel.with_open_bin saved In_channel.input_all))
             else None);
          allow_unencrypted = false;
        }
      in
      let state = Eio.Path.(Eio.Stdenv.fs env / state) in
      let reconnected = ref None in
      let connect () =
        Eio.Switch.run @@ fun sw ->
        let bot = Bot.connect ~sw ~env ~state config in
        let clock = env#clock in
        let rec wait n =
          if Bot.ready bot ~room then ()
          else if n = 0 then failwith "the room never became ready"
          else (
            Eio.Time.sleep clock 0.5;
            wait (n - 1))
        in
        wait 120;
        reconnected := Some (Bot.device_id bot);
        Eio.Switch.fail sw Exit
      in
      let send () =
        Eio.Switch.run @@ fun sw ->
        let bot = Bot.connect ~sw ~env ~state config in
        let rec wait n =
          if Bot.ready bot ~room then ()
          else if n = 0 then failwith "the room never became ready"
          else (
            Eio.Time.sleep env#clock 0.5;
            wait (n - 1))
        in
        wait 120;
        let first =
          Bot.send bot ~room
            { plain = "🔄 first: running"; html = "<b>first</b>" }
        in
        Bot.edit bot ~room ~event:first
          { plain = "✅ first: passed"; html = "<b>first</b> passed" };
        Printf.printf "device %s\nsent %s\n%!" (Bot.device_id bot) first;
        Eio.Switch.fail sw Exit
      in
      (try send () with Exit -> ());
      (try connect () with Exit -> ());
      Printf.printf "reconnected as %s\n%!" (Option.get !reconnected)
  | _ ->
      prerr_endline "live.exe HOMESERVER USER PASSWORD-FILE STATE-DIR ROOM";
      exit 2
