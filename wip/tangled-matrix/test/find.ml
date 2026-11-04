(* SPDX-License-Identifier: ISC *)
(* [find.exe HOMESERVER USER PASSWORD-FILE STATE-DIR ALIAS] joins the rooms
   the bot is invited to and prints the room ALIAS finds. *)
open Tangled_matrix

let () =
  match Sys.argv with
  | [| _; homeserver; user; password_file; state; alias |] -> (
      Eio_main.run @@ fun env ->
      let config =
        {
          Bot.user;
          password =
            String.trim
              (In_channel.with_open_bin password_file In_channel.input_all);
          homeserver = Some homeserver;
          recovery_key = None;
          allow_unencrypted = false;
        }
      in
      let found = ref None in
      try
        Eio.Switch.run @@ fun sw ->
        let bot =
          Bot.connect ~sw ~env ~state:Eio.Path.(Eio.Stdenv.fs env / state) config
        in
        let rec wait n =
          match Bot.find_room bot alias with
          | Some r when Bot.ready bot ~room:r -> found := Some r
          | _ when n = 0 -> ()
          | _ ->
              Eio.Time.sleep env#clock 0.5;
              wait (n - 1)
        in
        wait 120;
        Eio.Switch.fail sw Exit
      with Exit ->
        print_endline (Option.value ~default:"not found" !found))
  | _ -> exit 2
