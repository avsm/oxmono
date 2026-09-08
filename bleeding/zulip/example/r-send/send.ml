(* Choose an existing channel your account can post to. *)
let channel = "sandbox"
let topic = "OCaml tutorial"
let content = "Hello from OCaml!"

let send context =
  let client = Zulip_bot.Context.client context in
  let message_id =
    Zulip_eio.Messages.send_channel client ~channel ~topic ~content ()
    |> Zulip_eio.Error.or_raise
  in
  Format.printf "Sent message %a@." Zulip.Id.Message.pp message_id

let () =
  Zulip_bot_cli.Main.run_once ~name:"send"
    ~doc:"Send one channel message and exit." send ()
