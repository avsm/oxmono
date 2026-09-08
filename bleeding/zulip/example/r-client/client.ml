let inspect context =
  let identity = Zulip_bot.Context.identity context in
  Format.printf "%s <%s>, user %a@." identity.full_name identity.email
    Zulip.Id.User.pp identity.user_id;
  let client = Zulip_bot.Context.client context in
  let subscriptions =
    Zulip_eio.Channels.get_subscriptions client |> Zulip_eio.Error.or_raise
  in
  List.iter
    (fun subscription ->
      let channel = Zulip.Channel.Subscription.channel subscription in
      Format.printf "  %s@." (Zulip.Channel.name channel))
    subscriptions

let () =
  Zulip_bot_cli.Main.run_once ~name:"client"
    ~doc:"Print the account and its subscribed channels." inspect ()
