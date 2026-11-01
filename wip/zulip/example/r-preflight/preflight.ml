let inspect context =
  let client = Zulip_bot.Context.client context in
  let settings =
    Zulip_eio.Server.get_settings client |> Zulip_eio.Error.or_raise
  in
  let user = Zulip_eio.Users.me client |> Zulip_eio.Error.or_raise in
  let channels =
    Zulip_eio.Channels.get_subscriptions client |> Zulip_eio.Error.or_raise
  in
  Format.printf "Connected to %s (Zulip %s, feature level %d)@."
    (Zulip_eio.Client.site client)
    settings.zulip_version settings.zulip_feature_level;
  Format.printf "Account: %s, user %a, bot: %b@."
    (Zulip.User.full_name user)
    Zulip.Id.User.pp (Zulip.User.user_id user) (Zulip.User.is_bot user);
  List.iter
    (fun subscription ->
      let channel = Zulip.Channel.Subscription.channel subscription in
      Format.printf "Channel: %s@." (Zulip.Channel.name channel))
    channels

let () =
  Zulip_bot_cli.Main.run_once ~name:"preflight"
    ~doc:
      "Check authentication, server version and subscribed channels without \
       sending messages."
    inspect ()
