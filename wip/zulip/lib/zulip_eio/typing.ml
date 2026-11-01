type op = Start | Stop

let op_to_string = function Start -> "start" | Stop -> "stop"
let unit_result = Result.map (Fun.const ())

let set_dm client ~op ~user_ids =
  match Codec.encode (Jsont.list Zulip.Id.User.jsont) user_ids with
  | Error _ as error -> error
  | Ok to_ ->
      Client.request client ~method_:`POST ~path:"/api/v1/typing"
        ~params:[ ("op", op_to_string op); ("type", "direct"); ("to", to_) ]
        ()
      |> unit_result

let set_channel client ~op ~channel_id ~topic =
  Client.request client ~method_:`POST ~path:"/api/v1/typing"
    ~params:
      [
        ("op", op_to_string op);
        ("type", "channel");
        ("stream_id", string_of_int (Zulip.Id.Channel.to_int channel_id));
        ("topic", topic);
      ]
    ()
  |> unit_result

let set_edit client ~op ~message_id =
  Client.request client ~method_:`POST
    ~path:
      ("/api/v1/messages/"
      ^ string_of_int (Zulip.Id.Message.to_int message_id)
      ^ "/typing")
    ~params:[ ("op", op_to_string op) ]
    ()
  |> unit_result

let set client ~op ~to_ =
  match to_ with
  | `User_ids user_ids -> set_dm client ~op ~user_ids
  | `Channel (channel_id, topic) -> set_channel client ~op ~channel_id ~topic
  | `Message_edit message_id -> set_edit client ~op ~message_id
