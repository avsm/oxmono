type destination =
  | Channel of {
      channel_id : Id.Channel.t;
      channel_name : string;
      topic : string;
    }
  | Direct of { recipient_id : Id.Recipient.t; participants : Id.User.t list }

type t = {
  id : Id.Message.t;
  sender_id : Id.User.t;
  sender_email : string;
  sender_full_name : string;
  timestamp : float;
  content : string;
  content_type : string;
  destination : destination;
  flags : Message_flag.t list;
  raw : Jsont.json;
}

let id t = t.id
let sender_id t = t.sender_id
let sender_email t = t.sender_email
let sender_full_name t = t.sender_full_name
let timestamp t = t.timestamp
let content t = t.content
let content_type t = t.content_type
let destination t = t.destination
let flags t = t.flags
let raw t = t.raw

type direct_participant = {
  user_id : Id.User.t;
  email : string option;
  full_name : string option;
  is_mirror_dummy : bool option;
  raw : Jsont.json;
}

type edit = {
  topic : string option;
  previous_topic : string option;
  channel_id : Id.Channel.t option;
  previous_channel_id : Id.Channel.t option;
  content : string option;
  rendered_content : string option;
  previous_content : string option;
  previous_rendered_content : string option;
  user_id : Id.User.t option;
  content_html_diff : string option;
  timestamp : int option;
  raw : Jsont.json;
}

type reaction = {
  emoji_name : string;
  emoji_code : string;
  reaction_type : string;
  user_id : Id.User.t;
  raw : Jsont.json;
}

type topic_link = { text : string; url : string; raw : Jsont.json }

type submessage = {
  id : int;
  message_id : Id.Message.t;
  sender_id : Id.User.t;
  msg_type : string;
  content : string;
  raw : Jsont.json;
}

let object_with_unknown known unknown =
  let unknown =
    match unknown with Jsont.Object (members, _) -> members | _ -> []
  in
  Jsont.Object (known @ unknown, Jsont.Meta.none)

let mem name value = ((name, Jsont.Meta.none), value)
let json_int value = Jsont.Json.int value
let json_string value = Jsont.Json.string value

let without known = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
          meta )
  | _ -> Jsont.Json.object' []

let direct_participant_jsont =
  let make user_id email full_name is_mirror_dummy unknown =
    let known =
      [ mem "id" (json_int (Id.User.to_int user_id)) ]
      @ Option.fold ~none:[]
          ~some:(fun value -> [ mem "email" (json_string value) ])
          email
      @ Option.fold ~none:[]
          ~some:(fun value -> [ mem "full_name" (json_string value) ])
          full_name
      @ Option.fold ~none:[]
          ~some:(fun value -> [ mem "is_mirror_dummy" (Jsont.Json.bool value) ])
          is_mirror_dummy
    in
    {
      user_id;
      email;
      full_name;
      is_mirror_dummy;
      raw = object_with_unknown known unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip direct-message participant" make
  |> Jsont.Object.mem "id" Id.User.jsont ~enc:(fun (p : direct_participant) ->
      p.user_id)
  |> Jsont.Object.opt_mem "email" Jsont.string
       ~enc:(fun (p : direct_participant) -> p.email)
  |> Jsont.Object.opt_mem "full_name" Jsont.string
       ~enc:(fun (p : direct_participant) -> p.full_name)
  |> Jsont.Object.opt_mem "is_mirror_dummy" Jsont.bool
       ~enc:(fun (p : direct_participant) -> p.is_mirror_dummy)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (p : direct_participant) ->
         without [ "id"; "email"; "full_name"; "is_mirror_dummy" ] p.raw)
  |> Jsont.Object.finish

let edit_jsont =
  let make topic previous_topic channel_id previous_channel_id content
      rendered_content previous_content previous_rendered_content user_id
      content_html_diff timestamp unknown =
    let add name json = function
      | None -> []
      | Some value -> [ mem name (json value) ]
    in
    let known =
      add "topic" json_string topic
      @ add "prev_topic" json_string previous_topic
      @ add "stream" (fun id -> json_int (Id.Channel.to_int id)) channel_id
      @ add "prev_stream"
          (fun id -> json_int (Id.Channel.to_int id))
          previous_channel_id
      @ add "content" json_string content
      @ add "rendered_content" json_string rendered_content
      @ add "prev_content" json_string previous_content
      @ add "prev_rendered_content" json_string previous_rendered_content
      @ [
          mem "user_id"
            (Option.fold ~none:(Jsont.Json.null ())
               ~some:(fun id -> json_int (Id.User.to_int id))
               user_id);
        ]
      @ add "content_html_diff" json_string content_html_diff
      @ add "timestamp" json_int timestamp
    in
    {
      topic;
      previous_topic;
      channel_id;
      previous_channel_id;
      content;
      rendered_content;
      previous_content;
      previous_rendered_content;
      user_id;
      content_html_diff;
      timestamp;
      raw = object_with_unknown known unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip message edit" make
  |> Jsont.Object.opt_mem "topic" Jsont.string ~enc:(fun (e : edit) -> e.topic)
  |> Jsont.Object.opt_mem "prev_topic" Jsont.string ~enc:(fun (e : edit) ->
      e.previous_topic)
  |> Jsont.Object.opt_mem "stream" Id.Channel.jsont ~enc:(fun (e : edit) ->
      e.channel_id)
  |> Jsont.Object.opt_mem "prev_stream" Id.Channel.jsont ~enc:(fun (e : edit) ->
      e.previous_channel_id)
  |> Jsont.Object.opt_mem "content" Jsont.string ~enc:(fun (e : edit) ->
      e.content)
  |> Jsont.Object.opt_mem "rendered_content" Jsont.string
       ~enc:(fun (e : edit) -> e.rendered_content)
  |> Jsont.Object.opt_mem "prev_content" Jsont.string ~enc:(fun (e : edit) ->
      e.previous_content)
  |> Jsont.Object.opt_mem "prev_rendered_content" Jsont.string
       ~enc:(fun (e : edit) -> e.previous_rendered_content)
  |> Jsont.Object.mem "user_id"
       (Jsont.option Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun (e : edit) -> e.user_id)
  |> Jsont.Object.opt_mem "content_html_diff" Jsont.string
       ~enc:(fun (e : edit) -> e.content_html_diff)
  |> Jsont.Object.opt_mem "timestamp" Jsont.int ~enc:(fun (e : edit) ->
      e.timestamp)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (e : edit) ->
      without
        [
          "topic";
          "prev_topic";
          "stream";
          "prev_stream";
          "content";
          "rendered_content";
          "prev_content";
          "prev_rendered_content";
          "user_id";
          "content_html_diff";
          "timestamp";
        ]
        e.raw)
  |> Jsont.Object.finish

let reaction_jsont =
  let make emoji_name emoji_code reaction_type user_id unknown =
    let known =
      [
        mem "emoji_name" (json_string emoji_name);
        mem "emoji_code" (json_string emoji_code);
        mem "reaction_type" (json_string reaction_type);
        mem "user_id" (json_int (Id.User.to_int user_id));
      ]
    in
    {
      emoji_name;
      emoji_code;
      reaction_type;
      user_id;
      raw = object_with_unknown known unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip message reaction" make
  |> Jsont.Object.mem "emoji_name" Jsont.string ~enc:(fun (r : reaction) ->
      r.emoji_name)
  |> Jsont.Object.mem "emoji_code" Jsont.string ~enc:(fun (r : reaction) ->
      r.emoji_code)
  |> Jsont.Object.mem "reaction_type" Jsont.string ~enc:(fun (r : reaction) ->
      r.reaction_type)
  |> Jsont.Object.mem "user_id" Id.User.jsont ~enc:(fun (r : reaction) ->
      r.user_id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : reaction) ->
      without [ "emoji_name"; "emoji_code"; "reaction_type"; "user_id" ] r.raw)
  |> Jsont.Object.finish

let topic_link_jsont =
  let make text url unknown =
    {
      text;
      url;
      raw =
        object_with_unknown
          [ mem "text" (json_string text); mem "url" (json_string url) ]
          unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip message topic link" make
  |> Jsont.Object.mem "text" Jsont.string ~enc:(fun (l : topic_link) -> l.text)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun (l : topic_link) -> l.url)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (l : topic_link) ->
      without [ "text"; "url" ] l.raw)
  |> Jsont.Object.finish

let submessage_jsont =
  let make msg_type content message_id sender_id id unknown =
    let known =
      [
        mem "msg_type" (json_string msg_type);
        mem "content" (json_string content);
        mem "message_id" (json_int (Id.Message.to_int message_id));
        mem "sender_id" (json_int (Id.User.to_int sender_id));
        mem "id" (json_int id);
      ]
    in
    {
      id;
      message_id;
      sender_id;
      msg_type;
      content;
      raw = object_with_unknown known unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip submessage" make
  |> Jsont.Object.mem "msg_type" Jsont.string ~enc:(fun (s : submessage) ->
      s.msg_type)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun (s : submessage) ->
      s.content)
  |> Jsont.Object.mem "message_id" Id.Message.jsont
       ~enc:(fun (s : submessage) -> s.message_id)
  |> Jsont.Object.mem "sender_id" Id.User.jsont ~enc:(fun (s : submessage) ->
      s.sender_id)
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun (s : submessage) -> s.id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (s : submessage) ->
      without [ "msg_type"; "content"; "message_id"; "sender_id"; "id" ] s.raw)
  |> Jsont.Object.finish

let member name codec raw =
  match raw with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | None -> None
      | Some (_, json) -> (
          match Jsont.Json.decode' codec json with
          | Ok value -> Some value
          | Error error -> raise (Jsont.Error error)))
  | _ -> None

let client (t : t) = member "client" Jsont.string t.raw

let avatar_url (t : t) =
  match member "avatar_url" (Jsont.option Jsont.string) t.raw with
  | None | Some None -> None
  | Some (Some url) -> Some url

let is_me_message (t : t) = member "is_me_message" Jsont.bool t.raw
let last_edit_timestamp (t : t) = member "last_edit_timestamp" Jsont.int t.raw
let last_moved_timestamp (t : t) = member "last_moved_timestamp" Jsont.int t.raw
let sender_realm (t : t) = member "sender_realm_str" Jsont.string t.raw
let edit_history (t : t) = member "edit_history" (Jsont.list edit_jsont) t.raw
let reactions (t : t) = member "reactions" (Jsont.list reaction_jsont) t.raw

let topic_links (t : t) =
  member "topic_links" (Jsont.list topic_link_jsont) t.raw

let submessages (t : t) =
  member "submessages" (Jsont.list submessage_jsont) t.raw

let direct_participants (t : t) =
  match t.destination with
  | Channel _ -> None
  | Direct _ ->
      member "display_recipient" (Jsont.list direct_participant_jsont) t.raw

type recipient = { user_id : Id.User.t }

let recipient_jsont =
  Jsont.Object.map ~kind:"Zulip direct-message recipient" (fun user_id ->
      { user_id })
  |> Jsont.Object.mem "id" Id.User.jsont ~enc:(fun (r : recipient) -> r.user_id)
  |> Jsont.Object.finish

type wire = {
  id : Id.Message.t;
  sender_id : Id.User.t;
  sender_email : string;
  sender_full_name : string;
  timestamp : float;
  content : string;
  content_type : string;
  type_ : string;
  stream_id : Id.Channel.t option;
  display_recipient : Jsont.json;
  topic : string option;
  recipient_id : Id.Recipient.t option;
  flags : Message_flag.t list;
  unknown : Jsont.json;
}

let json_number = Jsont.Json.int

let known_members w =
  let mem name value = ((name, Jsont.Meta.none), value) in
  let base =
    [
      mem "id" (json_number (Id.Message.to_int w.id));
      mem "sender_id" (json_number (Id.User.to_int w.sender_id));
      mem "sender_email" (json_string w.sender_email);
      mem "sender_full_name" (json_string w.sender_full_name);
      mem "timestamp" (Jsont.Json.number w.timestamp);
      mem "content" (json_string w.content);
      mem "content_type" (json_string w.content_type);
      mem "type" (json_string w.type_);
      mem "display_recipient" w.display_recipient;
      mem "flags"
        (Jsont.Json.list
           (List.map
              (fun flag -> json_string (Message_flag.to_string flag))
              w.flags));
    ]
  in
  let opt name f = function None -> [] | Some v -> [ mem name (f v) ] in
  base
  @ opt "stream_id" (fun id -> json_number (Id.Channel.to_int id)) w.stream_id
  @ opt "subject" json_string w.topic
  @ opt "recipient_id"
      (fun id -> json_number (Id.Recipient.to_int id))
      w.recipient_id

let raw_of_wire w =
  let unknown =
    match w.unknown with Jsont.Object (members, _) -> members | _ -> []
  in
  Jsont.Object (known_members w @ unknown, Jsont.Meta.none)

let participants_of_json = function
  | Jsont.Array (items, _) ->
      List.map
        (fun item ->
          match Jsont.Json.decode' recipient_jsont item with
          | Ok recipient -> recipient.user_id
          | Error error -> raise (Jsont.Error error))
        items
  | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Array json

let destination_of_wire w =
  match w.type_ with
  | "stream" ->
      let channel_id =
        match w.stream_id with
        | Some id -> id
        | None ->
            Jsont.Error.msgf Jsont.Meta.none "channel message lacks stream_id"
      in
      let channel_name =
        match w.display_recipient with
        | Jsont.String (name, _) -> name
        | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.String json
      in
      let topic =
        match w.topic with
        | Some topic -> topic
        | None ->
            Jsont.Error.msgf Jsont.Meta.none "channel message lacks subject"
      in
      Channel { channel_id; channel_name; topic }
  | "private" | "direct" ->
      let recipient_id =
        match w.recipient_id with
        | Some id -> id
        | None ->
            Jsont.Error.msgf Jsont.Meta.none "direct message lacks recipient_id"
      in
      Direct
        {
          recipient_id;
          participants = participants_of_json w.display_recipient;
        }
  | type_ ->
      Jsont.Error.msgf Jsont.Meta.none "unknown received message type %S" type_

let of_wire w =
  let message =
    {
      id = w.id;
      sender_id = w.sender_id;
      sender_email = w.sender_email;
      sender_full_name = w.sender_full_name;
      timestamp = w.timestamp;
      content = w.content;
      content_type = w.content_type;
      destination = destination_of_wire w;
      flags = w.flags;
      raw = raw_of_wire w;
    }
  in
  (* Validate every currently documented structured field while retaining the
     original object for future fields. *)
  ignore (client message);
  ignore (avatar_url message);
  ignore (is_me_message message);
  ignore (last_edit_timestamp message);
  ignore (last_moved_timestamp message);
  ignore (sender_realm message);
  ignore (edit_history message);
  ignore (reactions message);
  ignore (topic_links message);
  ignore (submessages message);
  ignore (direct_participants message);
  message

let wire_of_message t =
  let type_, stream_id, display_recipient, topic, recipient_id =
    match t.destination with
    | Channel { channel_id; channel_name; topic } ->
        ("stream", Some channel_id, json_string channel_name, Some topic, None)
    | Direct { recipient_id; participants } ->
        let recipient user_id =
          Jsont.Json.object'
            [ (Jsont.Json.name "id", json_number (Id.User.to_int user_id)) ]
        in
        ( "private",
          None,
          (match t.raw with
          | Jsont.Object (members, _) -> (
              match Jsont.Json.find_mem "display_recipient" members with
              | Some (_, json) -> json
              | None -> Jsont.Json.list (List.map recipient participants))
          | _ -> Jsont.Json.list (List.map recipient participants)),
          None,
          Some recipient_id )
  in
  let known =
    [
      "id";
      "sender_id";
      "sender_email";
      "sender_full_name";
      "timestamp";
      "content";
      "content_type";
      "type";
      "stream_id";
      "display_recipient";
      "subject";
      "recipient_id";
      "flags";
    ]
  in
  let unknown =
    match t.raw with
    | Jsont.Object (members, meta) ->
        Jsont.Object
          ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
            meta )
    | _ -> Jsont.Json.object' []
  in
  {
    id = t.id;
    sender_id = t.sender_id;
    sender_email = t.sender_email;
    sender_full_name = t.sender_full_name;
    timestamp = t.timestamp;
    content = t.content;
    content_type = t.content_type;
    type_;
    stream_id;
    display_recipient;
    topic;
    recipient_id;
    flags = t.flags;
    unknown;
  }

let timestamp_jsont =
  Jsont.iter ~kind:"Zulip message timestamp"
    ~dec:(fun timestamp ->
      if not (Float.is_finite timestamp) then
        Jsont.Error.msgf Jsont.Meta.none "timestamp must be finite")
    ~enc:(fun timestamp ->
      if not (Float.is_finite timestamp) then
        Jsont.Error.msgf Jsont.Meta.none "timestamp must be finite")
    Jsont.number

let wire_jsont =
  let make id sender_id sender_email sender_full_name timestamp content
      content_type type_ stream_id display_recipient topic recipient_id flags
      unknown =
    {
      id;
      sender_id;
      sender_email;
      sender_full_name;
      timestamp;
      content;
      content_type;
      type_;
      stream_id;
      display_recipient;
      topic;
      recipient_id;
      flags;
      unknown;
    }
  in
  Jsont.Object.map ~kind:"Zulip received message" make
  |> Jsont.Object.mem "id" Id.Message.jsont ~enc:(fun w -> w.id)
  |> Jsont.Object.mem "sender_id" Id.User.jsont ~enc:(fun w -> w.sender_id)
  |> Jsont.Object.mem "sender_email" Jsont.string ~enc:(fun w -> w.sender_email)
  |> Jsont.Object.mem "sender_full_name" Jsont.string ~enc:(fun w ->
      w.sender_full_name)
  |> Jsont.Object.mem "timestamp" timestamp_jsont ~enc:(fun w -> w.timestamp)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun w -> w.content)
  |> Jsont.Object.mem "content_type" Jsont.string ~enc:(fun w -> w.content_type)
  |> Jsont.Object.mem "type" Jsont.string ~enc:(fun w -> w.type_)
  |> Jsont.Object.opt_mem "stream_id" Id.Channel.jsont ~enc:(fun w ->
      w.stream_id)
  |> Jsont.Object.mem "display_recipient" Jsont.json ~enc:(fun w ->
      w.display_recipient)
  |> Jsont.Object.opt_mem "subject" Jsont.string ~enc:(fun w -> w.topic)
  |> Jsont.Object.opt_mem "recipient_id" Id.Recipient.jsont ~enc:(fun w ->
      w.recipient_id)
  |> Jsont.Object.mem "flags"
       (Jsont.list Message_flag.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun w -> w.flags)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun w -> w.unknown)
  |> Jsont.Object.finish

let jsont =
  Jsont.map ~kind:"Zulip received message" ~dec:of_wire ~enc:wire_of_message
    wire_jsont

let pp ppf (t : t) =
  Format.fprintf ppf "Message{id=%a; sender=%a; destination=%s}" Id.Message.pp
    t.id Id.User.pp t.sender_id
    (match t.destination with Channel _ -> "channel" | Direct _ -> "direct")

let create ~id ~sender_id ~sender_email ~sender_full_name ~timestamp ~content
    ~destination ?(content_type = "text/x-markdown") ?(flags = []) () =
  let candidate =
    {
      id;
      sender_id;
      sender_email;
      sender_full_name;
      timestamp;
      content;
      content_type;
      destination;
      flags;
      raw = Jsont.Json.object' [];
    }
  in
  Result.bind
    (Jsont.Json.encode' wire_jsont (wire_of_message candidate))
    (Jsont.Json.decode' jsont)
