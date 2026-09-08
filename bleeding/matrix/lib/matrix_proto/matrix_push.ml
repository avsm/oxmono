open Matrix_json

let json_null = Jsont.Json.null ()
let jstring s = Jsont.Json.string s
let jbool b = Jsont.Json.bool b

let jobject mems =
  Jsont.Json.object'
    (List.map (fun (n, v) -> Jsont.Json.mem (Jsont.Json.name n) v) mems)

module Kind = struct
  type t = Override | Content | Room | Sender | Underride

  let all = [ Override; Content; Room; Sender; Underride ]

  let to_string = function
    | Override -> "override"
    | Content -> "content"
    | Room -> "room"
    | Sender -> "sender"
    | Underride -> "underride"

  let of_string = function
    | "override" -> Ok Override
    | "content" -> Ok Content
    | "room" -> Ok Room
    | "sender" -> Ok Sender
    | "underride" -> Ok Underride
    | s -> Error (`Msg (Printf.sprintf "unknown push rule kind %S" s))

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"push rule kind" (List.map (fun k -> (to_string k, k)) all)
end

module Rule_id = struct
  type t = { kind : Kind.t; id : string }

  let v ~kind id = { kind; id }
  let override id = { kind = Kind.Override; id }
  let content id = { kind = Kind.Content; id }
  let room id = { kind = Kind.Room; id = Matrix_id.Room_id.to_string id }
  let sender id = { kind = Kind.Sender; id = Matrix_id.User_id.to_string id }
  let underride id = { kind = Kind.Underride; id }
  let kind t = t.kind
  let id t = t.id
  let equal a b = Kind.equal a.kind b.kind && String.equal a.id b.id
  let pp ppf t = Format.fprintf ppf "%a/%s" Kind.pp t.kind t.id
end

module Tweak = struct
  type t = Sound of string | Highlight of bool | Custom of string * Jsont.json

  let name = function
    | Sound _ -> "sound"
    | Highlight _ -> "highlight"
    | Custom (n, _) -> n

  let equal a b =
    match (a, b) with
    | Sound x, Sound y -> String.equal x y
    | Highlight x, Highlight y -> Bool.equal x y
    | Custom (n, x), Custom (m, y) -> String.equal n m && Jsont.Json.equal x y
    | _ -> false

  let pp ppf = function
    | Sound s -> Format.fprintf ppf "sound=%S" s
    | Highlight b -> Format.fprintf ppf "highlight=%B" b
    | Custom (n, v) -> Format.fprintf ppf "%s=%a" n Jsont.pp_json v

  (* The value member is absent for a tweak whose presence is the whole of
     its meaning, which is how [highlight] is usually written. *)
  let of_json ~name value =
    match (name, value) with
    | "sound", Some (Jsont.String (s, _)) -> Sound s
    | "highlight", None -> Highlight true
    | "highlight", Some (Jsont.Bool (b, _)) -> Highlight b
    | _, None -> Custom (name, json_null)
    | _, Some v -> Custom (name, v)

  let to_json t =
    let name = name t in
    match t with
    | Sound s -> jobject [ ("set_tweak", jstring name); ("value", jstring s) ]
    | Highlight true -> jobject [ ("set_tweak", jstring name) ]
    | Highlight false ->
        jobject [ ("set_tweak", jstring name); ("value", jbool false) ]
    | Custom (_, Jsont.Null _) -> jobject [ ("set_tweak", jstring name) ]
    | Custom (_, v) -> jobject [ ("set_tweak", jstring name); ("value", v) ]
end

module Action = struct
  type t = Notify | Dont_notify | Coalesce | Set_tweak of Tweak.t

  let equal a b =
    match (a, b) with
    | Notify, Notify | Dont_notify, Dont_notify | Coalesce, Coalesce -> true
    | Set_tweak x, Set_tweak y -> Tweak.equal x y
    | _ -> false

  let of_json j =
    match as_string j with
    | Some "notify" -> Notify
    | Some "coalesce" -> Coalesce
    | Some _ -> Dont_notify
    | None -> (
        match find_string "set_tweak" j with
        | None -> Dont_notify
        | Some name -> Set_tweak (Tweak.of_json ~name (find_mem "value" j)))

  let to_json = function
    | Notify -> jstring "notify"
    | Dont_notify -> jstring "dont_notify"
    | Coalesce -> jstring "coalesce"
    | Set_tweak tweak -> Tweak.to_json tweak

  let pp ppf t = Jsont.pp_json ppf (to_json t)

  let jsont =
    Jsont.map ~kind:"push action" ~dec:of_json ~enc:to_json
      Matrix_json.Codec.json
end

module Condition = struct
  module Comparison = struct
    type t = Eq | Lt | Gt | Le | Ge

    let to_string = function
      | Eq -> "=="
      | Lt -> "<"
      | Gt -> ">"
      | Le -> "<="
      | Ge -> ">="

    let of_string = function
      | "==" | "" -> Ok Eq
      | "<" -> Ok Lt
      | ">" -> Ok Gt
      | "<=" -> Ok Le
      | ">=" -> Ok Ge
      | s -> Error (`Msg (Printf.sprintf "unknown comparison %S" s))

    let equal a b = a = b
    let pp ppf t = Format.pp_print_string ppf (to_string t)
  end

  type t =
    | Event_match of { key : string; pattern : string }
    | Event_property_is of { key : string; value : Jsont.json }
    | Event_property_contains of { key : string; value : Jsont.json }
    | Contains_display_name
    | Room_member_count of { comparison : Comparison.t; count : int }
    | Sender_notification_permission of { key : string }
    | Other of Jsont.json

  let equal a b =
    match (a, b) with
    | Event_match x, Event_match y ->
        String.equal x.key y.key && String.equal x.pattern y.pattern
    | Event_property_is x, Event_property_is y ->
        String.equal x.key y.key && Jsont.Json.equal x.value y.value
    | Event_property_contains x, Event_property_contains y ->
        String.equal x.key y.key && Jsont.Json.equal x.value y.value
    | Contains_display_name, Contains_display_name -> true
    | Room_member_count x, Room_member_count y ->
        Comparison.equal x.comparison y.comparison && x.count = y.count
    | Sender_notification_permission x, Sender_notification_permission y ->
        String.equal x.key y.key
    | Other x, Other y -> Jsont.Json.equal x y
    | _ -> false

  let string_or name j = Option.value (find_string name j) ~default:""
  let json_or name j = Option.value (find_mem name j) ~default:json_null

  (* [is] is an operator followed by a count, and a bare count means [==]. *)
  let member_count_of_string is =
    let is = String.trim is in
    let split n = (String.sub is 0 n, String.sub is n (String.length is - n)) in
    let op, digits =
      match is with
      | _
        when String.length is >= 2
             && List.mem (String.sub is 0 2) [ "=="; "<="; ">=" ] ->
          split 2
      | _ when String.length is >= 1 && (is.[0] = '<' || is.[0] = '>') ->
          split 1
      | _ -> ("", is)
    in
    match (Comparison.of_string op, int_of_string_opt (String.trim digits)) with
    | Ok comparison, Some count -> Some (comparison, count)
    | _ -> None

  let of_json j =
    match find_string "kind" j with
    | Some "event_match" ->
        Event_match { key = string_or "key" j; pattern = string_or "pattern" j }
    | Some "event_property_is" ->
        Event_property_is { key = string_or "key" j; value = json_or "value" j }
    | Some "event_property_contains" ->
        Event_property_contains
          { key = string_or "key" j; value = json_or "value" j }
    | Some "contains_display_name" -> Contains_display_name
    | Some "room_member_count" -> (
        match member_count_of_string (string_or "is" j) with
        | Some (comparison, count) -> Room_member_count { comparison; count }
        | None -> Other j)
    | Some "sender_notification_permission" ->
        Sender_notification_permission { key = string_or "key" j }
    | Some _ | None -> Other j

  let to_json = function
    | Event_match { key; pattern } ->
        jobject
          [
            ("kind", jstring "event_match");
            ("key", jstring key);
            ("pattern", jstring pattern);
          ]
    | Event_property_is { key; value } ->
        jobject
          [
            ("kind", jstring "event_property_is");
            ("key", jstring key);
            ("value", value);
          ]
    | Event_property_contains { key; value } ->
        jobject
          [
            ("kind", jstring "event_property_contains");
            ("key", jstring key);
            ("value", value);
          ]
    | Contains_display_name ->
        jobject [ ("kind", jstring "contains_display_name") ]
    | Room_member_count { comparison; count } ->
        jobject
          [
            ("kind", jstring "room_member_count");
            ( "is",
              jstring (Comparison.to_string comparison ^ string_of_int count) );
          ]
    | Sender_notification_permission { key } ->
        jobject
          [
            ("kind", jstring "sender_notification_permission");
            ("key", jstring key);
          ]
    | Other j -> j

  let pp ppf t = Jsont.pp_json ppf (to_json t)

  let jsont =
    Jsont.map ~kind:"push condition" ~dec:of_json ~enc:to_json
      Matrix_json.Codec.json
end

module Rule = struct
  type t = {
    rule_id : Rule_id.t;
    default : bool;
    enabled : bool;
    actions : Action.t list;
    conditions : Condition.t list;
    pattern : string option;
  }

  let v ?(default = false) ?(enabled = true) ?(conditions = []) ?pattern
      ~rule_id actions =
    { rule_id; default; enabled; actions; conditions; pattern }

  let kind t = Rule_id.kind t.rule_id

  let equal a b =
    Rule_id.equal a.rule_id b.rule_id
    && Bool.equal a.default b.default
    && Bool.equal a.enabled b.enabled
    && List.equal Action.equal a.actions b.actions
    && List.equal Condition.equal a.conditions b.conditions
    && Option.equal String.equal a.pattern b.pattern

  let jsont kind =
    Jsont.Object.(
      map ~kind:"push rule"
        (fun rule_id default enabled actions conditions pattern ->
          {
            rule_id = Rule_id.v ~kind rule_id;
            default;
            enabled;
            actions;
            conditions;
            pattern;
          })
      |> mem "rule_id" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> Rule_id.id t.rule_id)
      |> mem "default" Jsont.bool
           ~dec_absent:(fun () -> false)
           ~enc:(fun t -> t.default)
      |> mem "enabled" Jsont.bool
           ~dec_absent:(fun () -> true)
           ~enc:(fun t -> t.enabled)
      |> mem "actions" (Jsont.list Action.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.actions)
      |> mem "conditions"
           (Jsont.list Condition.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.conditions)
      |> opt_mem "pattern" Matrix_json.Codec.string ~enc:(fun t -> t.pattern)
      |> finish)

  let pp ppf t = Jsont.pp_value (jsont (kind t)) () ppf t
end

module Ruleset = struct
  type t = {
    override : Rule.t list;
    content : Rule.t list;
    room : Rule.t list;
    sender : Rule.t list;
    underride : Rule.t list;
  }

  let empty =
    { override = []; content = []; room = []; sender = []; underride = [] }

  let rules t = function
    | Kind.Override -> t.override
    | Kind.Content -> t.content
    | Kind.Room -> t.room
    | Kind.Sender -> t.sender
    | Kind.Underride -> t.underride

  let equal a b =
    List.for_all
      (fun k -> List.equal Rule.equal (rules a k) (rules b k))
      Kind.all

  let jsont =
    let list_of kind = Jsont.list (Rule.jsont kind) in
    Jsont.Object.(
      map ~kind:"push ruleset" (fun override content room sender underride ->
          { override; content; room; sender; underride })
      |> mem "override" (list_of Kind.Override)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.override)
      |> mem "content" (list_of Kind.Content)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.content)
      |> mem "room" (list_of Kind.Room)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.room)
      |> mem "sender" (list_of Kind.Sender)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.sender)
      |> mem "underride" (list_of Kind.Underride)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.underride)
      |> finish)

  let global_jsont =
    Jsont.Object.(
      map ~kind:"push rules" Fun.id |> mem "global" jsont ~enc:Fun.id |> finish)

  let pp ppf t = Jsont.pp_value jsont () ppf t
end

let sound_default = Action.Set_tweak (Tweak.Sound "default")
let sound_ring = Action.Set_tweak (Tweak.Sound "ring")
let highlight = Action.Set_tweak (Tweak.Highlight true)
let event_match key pattern = Condition.Event_match { key; pattern }

let default_ruleset ~user_id =
  let uid = Matrix_id.User_id.to_string user_id in
  let rule ?enabled ~rule_id ~conditions actions =
    Rule.v ~default:true ?enabled ~rule_id:(Rule_id.override rule_id)
      ~conditions actions
  in
  let override =
    [
      rule ~enabled:false ~rule_id:".m.rule.master" ~conditions:[] [];
      rule ~rule_id:".m.rule.suppress_notices"
        ~conditions:[ event_match "content.msgtype" "m.notice" ]
        [];
      rule ~rule_id:".m.rule.invite_for_me"
        ~conditions:
          [
            event_match "type" "m.room.member";
            event_match "content.membership" "invite";
            event_match "state_key" uid;
          ]
        [ Action.Notify; sound_default ];
      rule ~rule_id:".m.rule.member_event"
        ~conditions:[ event_match "type" "m.room.member" ]
        [];
      rule ~rule_id:".m.rule.is_user_mention"
        ~conditions:
          [
            Condition.Event_property_contains
              { key = {|content.m\.mentions.user_ids|}; value = jstring uid };
          ]
        [ Action.Notify; sound_default; highlight ];
      rule ~rule_id:".m.rule.is_room_mention"
        ~conditions:
          [
            Condition.Event_property_is
              { key = {|content.m\.mentions.room|}; value = jbool true };
            Condition.Sender_notification_permission { key = "room" };
          ]
        [ Action.Notify; highlight ];
      rule ~rule_id:".m.rule.tombstone"
        ~conditions:
          [ event_match "type" "m.room.tombstone"; event_match "state_key" "" ]
        [ Action.Notify; highlight ];
      rule ~rule_id:".m.rule.reaction"
        ~conditions:[ event_match "type" "m.reaction" ]
        [];
      rule ~rule_id:".m.rule.room.server_acl"
        ~conditions:
          [ event_match "type" "m.room.server_acl"; event_match "state_key" "" ]
        [];
      rule ~rule_id:".m.rule.suppress_edits"
        ~conditions:
          [
            Condition.Event_property_is
              {
                key = {|content.m\.relates_to.rel_type|};
                value = jstring "m.replace";
              };
          ]
        [];
    ]
  in
  let rule ~rule_id ~conditions actions =
    Rule.v ~default:true
      ~rule_id:(Rule_id.underride rule_id)
      ~conditions actions
  in
  let underride =
    [
      rule ~rule_id:".m.rule.call"
        ~conditions:[ event_match "type" "m.call.invite" ]
        [ Action.Notify; sound_ring ];
      rule ~rule_id:".m.rule.encrypted_room_one_to_one"
        ~conditions:
          [
            Condition.Room_member_count
              { comparison = Condition.Comparison.Eq; count = 2 };
            event_match "type" "m.room.encrypted";
          ]
        [ Action.Notify; sound_default ];
      rule ~rule_id:".m.rule.room_one_to_one"
        ~conditions:
          [
            Condition.Room_member_count
              { comparison = Condition.Comparison.Eq; count = 2 };
            event_match "type" "m.room.message";
          ]
        [ Action.Notify; sound_default ];
      rule ~rule_id:".m.rule.message"
        ~conditions:[ event_match "type" "m.room.message" ]
        [ Action.Notify ];
      rule ~rule_id:".m.rule.encrypted"
        ~conditions:[ event_match "type" "m.room.encrypted" ]
        [ Action.Notify ];
    ]
  in
  { Ruleset.empty with override; underride }
