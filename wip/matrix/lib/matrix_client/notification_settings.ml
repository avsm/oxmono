module Proto = Matrix_proto.Push
module Id = Matrix_proto.Id

type room_notification_mode = All_messages | Mentions_and_keywords_only | Mute
type subscription = int

type t = {
  client : Client.t;
  mutable ruleset : Proto.Ruleset.t;
  mutable next_subscription : int;
  subscribers : (subscription, Proto.Ruleset.t -> unit) Hashtbl.t;
}

let create ?(ruleset = Proto.Ruleset.empty) client =
  { client; ruleset; next_subscription = 0; subscribers = Hashtbl.create 4 }

let client t = t.client
let ruleset t = t.ruleset

let publish t ruleset =
  t.ruleset <- ruleset;
  (* Take a snapshot so that a callback may unsubscribe itself safely. *)
  Hashtbl.to_seq_values t.subscribers
  |> List.of_seq
  |> List.iter (fun callback -> callback ruleset)

let subscribe t callback =
  let subscription = t.next_subscription in
  t.next_subscription <- subscription + 1;
  Hashtbl.replace t.subscribers subscription callback;
  subscription

let unsubscribe t subscription = Hashtbl.remove t.subscribers subscription

let refresh t =
  match Push.get_push_rules t.client with
  | Error _ as error -> error
  | Ok ruleset ->
      publish t ruleset;
      Ok ()

let room_string = Id.Room_id.to_string
let rule_id_string (rule : Proto.Rule.t) = Proto.Rule_id.id rule.rule_id
let rule_kind (rule : Proto.Rule.t) = Proto.Rule.kind rule
let is_custom (rule : Proto.Rule.t) = not rule.default

let has_notify (rule : Proto.Rule.t) =
  List.exists (function Proto.Action.Notify -> true | _ -> false) rule.actions

let has_room_condition room_id (rule : Proto.Rule.t) =
  let room_id = room_string room_id in
  List.exists
    (function
      | Proto.Condition.Event_match { key = "room_id"; pattern } ->
          String.equal pattern room_id
      | _ -> false)
    rule.conditions

let has_room_id room_id (rule : Proto.Rule.t) =
  String.equal (rule_id_string rule) (room_string room_id)

let custom_rules_for_room ruleset room_id =
  let conditional rules =
    List.filter
      (fun rule ->
        is_custom rule
        && (has_room_id room_id rule || has_room_condition room_id rule))
      rules
  in
  let rooms =
    List.filter
      (fun rule -> is_custom rule && has_room_id room_id rule)
      ruleset.Proto.Ruleset.room
  in
  conditional ruleset.override @ rooms @ conditional ruleset.underride

let user_defined_room_mode t room_id =
  let muted =
    List.exists
      (fun rule ->
        is_custom rule && rule.enabled
        && has_room_condition room_id rule
        && not (has_notify rule))
      t.ruleset.override
  in
  if muted then Some Mute
  else
    match
      List.find_opt
        (fun rule -> is_custom rule && rule.enabled && has_room_id room_id rule)
        t.ruleset.room
    with
    | Some rule when has_notify rule -> Some All_messages
    | Some _ -> Some Mentions_and_keywords_only
    | None -> None

let default_rule_id ~encrypted ~one_to_one =
  match (encrypted, one_to_one) with
  | true, true -> ".m.rule.encrypted_room_one_to_one"
  | false, true -> ".m.rule.room_one_to_one"
  | true, false -> ".m.rule.encrypted"
  | false, false -> ".m.rule.message"

let poll_rule_id ~one_to_one =
  if one_to_one then ".m.rule.poll_start_one_to_one" else ".m.rule.poll_start"

let find_rule ruleset rule_id =
  Proto.Ruleset.rules ruleset (Proto.Rule_id.kind rule_id)
  |> List.find_opt (fun (rule : Proto.Rule.t) ->
      Proto.Rule_id.equal rule.rule_id rule_id)

let default_room_mode t ~encrypted ~one_to_one =
  let rule_id =
    Proto.Rule_id.underride (default_rule_id ~encrypted ~one_to_one)
  in
  match find_rule t.ruleset rule_id with
  | Some rule when rule.enabled && has_notify rule -> All_messages
  | Some _ | None -> Mentions_and_keywords_only

let room_mode t room_id ~encrypted ~one_to_one =
  match user_defined_room_mode t room_id with
  | Some mode -> mode
  | None -> default_room_mode t ~encrypted ~one_to_one

let rooms_with_user_defined_rules ?enabled t =
  let seen = Hashtbl.create 16 in
  let answer = ref [] in
  let add room_id =
    if not (Hashtbl.mem seen room_id) then begin
      Hashtbl.add seen room_id ();
      answer := room_id :: !answer
    end
  in
  let selected (rule : Proto.Rule.t) =
    is_custom rule
    && Option.fold ~none:true ~some:(Bool.equal rule.enabled) enabled
  in
  let add_conditions rule =
    if selected rule then
      List.iter
        (function
          | Proto.Condition.Event_match { key = "room_id"; pattern } ->
              add pattern
          | _ -> ())
        rule.Proto.Rule.conditions
  in
  List.iter add_conditions t.ruleset.override;
  List.iter
    (fun rule -> if selected rule then add (rule_id_string rule))
    t.ruleset.room;
  List.iter add_conditions t.ruleset.underride;
  List.rev !answer

let contains_keyword_rules t =
  List.exists (fun rule -> is_custom rule && rule.enabled) t.ruleset.content

let enabled_keywords t =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun rule ->
      match rule.Proto.Rule.pattern with
      | Some keyword
        when is_custom rule && rule.enabled && not (Hashtbl.mem seen keyword) ->
          Hashtbl.add seen keyword ();
          Some keyword
      | Some _ | None -> None)
    t.ruleset.content

let notifying_actions =
  [ Proto.Action.Notify; Proto.Action.Set_tweak (Proto.Tweak.Sound "default") ]

let silent_actions = []

type command =
  | Set_rule of Proto.Rule.t
  | Delete_rule of Proto.Rule_id.t
  | Set_enabled of Proto.Rule_id.t * bool
  | Set_actions of Proto.Rule_id.t * Proto.Action.t list

let replace_kind (ruleset : Proto.Ruleset.t) kind rules =
  match kind with
  | Proto.Kind.Override -> { ruleset with override = rules }
  | Proto.Kind.Content -> { ruleset with content = rules }
  | Proto.Kind.Room -> { ruleset with room = rules }
  | Proto.Kind.Sender -> { ruleset with sender = rules }
  | Proto.Kind.Underride -> { ruleset with underride = rules }

let apply_command ruleset = function
  | Set_rule rule ->
      let kind = rule_kind rule in
      let rules =
        Proto.Ruleset.rules ruleset kind
        |> List.filter (fun (existing : Proto.Rule.t) ->
            not (Proto.Rule_id.equal existing.rule_id rule.rule_id))
      in
      replace_kind ruleset kind (rule :: rules)
  | Delete_rule rule_id ->
      let kind = Proto.Rule_id.kind rule_id in
      let rules =
        Proto.Ruleset.rules ruleset kind
        |> List.filter (fun (rule : Proto.Rule.t) ->
            not (Proto.Rule_id.equal rule.rule_id rule_id))
      in
      replace_kind ruleset kind rules
  | Set_enabled (rule_id, enabled) ->
      let kind = Proto.Rule_id.kind rule_id in
      let rules =
        Proto.Ruleset.rules ruleset kind
        |> List.map (fun (rule : Proto.Rule.t) ->
            if Proto.Rule_id.equal rule.rule_id rule_id then
              { rule with enabled }
            else rule)
      in
      replace_kind ruleset kind rules
  | Set_actions (rule_id, actions) ->
      let kind = Proto.Rule_id.kind rule_id in
      let rules =
        Proto.Ruleset.rules ruleset kind
        |> List.map (fun (rule : Proto.Rule.t) ->
            if Proto.Rule_id.equal rule.rule_id rule_id then
              { rule with actions }
            else rule)
      in
      replace_kind ruleset kind rules

let run_command client = function
  | Set_rule rule ->
      let conditions =
        match rule_kind rule with
        | Proto.Kind.Override | Proto.Kind.Underride -> Some rule.conditions
        | Proto.Kind.Content | Proto.Kind.Room | Proto.Kind.Sender -> None
      in
      let pattern =
        match rule_kind rule with
        | Proto.Kind.Content -> rule.pattern
        | Proto.Kind.Override | Proto.Kind.Room | Proto.Kind.Sender
        | Proto.Kind.Underride ->
            None
      in
      Push.set_push_rule client rule.rule_id ~actions:rule.actions ?conditions
        ?pattern ()
  | Delete_rule rule_id -> Push.delete_push_rule client rule_id
  | Set_enabled (rule_id, enabled) -> Push.set_enabled client rule_id ~enabled
  | Set_actions (rule_id, actions) -> Push.set_actions client rule_id ~actions

let commit t commands =
  let rec run = function
    | [] -> Ok ()
    | command :: rest -> (
        match run_command t.client command with
        | Ok () -> run rest
        | Error _ as error -> error)
  in
  if commands = [] then Ok ()
  else
    match run commands with
    | Error _ as error -> error
    | Ok () ->
        publish t (List.fold_left apply_command t.ruleset commands);
        Ok ()

let set_room_mode t room_id mode =
  if user_defined_room_mode t room_id = Some mode then Ok ()
  else
    let room_id_string = room_string room_id in
    let new_kind, new_rule =
      match mode with
      | All_messages ->
          ( Proto.Kind.Room,
            Proto.Rule.v ~rule_id:(Proto.Rule_id.room room_id) notifying_actions
          )
      | Mentions_and_keywords_only ->
          ( Proto.Kind.Room,
            Proto.Rule.v ~rule_id:(Proto.Rule_id.room room_id) silent_actions )
      | Mute ->
          let rule_id = Proto.Rule_id.override room_id_string in
          let conditions =
            [
              Proto.Condition.Event_match
                { key = "room_id"; pattern = room_id_string };
            ]
          in
          (Proto.Kind.Override, Proto.Rule.v ~rule_id ~conditions silent_actions)
    in
    let old_rules =
      custom_rules_for_room t.ruleset room_id
      |> List.filter (fun rule ->
          not
            (Proto.Kind.equal (rule_kind rule) new_kind
            && String.equal (rule_id_string rule) room_id_string))
    in
    commit t
      (Set_rule new_rule
      :: List.map
           (fun (rule : Proto.Rule.t) -> Delete_rule rule.rule_id)
           old_rules)

let delete_room_mode t room_id =
  custom_rules_for_room t.ruleset room_id
  |> List.map (fun (rule : Proto.Rule.t) -> Delete_rule rule.rule_id)
  |> commit t

let unmute_room t room_id ~encrypted ~one_to_one =
  match user_defined_room_mode t room_id with
  | Some (All_messages | Mentions_and_keywords_only) -> Ok ()
  | Some Mute -> (
      match default_room_mode t ~encrypted ~one_to_one with
      | Mute -> set_room_mode t room_id All_messages
      | All_messages | Mentions_and_keywords_only -> delete_room_mode t room_id)
  | None -> set_room_mode t room_id All_messages

let actions_equal = List.equal Proto.Action.equal

let commands_for_actions_and_enable ruleset rule_id actions =
  match find_rule ruleset rule_id with
  | None -> None
  | Some rule ->
      let commands =
        if actions_equal rule.actions actions then []
        else [ Set_actions (rule_id, actions) ]
      in
      let commands =
        if rule.enabled then commands
        else commands @ [ Set_enabled (rule_id, true) ]
      in
      Some commands

let set_default_room_mode t ~encrypted ~one_to_one mode =
  let actions =
    match mode with
    | All_messages -> notifying_actions
    | Mentions_and_keywords_only | Mute -> []
  in
  let room_rule =
    Proto.Rule_id.underride (default_rule_id ~encrypted ~one_to_one)
  in
  match commands_for_actions_and_enable t.ruleset room_rule actions with
  | None ->
      Error
        (Error.Json_error
           (Printf.sprintf "push rule %s was not found"
              (Proto.Rule_id.id room_rule)))
  | Some room_commands ->
      let poll_rule = Proto.Rule_id.underride (poll_rule_id ~one_to_one) in
      let poll_commands =
        Option.value
          (commands_for_actions_and_enable t.ruleset poll_rule actions)
          ~default:[]
      in
      commit t (room_commands @ poll_commands)

let keyword_rules t keyword =
  List.filter
    (fun rule ->
      is_custom rule
      && Option.equal String.equal rule.Proto.Rule.pattern (Some keyword))
    t.ruleset.content

let add_keyword t keyword =
  match keyword_rules t keyword with
  | [] ->
      let rule =
        Proto.Rule.v
          ~rule_id:(Proto.Rule_id.content keyword)
          ~pattern:keyword notifying_actions
      in
      commit t [ Set_rule rule ]
  | rules when List.exists (fun rule -> rule.Proto.Rule.enabled) rules -> Ok ()
  | (rule : Proto.Rule.t) :: _ -> commit t [ Set_enabled (rule.rule_id, true) ]

let remove_keyword t keyword =
  keyword_rules t keyword
  |> List.map (fun (rule : Proto.Rule.t) -> Delete_rule rule.rule_id)
  |> commit t

let rule_not_found rule_id =
  Error
    (Error.Json_error
       (Format.asprintf "push rule %a was not found" Proto.Rule_id.pp rule_id))

let legacy_rules rule_id =
  match (Proto.Rule_id.kind rule_id, Proto.Rule_id.id rule_id) with
  | Proto.Kind.Override, ".m.rule.is_user_mention" ->
      [
        Proto.Rule_id.content ".m.rule.contains_user_name";
        Proto.Rule_id.override ".m.rule.contains_display_name";
      ]
  | Proto.Kind.Override, ".m.rule.is_room_mention" ->
      [ Proto.Rule_id.override ".m.rule.roomnotif" ]
  | _ -> []

let is_enabled t rule_id =
  match find_rule t.ruleset rule_id with
  | Some rule -> Ok rule.enabled
  | None ->
      let legacy = legacy_rules rule_id in
      if legacy = [] then rule_not_found rule_id
      else
        Ok
          (List.exists
             (fun id ->
               match find_rule t.ruleset id with
               | Some rule -> rule.enabled && has_notify rule
               | None -> false)
             legacy)

let set_enabled t rule_id ~enabled =
  match find_rule t.ruleset rule_id with
  | None -> rule_not_found rule_id
  | Some _ ->
      let ids =
        rule_id
        :: List.filter
             (fun id -> Option.is_some (find_rule t.ruleset id))
             (legacy_rules rule_id)
      in
      let commands =
        List.filter_map
          (fun id ->
            match find_rule t.ruleset id with
            | Some rule when Bool.equal rule.enabled enabled -> None
            | Some _ -> Some (Set_enabled (id, enabled))
            | None -> None)
          ids
      in
      commit t commands

let set_actions t rule_id ~actions =
  match find_rule t.ruleset rule_id with
  | None -> rule_not_found rule_id
  | Some rule when actions_equal rule.actions actions -> Ok ()
  | Some _ -> commit t [ Set_actions (rule_id, actions) ]
