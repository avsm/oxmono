module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Push = Matrix_proto.Push
open Matrix_proto.Json

module Power_levels = struct
  type t = {
    users : (Id.User_id.t * int) list;
    users_default : int;
    notifications : (string * int) list;
  }

  let default = { users = []; users_default = 0; notifications = [] }

  let int_map j =
    match as_object j with
    | None -> []
    | Some o ->
        List.filter_map
          (fun ((name, _), v) -> Option.map (fun i -> (name, i)) (as_int v))
          o

  let of_json content =
    let member name =
      match find_mem name content with Some j -> int_map j | None -> []
    in
    {
      users =
        List.filter_map
          (fun (name, level) ->
            match Id.User_id.of_string name with
            | Ok id -> Some (id, level)
            | Error _ -> None)
          (member "users");
      users_default =
        Option.value
          (Option.bind (find_mem "users_default" content) as_int)
          ~default:default.users_default;
      notifications = member "notifications";
    }

  let of_user t user_id =
    match
      List.find_opt (fun (id, _) -> Id.User_id.equal id user_id) t.users
    with
    | Some (_, level) -> level
    | None -> t.users_default

  (* The spec's default for a notification nobody has set a level for. *)
  let notification_required t key =
    Option.value (List.assoc_opt key t.notifications) ~default:50
end

module Context = struct
  type t = {
    user_id : Id.User_id.t;
    display_name : string;
    room_id : Id.Room_id.t;
    member_count : int;
    power_levels : Power_levels.t option;
  }

  let v ~user_id ~room_id ?display_name ?(member_count = 0) ?power_levels () =
    let display_name =
      match display_name with
      | Some n -> n
      | None -> Id.User_id.localpart user_id
    in
    { user_id; display_name; room_id; member_count; power_levels }

  let user_id t = t.user_id
  let room_id t = t.room_id
  let display_name t = t.display_name
  let member_count t = t.member_count
  let power_levels t = t.power_levels
end

let escape_name name =
  let b = Buffer.create (String.length name) in
  String.iter
    (fun c ->
      (match c with '\\' | '.' -> Buffer.add_char b '\\' | _ -> ());
      Buffer.add_char b c)
    name;
  Buffer.contents b

let rec flatten_into prefix j acc =
  match j with
  | Jsont.Object ([], _) -> acc
  | Jsont.Object (mems, _) ->
      List.fold_left
        (fun acc ((name, _), v) ->
          let path =
            if prefix = "" then escape_name name
            else prefix ^ "." ^ escape_name name
          in
          flatten_into path v acc)
        acc mems
  | v -> if prefix = "" then acc else (prefix, v) :: acc

(* The event as the dot-separated property paths a condition key addresses. A
   [.] inside a member name is escaped, as the appendix requires. *)
let flatten (e : Event.Raw_event.t) =
  let base =
    [
      ("type", Jsont.Json.string (Event.Event_type.to_string e.type_));
      ("sender", Jsont.Json.string (Id.User_id.to_string e.sender));
    ]
  in
  let base =
    match e.state_key with
    | Some k -> base @ [ ("state_key", Jsont.Json.string k) ]
    | None -> base
  in
  let base =
    match e.room_id with
    | Some r ->
        base @ [ ("room_id", Jsont.Json.string (Id.Room_id.to_string r)) ]
    | None -> base
  in
  base @ List.rev (flatten_into "content" e.content [])

let lookup flat key = List.assoc_opt key flat
let lookup_string flat key = Option.bind (lookup flat key) as_string

let member_count_applies ~count comparison n =
  match (comparison : Push.Condition.Comparison.t) with
  | Eq -> count = n
  | Lt -> count < n
  | Gt -> count > n
  | Le -> count <= n
  | Ge -> count >= n

let sender_permission ~sender ~key ctx =
  match Context.power_levels ctx with
  | None -> false
  | Some pl ->
      Power_levels.of_user pl sender
      >= Power_levels.notification_required pl key

let condition_applies ~sender ctx flat = function
  | Push.Condition.Event_match { key; pattern } -> (
      let value =
        if String.equal key "room_id" then
          Some (Id.Room_id.to_string (Context.room_id ctx))
        else lookup_string flat key
      in
      match value with
      | None -> false
      | Some v ->
          if String.equal key "content.body" then
            Matrix_glob.word_boundary ~pattern v
          else Matrix_glob.whole_string ~pattern v)
  | Push.Condition.Event_property_is { key; value } -> (
      match lookup flat key with
      | None -> false
      | Some v -> Jsont.Json.equal v value)
  | Push.Condition.Event_property_contains { key; value } -> (
      match Option.bind (lookup flat key) as_array with
      | None -> false
      | Some items -> List.exists (fun i -> Jsont.Json.equal i value) items)
  | Push.Condition.Contains_display_name -> (
      match lookup_string flat "content.body" with
      | None -> false
      | Some body ->
          Matrix_glob.word_boundary ~pattern:(Context.display_name ctx) body)
  | Push.Condition.Room_member_count { comparison; count } ->
      member_count_applies ~count:(Context.member_count ctx) comparison count
  | Push.Condition.Sender_notification_permission { key } ->
      sender_permission ~sender ~key ctx
  | Push.Condition.Other _ -> false

let rule_applies ~sender ctx flat kind (rule : Push.Rule.t) =
  if not rule.enabled then false
  else
    match (kind : Push.Kind.t) with
    | Override | Underride ->
        List.for_all (condition_applies ~sender ctx flat) rule.conditions
    | Content -> (
        match rule.pattern with
        | None | Some "" -> false
        | Some pattern ->
            condition_applies ~sender ctx flat
              (Push.Condition.Event_match { key = "content.body"; pattern }))
    | Room ->
        String.equal
          (Push.Rule_id.id rule.rule_id)
          (Id.Room_id.to_string (Context.room_id ctx))
    | Sender ->
        String.equal
          (Push.Rule_id.id rule.rule_id)
          (Id.User_id.to_string sender)

let find_matching_rule rules ctx (e : Event.Raw_event.t) =
  (* An event the user sent themselves matches no rule at all, so it can
     never raise their own unread count. *)
  if Id.User_id.equal e.sender (Context.user_id ctx) then None
  else
    let flat = flatten e in
    List.find_map
      (fun kind ->
        List.find_opt
          (rule_applies ~sender:e.sender ctx flat kind)
          (Push.Ruleset.rules rules kind))
      Push.Kind.all

let evaluate rules ctx e =
  match find_matching_rule rules ctx e with
  | None -> []
  | Some r -> r.Push.Rule.actions

type notification = { notify : bool; highlight : bool; sound : string option }

let no_notification = { notify = false; highlight = false; sound = None }

let notification_of_actions actions =
  List.fold_left
    (fun acc -> function
      | Push.Action.Notify | Push.Action.Coalesce -> { acc with notify = true }
      | Push.Action.Dont_notify -> acc
      | Push.Action.Set_tweak (Push.Tweak.Highlight highlight) ->
          { acc with highlight }
      | Push.Action.Set_tweak (Push.Tweak.Sound "") -> { acc with sound = None }
      | Push.Action.Set_tweak (Push.Tweak.Sound s) ->
          { acc with sound = Some s }
      | Push.Action.Set_tweak (Push.Tweak.Custom _) -> acc)
    no_notification actions

let notification_for_event rules ctx e =
  notification_of_actions (evaluate rules ctx e)
