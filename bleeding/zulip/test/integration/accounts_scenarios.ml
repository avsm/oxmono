open Zulip_eio

let fail error = Alcotest.fail (Error.error_to_string error)
let ok = function Ok value -> value | Error error -> fail error
let check condition message = Alcotest.(check bool) message true condition

let nonce () =
  let micros = mod_float (Unix.gettimeofday () *. 1_000_000.) 16_777_216. in
  Printf.sprintf "%x-%06x" (Unix.getpid ()) (int_of_float micros)

let choice_data label =
  Jsont.Json.object'
    [
      ( ("ocaml", Jsont.Meta.none),
        Jsont.Json.object'
          [
            (("text", Jsont.Meta.none), Jsont.Json.string label);
            (("order", Jsont.Meta.none), Jsont.Json.string "1");
          ] );
    ]

let find_profile_field id fields =
  List.find_opt (fun (field : Server.profile_field) -> field.id = id) fields

let find_linkifier id linkifiers =
  List.find_opt
    (fun (linkifier : Server.linkifier) -> linkifier.id = id)
    linkifiers

let sorted_profile_ids fields =
  fields
  |> List.sort (fun (a : Server.profile_field) b -> Int.compare a.order b.order)
  |> List.map (fun (field : Server.profile_field) -> field.id)

let snapshot client =
  let queue = Event_queue.register client () |> ok in
  Fun.protect
    ~finally:(fun () -> ignore (Event_queue.delete queue client))
    (fun () -> Event_queue.state queue)

let restore_status client ~user (status : Users.user_status) =
  let text = Option.value ~default:"" status.status_text in
  let emoji =
    match status.emoji with
    | None -> Users.Clear_emoji
    | Some emoji -> Users.Set_emoji emoji
  in
  ignore (Users.update_status_for_user client ~user_id:user ~text ~emoji ())

let run ~client ~user =
  let profile_field_id = ref None in
  let linkifier_id = ref None in
  let original_time_setting = ref None in
  let original_status = ref None in
  let cleanup () =
    Option.iter
      (fun id -> ignore (Server.delete_linkifier client ~filter_id:id))
      !linkifier_id;
    Option.iter
      (fun id -> ignore (Server.delete_profile_field client ~field_id:id))
      !profile_field_id;
    Option.iter
      (fun value ->
        ignore
          (Settings.update client
             [ Settings.Set (Settings.Twenty_four_hour_time, value) ]))
      !original_time_setting;
    Option.iter (restore_status client ~user) !original_status
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let me = Users.me client |> ok in
  check (Zulip.User.is_admin me) "account scenario requires an administrator";
  check (not (Zulip.User.is_bot me)) "account scenario requires a human user";
  let target =
    Users.get_by_id client ~user_id:user ~include_custom_profile_fields:true ()
    |> ok
  in
  check
    (not (Zulip.User.is_bot target))
    "account scenario status target must be a human user";

  let marker = nonce () in
  let profile_name = "OCaml parity " ^ marker in
  let original_fields = Server.get_profile_fields client |> ok in
  let original_profile_order = sorted_profile_ids original_fields in
  let field_id =
    Server.create_profile_field client ~field_type:Server.Choice
      ~name:profile_name ~hint:"OCaml parity integration field"
      ~field_data:(choice_data "OCaml") ~display_in_profile_summary:false
      ~required:false ~editable_by_user:true ~use_for_user_matching:false ()
    |> ok
  in
  profile_field_id := Some field_id;
  let created =
    match
      find_profile_field field_id (Server.get_profile_fields client |> ok)
    with
    | Some field -> field
    | None -> Alcotest.fail "created profile field was not listed"
  in
  Alcotest.(check string) "created profile field name" profile_name created.name;
  Alcotest.(check string)
    "created profile field hint" "OCaml parity integration field" created.hint;
  check (created.field_data <> "") "created profile field lost field_data";

  let updated_name = profile_name ^ " updated" in
  Server.update_profile_field client ~field_id ~name:updated_name
    ~hint:"Updated OCaml parity field" ~field_data:(choice_data "OCaml 5")
    ~display_in_profile_summary:false ~required:false ~editable_by_user:false
    ~use_for_user_matching:false ()
  |> ok;
  let updated =
    match
      find_profile_field field_id (Server.get_profile_fields client |> ok)
    with
    | Some field -> field
    | None -> Alcotest.fail "updated profile field was not listed"
  in
  Alcotest.(check string) "updated profile field name" updated_name updated.name;
  Alcotest.(check string)
    "updated profile field hint" "Updated OCaml parity field" updated.hint;
  Alcotest.(check bool)
    "updated profile field editability" false updated.editable_by_user;

  Server.reorder_profile_fields client
    ~order:(field_id :: original_profile_order)
  |> ok;
  (match sorted_profile_ids (Server.get_profile_fields client |> ok) with
  | first :: _ ->
      Alcotest.(check int)
        "profile field reorder"
        (Zulip.Id.Profile_field.to_int field_id)
        (Zulip.Id.Profile_field.to_int first)
  | [] -> Alcotest.fail "profile fields disappeared after reorder");
  Server.reorder_profile_fields client
    ~order:(original_profile_order @ [ field_id ])
  |> ok;
  Server.delete_profile_field client ~field_id |> ok;
  profile_field_id := None;
  check
    (Option.is_none
       (find_profile_field field_id (Server.get_profile_fields client |> ok)))
    "deleted profile field was still listed";

  let initial_state = snapshot client in
  let current_time_setting =
    match Settings.get initial_state Settings.Twenty_four_hour_time |> ok with
    | Some value -> value
    | None -> Alcotest.fail "registration snapshot omitted personal settings"
  in
  original_time_setting := Some current_time_setting;
  let toggled_time_setting = not current_time_setting in
  let settings_result =
    Settings.update client
      [ Settings.Set (Settings.Twenty_four_hour_time, toggled_time_setting) ]
    |> ok
  in
  Alcotest.(check (list string))
    "supported setting was not ignored" []
    settings_result.ignored_parameters_unsupported;
  let updated_state = snapshot client in
  Alcotest.(check (option bool))
    "typed setting snapshot" (Some toggled_time_setting)
    (Settings.get updated_state Settings.Twenty_four_hour_time |> ok);

  let old_status = Users.get_status client ~user_id:user |> ok in
  original_status := Some old_status;
  let status_text = "OCaml parity " ^ marker in
  let emoji : Users.status_emoji =
    {
      emoji_name = "wave";
      emoji_code = "1f44b";
      reaction_type = Users.Unicode_emoji;
    }
  in
  Users.update_status_for_user client ~user_id:user ~text:status_text
    ~emoji:(Users.Set_emoji emoji) ()
  |> ok;
  let status = Users.get_status client ~user_id:user |> ok in
  Alcotest.(check (option string))
    "typed status text" (Some status_text) status.status_text;
  (match status.emoji with
  | Some returned ->
      Alcotest.(check string) "typed status emoji" "1f44b" returned.emoji_code
  | None -> Alcotest.fail "status response omitted the updated emoji");

  let cursor =
    match Initial_state.presence_last_update_id updated_state |> ok with
    | Some cursor -> cursor
    | None -> Alcotest.fail "registration snapshot omitted presence cursor"
  in
  let delta =
    Presence.update client ~status:Presence.Active ~last_update_id:cursor
      ~history_limit_days:1 ~ping_only:false ~new_user_input:true
      ~slim_presence:true ()
    |> ok
  in
  (match delta.presence_last_update_id with
  | Some next -> check (next >= cursor) "presence cursor moved backwards"
  | None -> Alcotest.fail "presence delta omitted its cursor");
  check
    (Option.is_some delta.server_timestamp)
    "presence delta omitted server timestamp";
  check (Option.is_some delta.presences) "presence delta omitted presences";

  let linkifier_pattern = Printf.sprintf "OCAML%s-(?P<id>[0-9]+)" marker in
  let example_input = Printf.sprintf "OCAML%s-42" marker in
  let reverse_template = Printf.sprintf "OCAML%s-{id}" marker in
  let url_template =
    Printf.sprintf "https://example.com/ocaml-%s/{id}" marker
  in
  let alternative_url =
    Printf.sprintf "https://alt.example.com/ocaml-%s/{id}" marker
  in
  let existing_linkifiers = Server.get_linkifiers client |> ok in
  let existing_linkifier_ids =
    List.map
      (fun (linkifier : Server.linkifier) -> linkifier.id)
      existing_linkifiers
  in
  let filter_id =
    Server.add_linkifier client ~pattern:linkifier_pattern ~url_template
      ~example_input:(Server.Set example_input)
      ~reverse_template:(Server.Set reverse_template)
      ~alternative_url_templates:[ alternative_url ] ()
    |> ok
  in
  linkifier_id := Some filter_id;
  let created_linkifier =
    match find_linkifier filter_id (Server.get_linkifiers client |> ok) with
    | Some linkifier -> linkifier
    | None -> Alcotest.fail "created linkifier was not listed"
  in
  Alcotest.(check string)
    "created linkifier pattern" linkifier_pattern created_linkifier.pattern;
  Alcotest.(check (option string))
    "created reverse template" (Some reverse_template)
    created_linkifier.reverse_template;
  Alcotest.(check (list string))
    "created alternative templates" [ alternative_url ]
    created_linkifier.alternative_url_templates;

  let updated_url =
    Printf.sprintf "https://example.com/updated-ocaml-%s/{id}" marker
  in
  Server.update_linkifier client ~filter_id ~pattern:linkifier_pattern
    ~url_template:updated_url ~example_input:(Server.Set example_input)
    ~reverse_template:(Server.Set reverse_template)
    ~alternative_url_templates:[] ()
  |> ok;
  let updated_linkifier =
    match find_linkifier filter_id (Server.get_linkifiers client |> ok) with
    | Some linkifier -> linkifier
    | None -> Alcotest.fail "updated linkifier was not listed"
  in
  Alcotest.(check string)
    "updated linkifier URL" updated_url updated_linkifier.url_template;
  Alcotest.(check (list string))
    "cleared alternative templates" []
    updated_linkifier.alternative_url_templates;
  Server.reorder_linkifiers client
    ~ordered_linkifier_ids:(filter_id :: existing_linkifier_ids)
  |> ok;
  (match Server.get_linkifiers client |> ok with
  | first :: _ ->
      Alcotest.(check int)
        "linkifier reorder"
        (Zulip.Id.Linkifier.to_int filter_id)
        (Zulip.Id.Linkifier.to_int first.id)
  | [] -> Alcotest.fail "linkifiers disappeared after reorder");
  Server.reorder_linkifiers client
    ~ordered_linkifier_ids:(existing_linkifier_ids @ [ filter_id ])
  |> ok;
  Server.delete_linkifier client ~filter_id |> ok;
  linkifier_id := None;
  check
    (Option.is_none
       (find_linkifier filter_id (Server.get_linkifiers client |> ok)))
    "deleted linkifier was still listed";

  Settings.update client
    [ Settings.Set (Settings.Twenty_four_hour_time, current_time_setting) ]
  |> ok |> ignore;
  original_time_setting := None;
  restore_status client ~user old_status;
  original_status := None
