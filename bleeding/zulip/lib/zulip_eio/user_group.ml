type group_setting = Zulip.Group_setting.t =
  | Group of Zulip.Id.User_group.t
  | Direct of {
      members : Zulip.Id.User.t list;
      subgroups : Zulip.Id.User_group.t list;
    }

type group_setting_update = Zulip.Group_setting.update = {
  new_ : group_setting;
  old : group_setting option;
}

type t = {
  id : Zulip.Id.User_group.t;
  name : string;
  description : string;
  members : Zulip.Id.User.t list;
  direct_subgroup_ids : Zulip.Id.User_group.t list;
  is_system_group : bool;
  deactivated : bool;
  creator_id : Zulip.Id.User.t option;
  date_created : float option;
  can_add_members_group : group_setting option;
  can_join_group : group_setting option;
  can_leave_group : group_setting option;
  can_manage_group : group_setting option;
  can_mention_group : group_setting;
  can_remove_members_group : group_setting option;
  extensions : Jsont.json;
}

let group_setting_jsont = Zulip.Group_setting.jsont
let group_setting_update_jsont = Zulip.Group_setting.update_jsont

let jsont =
  Jsont.Object.map ~kind:"Zulip user group"
    (fun
      id
      name
      description
      members
      direct_subgroup_ids
      is_system_group
      deactivated
      creator_id
      date_created
      can_add_members_group
      can_join_group
      can_leave_group
      can_manage_group
      can_mention_group
      can_remove_members_group
      extensions
    ->
      {
        id;
        name;
        description;
        members;
        direct_subgroup_ids;
        is_system_group;
        deactivated;
        creator_id;
        date_created;
        can_add_members_group;
        can_join_group;
        can_leave_group;
        can_manage_group;
        can_mention_group;
        can_remove_members_group;
        extensions;
      })
  |> Jsont.Object.mem "id" Zulip.Id.User_group.jsont ~enc:(fun g -> g.id)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun g -> g.name)
  |> Jsont.Object.mem "description" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun g -> g.description)
  |> Jsont.Object.mem "members"
       (Jsont.list Zulip.Id.User.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun g -> g.members)
  |> Jsont.Object.mem "direct_subgroup_ids"
       (Jsont.list Zulip.Id.User_group.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun g -> g.direct_subgroup_ids)
  |> Jsont.Object.mem "is_system_group" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun g -> g.is_system_group)
  |> Jsont.Object.mem "deactivated" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun g -> g.deactivated)
  |> Jsont.Object.mem "creator_id"
       (Jsont.option Zulip.Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun g -> g.creator_id)
  |> Jsont.Object.mem "date_created"
       (Jsont.option Jsont.number)
       ~dec_absent:(fun () -> None)
       ~enc:(fun g -> g.date_created)
  |> Jsont.Object.opt_mem "can_add_members_group" group_setting_jsont
       ~enc:(fun g -> g.can_add_members_group)
  |> Jsont.Object.opt_mem "can_join_group" group_setting_jsont ~enc:(fun g ->
      g.can_join_group)
  |> Jsont.Object.opt_mem "can_leave_group" group_setting_jsont ~enc:(fun g ->
      g.can_leave_group)
  |> Jsont.Object.opt_mem "can_manage_group" group_setting_jsont ~enc:(fun g ->
      g.can_manage_group)
  |> Jsont.Object.mem "can_mention_group" group_setting_jsont ~enc:(fun g ->
      g.can_mention_group)
  |> Jsont.Object.opt_mem "can_remove_members_group" group_setting_jsont
       ~enc:(fun g -> g.can_remove_members_group)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun g -> g.extensions)
  |> Jsont.Object.finish

let raw group =
  match Jsont.Json.encode' jsont group with
  | Ok j -> j
  | Error e -> raise (Jsont.Error e)

let pp ppf group =
  Format.fprintf ppf "User_group{id=%a; name=%S}" Zulip.Id.User_group.pp
    group.id group.name

let ( let* ) = Result.bind
let unit_result result = Result.map (Fun.const ()) result

let path id =
  "/api/v1/user_groups/" ^ string_of_int (Zulip.Id.User_group.to_int id)

let encode_list codec values = Codec.encode (Jsont.list codec) values

let list_jsont =
  Jsont.Object.map ~kind:"Zulip user groups response" Fun.id
  |> Jsont.Object.mem "user_groups" (Jsont.list jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let list_all client ?include_deactivated_groups () =
  let params =
    Option.fold ~none:[]
      ~some:(fun v -> [ ("include_deactivated_groups", string_of_bool v) ])
      include_deactivated_groups
  in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/user_groups" ~params
    ~codec:list_jsont ()

let list client = list_all client ()

let id_jsont =
  Jsont.Object.map ~kind:"Zulip user group ID" Fun.id
  |> Jsont.Object.mem "group_id" Zulip.Id.User_group.jsont ~enc:Fun.id
  |> Jsont.Object.finish

let setting name = function
  | None -> Ok []
  | Some value ->
      let* value = Codec.encode group_setting_jsont value in
      Ok [ (name, value) ]

let setting_update name = function
  | None -> Ok []
  | Some value ->
      let* value = Codec.encode group_setting_update_jsont value in
      Ok [ (name, value) ]

let create client ~name ~description ~members ?(subgroups = [])
    ?can_add_members_group ?can_join_group ?can_leave_group ?can_manage_group
    ?can_mention_group ?can_remove_members_group () =
  let* members = encode_list Zulip.Id.User.jsont members in
  let* subgroups = encode_list Zulip.Id.User_group.jsont subgroups in
  let* can_add_members_group =
    setting "can_add_members_group" can_add_members_group
  in
  let* can_join_group = setting "can_join_group" can_join_group in
  let* can_leave_group = setting "can_leave_group" can_leave_group in
  let* can_manage_group = setting "can_manage_group" can_manage_group in
  let* can_mention_group = setting "can_mention_group" can_mention_group in
  let* can_remove_members_group =
    setting "can_remove_members_group" can_remove_members_group
  in
  let params =
    [
      ("name", name);
      ("description", description);
      ("members", members);
      ("subgroups", subgroups);
    ]
    @ can_add_members_group @ can_join_group @ can_leave_group
    @ can_manage_group @ can_mention_group @ can_remove_members_group
  in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/user_groups/create"
    ~params ~codec:id_jsont ()

let update client ~group_id ?name ?description ?can_add_members_group
    ?can_join_group ?can_leave_group ?can_manage_group ?can_mention_group
    ?can_remove_members_group ?deactivated () =
  let* can_add_members_group =
    setting_update "can_add_members_group" can_add_members_group
  in
  let* can_join_group = setting_update "can_join_group" can_join_group in
  let* can_leave_group = setting_update "can_leave_group" can_leave_group in
  let* can_manage_group = setting_update "can_manage_group" can_manage_group in
  let* can_mention_group =
    setting_update "can_mention_group" can_mention_group
  in
  let* can_remove_members_group =
    setting_update "can_remove_members_group" can_remove_members_group
  in
  let opt name f = Option.fold ~none:[] ~some:(fun v -> [ (name, f v) ]) in
  let params =
    opt "name" Fun.id name
    @ opt "description" Fun.id description
    @ can_add_members_group @ can_join_group @ can_leave_group
    @ can_manage_group @ can_mention_group @ can_remove_members_group
    @ opt "deactivated" string_of_bool deactivated
  in
  if params = [] then
    Error (Error.Invalid_request "user-group update has no changes")
  else
    Client.request client ~method_:`PATCH ~path:(path group_id) ~params ()
    |> unit_result

let optional_ids name codec = function
  | None -> Ok []
  | Some ids ->
      let* value = encode_list codec ids in
      Ok [ (name, value) ]

let update_members client ~group_id ?add ?remove ?add_subgroups
    ?remove_subgroups () =
  let* add = optional_ids "add" Zulip.Id.User.jsont add in
  let* remove = optional_ids "delete" Zulip.Id.User.jsont remove in
  let* add_subgroups =
    optional_ids "add_subgroups" Zulip.Id.User_group.jsont add_subgroups
  in
  let* remove_subgroups =
    optional_ids "delete_subgroups" Zulip.Id.User_group.jsont remove_subgroups
  in
  let params = add @ remove @ add_subgroups @ remove_subgroups in
  if params = [] then
    Error (Error.Invalid_request "group membership update has no changes")
  else
    Client.request client ~method_:`POST
      ~path:(path group_id ^ "/members")
      ~params ()
    |> unit_result

let update_subgroups client ~group_id ?add ?remove () =
  let* add = optional_ids "add" Zulip.Id.User_group.jsont add in
  let* remove = optional_ids "delete" Zulip.Id.User_group.jsont remove in
  let params = add @ remove in
  if params = [] then
    Error (Error.Invalid_request "group subgroup update has no changes")
  else
    Client.request client ~method_:`POST
      ~path:(path group_id ^ "/subgroups")
      ~params ()
    |> unit_result

let delete client ~group_id =
  Client.request client ~method_:`POST ~path:(path group_id ^ "/deactivate") ()
  |> unit_result

let members_jsont =
  Jsont.Object.map ~kind:"Zulip user-group members" Fun.id
  |> Jsont.Object.mem "members" (Jsont.list Zulip.Id.User.jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let subgroups_jsont =
  Jsont.Object.map ~kind:"Zulip user-group subgroups" Fun.id
  |> Jsont.Object.mem "subgroups"
       (Jsont.list Zulip.Id.User_group.jsont)
       ~enc:Fun.id
  |> Jsont.Object.finish

let bool_response name =
  Jsont.Object.map ~kind:"Zulip membership response" Fun.id
  |> Jsont.Object.mem name Jsont.bool ~enc:Fun.id
  |> Jsont.Object.finish

let bool_param name =
  Option.fold ~none:[] ~some:(fun v -> [ (name, string_of_bool v) ])

let get_members client ~group_id ?direct_member_only () =
  Client.request_typed client ~method_:`GET
    ~path:(path group_id ^ "/members")
    ~params:(bool_param "direct_member_only" direct_member_only)
    ~codec:members_jsont ()

let is_member client ~group_id ~user_id ?direct_member_only () =
  Client.request_typed client ~method_:`GET
    ~path:
      (path group_id ^ "/members/"
      ^ string_of_int (Zulip.Id.User.to_int user_id))
    ~params:(bool_param "direct_member_only" direct_member_only)
    ~codec:(bool_response "is_user_group_member")
    ()

let get_subgroups client ~group_id ?direct_subgroup_only () =
  Client.request_typed client ~method_:`GET
    ~path:(path group_id ^ "/subgroups")
    ~params:(bool_param "direct_subgroup_only" direct_subgroup_only)
    ~codec:subgroups_jsont ()

let is_subgroup client ~group_id ~subgroup_id ?direct_subgroup_only () =
  Result.map
    (fun ids -> List.mem subgroup_id ids)
    (get_subgroups client ~group_id ?direct_subgroup_only ())
