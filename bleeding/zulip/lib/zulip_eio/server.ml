let ( let* ) = Result.bind
let unit_result = Result.map (Fun.const ())

type authentication_method = {
  password : bool;
  dev : bool;
  email : bool;
  ldap : bool;
  remoteuser : bool;
  github : bool;
  azuread : bool;
  gitlab : bool;
  apple : bool;
  google : bool;
  saml : bool;
  openid_connect : bool;
  discord : bool;
}

type external_authentication_method = {
  name : string;
  display_name : string;
  display_icon : string option;
  login_url : string;
  signup_url : string;
}

type t = {
  zulip_version : string;
  zulip_feature_level : int;
  zulip_merge_base : string option;
  push_notifications_enabled : bool;
  is_incompatible : bool;
  email_auth_enabled : bool;
  require_email_format_usernames : bool;
  realm_uri : string;
  realm_url : string;
  realm_name : string;
  realm_icon : string;
  realm_description : string;
  realm_web_public_access_enabled : bool;
  authentication_methods : authentication_method;
  external_authentication_methods : external_authentication_method list;
}

let authentication_method_jsont =
  Jsont.Object.map ~kind:"Zulip authentication methods"
    (fun
      password
      dev
      email
      ldap
      remoteuser
      github
      azuread
      gitlab
      apple
      google
      saml
      openid_connect
      discord
    ->
      {
        password;
        dev;
        email;
        ldap;
        remoteuser;
        github;
        azuread;
        gitlab;
        apple;
        google;
        saml;
        openid_connect;
        discord;
      })
  |> Jsont.Object.mem "password" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.password)
  |> Jsont.Object.mem "dev" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.dev)
  |> Jsont.Object.mem "email" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.email)
  |> Jsont.Object.mem "ldap" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.ldap)
  |> Jsont.Object.mem "remoteuser" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.remoteuser)
  |> Jsont.Object.mem "github" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.github)
  |> Jsont.Object.mem "azuread" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.azuread)
  |> Jsont.Object.mem "gitlab" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.gitlab)
  |> Jsont.Object.mem "apple" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.apple)
  |> Jsont.Object.mem "google" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.google)
  |> Jsont.Object.mem "saml" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.saml)
  |> Jsont.Object.mem "openid connect" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.openid_connect)
  |> Jsont.Object.mem "discord" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.discord)
  |> Jsont.Object.finish

let external_jsont =
  Jsont.Object.map ~kind:"Zulip external authentication method"
    (fun name display_name display_icon login_url signup_url ->
      { name; display_name; display_icon; login_url; signup_url })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun x -> x.name)
  |> Jsont.Object.mem "display_name" Jsont.string ~enc:(fun x -> x.display_name)
  |> Jsont.Object.mem "display_icon"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> x.display_icon)
  |> Jsont.Object.mem "login_url" Jsont.string ~enc:(fun x -> x.login_url)
  |> Jsont.Object.mem "signup_url" Jsont.string ~enc:(fun x -> x.signup_url)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"Zulip server settings"
    (fun
      zulip_version
      zulip_feature_level
      zulip_merge_base
      push_notifications_enabled
      is_incompatible
      email_auth_enabled
      require_email_format_usernames
      realm_uri
      realm_url
      realm_name
      realm_icon
      realm_description
      realm_web_public_access_enabled
      authentication_methods
      external_authentication_methods
    ->
      {
        zulip_version;
        zulip_feature_level;
        zulip_merge_base;
        push_notifications_enabled;
        is_incompatible;
        email_auth_enabled;
        require_email_format_usernames;
        realm_uri;
        realm_url;
        realm_name;
        realm_icon;
        realm_description;
        realm_web_public_access_enabled;
        authentication_methods;
        external_authentication_methods;
      })
  |> Jsont.Object.mem "zulip_version" Jsont.string ~enc:(fun s ->
      s.zulip_version)
  |> Jsont.Object.mem "zulip_feature_level" Jsont.int ~enc:(fun s ->
      s.zulip_feature_level)
  |> Jsont.Object.opt_mem "zulip_merge_base" Jsont.string ~enc:(fun s ->
      s.zulip_merge_base)
  |> Jsont.Object.mem "push_notifications_enabled" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun s -> s.push_notifications_enabled)
  |> Jsont.Object.mem "is_incompatible" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun s -> s.is_incompatible)
  |> Jsont.Object.mem "email_auth_enabled" Jsont.bool
       ~dec_absent:(fun () -> true)
       ~enc:(fun s -> s.email_auth_enabled)
  |> Jsont.Object.mem "require_email_format_usernames" Jsont.bool
       ~dec_absent:(fun () -> true)
       ~enc:(fun s -> s.require_email_format_usernames)
  |> Jsont.Object.mem "realm_uri" Jsont.string ~enc:(fun s -> s.realm_uri)
  |> Jsont.Object.mem "realm_url" Jsont.string ~enc:(fun s -> s.realm_url)
  |> Jsont.Object.mem "realm_name" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun s -> s.realm_name)
  |> Jsont.Object.mem "realm_icon" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun s -> s.realm_icon)
  |> Jsont.Object.mem "realm_description" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun s -> s.realm_description)
  |> Jsont.Object.mem "realm_web_public_access_enabled" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun s -> s.realm_web_public_access_enabled)
  |> Jsont.Object.mem "authentication_methods" authentication_method_jsont
       ~enc:(fun s -> s.authentication_methods)
  |> Jsont.Object.mem "external_authentication_methods"
       (Jsont.list external_jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun s -> s.external_authentication_methods)
  |> Jsont.Object.finish

let get_settings_json client =
  Client.request client ~method_:`GET ~path:"/api/v1/server_settings" ()

let get_settings client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/server_settings"
    ~codec:jsont ()

let feature_level client =
  Result.map (fun s -> s.zulip_feature_level) (get_settings client)

let supports_feature client ~level =
  Result.map (fun n -> n >= level) (feature_level client)

type linkifier = {
  id : Zulip.Id.Linkifier.t;
  pattern : string;
  url_template : string;
  example_input : string option;
  reverse_template : string option;
  alternative_url_templates : string list;
  extensions : Jsont.json;
}

type 'a field_change = Clear | Set of 'a

let linkifier_jsont =
  Jsont.Object.map ~kind:"Zulip linkifier"
    (fun
      id
      pattern
      url_template
      example_input
      reverse_template
      alternative_url_templates
      extensions
    ->
      {
        id;
        pattern;
        url_template;
        example_input;
        reverse_template;
        alternative_url_templates;
        extensions;
      })
  |> Jsont.Object.mem "id" Zulip.Id.Linkifier.jsont ~enc:(fun l -> l.id)
  |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun l -> l.pattern)
  |> Jsont.Object.mem "url_template" Jsont.string ~enc:(fun l -> l.url_template)
  |> Jsont.Object.mem "example_input"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun l -> l.example_input)
  |> Jsont.Object.mem "reverse_template"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun l -> l.reverse_template)
  |> Jsont.Object.mem "alternative_url_templates" (Jsont.list Jsont.string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun l -> l.alternative_url_templates)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun l -> l.extensions)
  |> Jsont.Object.finish

let list_response name codec =
  Jsont.Object.map ~kind:("Zulip " ^ name ^ " response") Fun.id
  |> Jsont.Object.mem name (Jsont.list codec) ~enc:Fun.id
  |> Jsont.Object.finish

let get_linkifiers client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/realm/linkifiers"
    ~codec:(list_response "linkifiers" linkifier_jsont)
    ()

let id_response codec =
  Jsont.Object.map ~kind:"Zulip integer ID response" Fun.id
  |> Jsont.Object.mem "id" codec ~enc:Fun.id
  |> Jsont.Object.finish

let change_param name = function
  | None -> []
  | Some Clear -> [ (name, "") ]
  | Some (Set value) -> [ (name, value) ]

let string_list_param name = function
  | None -> Ok []
  | Some values ->
      let* values = Codec.encode (Jsont.list Jsont.string) values in
      Ok [ (name, values) ]

let linkifier_params ~pattern ~url_template ?example_input ?reverse_template
    ?alternative_url_templates () =
  let* alternatives =
    string_list_param "alternative_url_templates" alternative_url_templates
  in
  Ok
    ([ ("pattern", pattern); ("url_template", url_template) ]
    @ change_param "example_input" example_input
    @ change_param "reverse_template" reverse_template
    @ alternatives)

let add_linkifier client ~pattern ~url_template ?example_input ?reverse_template
    ?alternative_url_templates () =
  let* params =
    linkifier_params ~pattern ~url_template ?example_input ?reverse_template
      ?alternative_url_templates ()
  in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/realm/filters"
    ~params
    ~codec:(id_response Zulip.Id.Linkifier.jsont)
    ()

let update_linkifier client ~filter_id ~pattern ~url_template ?example_input
    ?reverse_template ?alternative_url_templates () =
  let* params =
    linkifier_params ~pattern ~url_template ?example_input ?reverse_template
      ?alternative_url_templates ()
  in
  Client.request client ~method_:`PATCH
    ~path:
      ("/api/v1/realm/filters/"
      ^ string_of_int (Zulip.Id.Linkifier.to_int filter_id))
    ~params ()
  |> unit_result

let delete_linkifier client ~filter_id =
  Client.request client ~method_:`DELETE
    ~path:
      ("/api/v1/realm/filters/"
      ^ string_of_int (Zulip.Id.Linkifier.to_int filter_id))
    ()
  |> unit_result

let reorder_linkifiers client ~ordered_linkifier_ids =
  let* ids =
    Codec.encode (Jsont.list Zulip.Id.Linkifier.jsont) ordered_linkifier_ids
  in
  Client.request client ~method_:`PATCH ~path:"/api/v1/realm/linkifiers"
    ~params:[ ("ordered_linkifier_ids", ids) ]
    ()
  |> unit_result

type emoji = {
  id : string;
  name : string;
  source_url : string;
  deactivated : bool;
  author_id : Zulip.Id.User.t option;
  still_url : string option;
  extensions : Jsont.json;
}

let emoji_jsont =
  Jsont.Object.map ~kind:"Zulip custom emoji"
    (fun id name source_url deactivated author_id still_url extensions ->
      ({ id; name; source_url; deactivated; author_id; still_url; extensions }
        : emoji))
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun (e : emoji) -> e.id)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (e : emoji) -> e.name)
  |> Jsont.Object.mem "source_url" Jsont.string ~enc:(fun (e : emoji) ->
      e.source_url)
  |> Jsont.Object.mem "deactivated" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (e : emoji) -> e.deactivated)
  |> Jsont.Object.mem "author_id"
       (Jsont.option Zulip.Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun (e : emoji) -> e.author_id)
  |> Jsont.Object.mem "still_url"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun (e : emoji) -> e.still_url)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (e : emoji) ->
      e.extensions)
  |> Jsont.Object.finish

let emoji_map_jsont =
  let dec = function
    | Jsont.Object (ms, _) ->
        List.map
          (fun ((key, meta), j) ->
            match Jsont.Json.decode' emoji_jsont j with
            | Ok e ->
                if key <> e.id then
                  Jsont.Error.msgf meta "emoji map key %S does not match id %S"
                    key e.id;
                {
                  id = e.id;
                  name = e.name;
                  source_url = e.source_url;
                  deactivated = e.deactivated;
                  author_id = e.author_id;
                  still_url = e.still_url;
                  extensions = e.extensions;
                }
            | Error e -> raise (Jsont.Error e))
          ms
    | j -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object j
  in
  Jsont.map ~kind:"Zulip custom emoji map" ~dec Jsont.json

let emoji_response =
  Jsont.Object.map ~kind:"Zulip custom emoji response" Fun.id
  |> Jsont.Object.mem "emoji" emoji_map_jsont ~enc:Fun.id
  |> Jsont.Object.finish

let get_emoji client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/realm/emoji"
    ~codec:emoji_response ()

let upload_emoji client ~name ~filename ~content_type content =
  Client.multipart client
    ~path:("/api/v1/realm/emoji/" ^ Client.path_segment name)
    [ Fetch.Form.file ~name:"file" ~filename ~content_type content ]
  |> unit_result

let upload_emoji_stream client ~name ~filename ~content_type ?length source =
  if Option.exists (fun n -> n < 0L) length then
    Error (Error.Invalid_request "Upload length must be nonnegative")
  else
    Client.multipart client
      ~path:("/api/v1/realm/emoji/" ^ Client.path_segment name)
      [ Fetch.Form.stream ~name:"file" ~filename ~content_type ?length source ]
    |> unit_result

let deactivate_emoji client ~name =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/realm/emoji/" ^ Client.path_segment name)
    ()
  |> unit_result

type profile_field_type =
  | Short_text
  | Long_text
  | Choice
  | Date
  | Link
  | User
  | External_account
  | Pronouns
  | Other of int

let profile_field_type_to_int = function
  | Short_text -> 1
  | Long_text -> 2
  | Choice -> 3
  | Date -> 4
  | Link -> 5
  | User -> 6
  | External_account -> 7
  | Pronouns -> 8
  | Other n -> n

let profile_field_type_of_int = function
  | 1 -> Short_text
  | 2 -> Long_text
  | 3 -> Choice
  | 4 -> Date
  | 5 -> Link
  | 6 -> User
  | 7 -> External_account
  | 8 -> Pronouns
  | n -> Other n

let profile_field_type_jsont =
  Jsont.map ~kind:"Zulip profile field type" ~dec:profile_field_type_of_int
    ~enc:profile_field_type_to_int Jsont.int

type profile_field = {
  id : Zulip.Id.Profile_field.t;
  field_type : profile_field_type;
  order : int;
  name : string;
  hint : string;
  field_data : string;
  display_in_profile_summary : bool option;
  required : bool;
  editable_by_user : bool;
  use_for_user_matching : bool;
  extensions : Jsont.json;
}

let profile_field_jsont =
  Jsont.Object.map ~kind:"Zulip profile field"
    (fun
      id
      field_type
      order
      name
      hint
      field_data
      display_in_profile_summary
      required
      editable_by_user
      use_for_user_matching
      extensions
    ->
      {
        id;
        field_type;
        order;
        name;
        hint;
        field_data;
        display_in_profile_summary;
        required;
        editable_by_user;
        use_for_user_matching;
        extensions;
      })
  |> Jsont.Object.mem "id" Zulip.Id.Profile_field.jsont ~enc:(fun f -> f.id)
  |> Jsont.Object.mem "type" profile_field_type_jsont ~enc:(fun f ->
      f.field_type)
  |> Jsont.Object.mem "order" Jsont.int ~enc:(fun f -> f.order)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun f -> f.name)
  |> Jsont.Object.mem "hint" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun f -> f.hint)
  |> Jsont.Object.mem "field_data" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun f -> f.field_data)
  |> Jsont.Object.opt_mem "display_in_profile_summary" Jsont.bool ~enc:(fun f ->
      f.display_in_profile_summary)
  |> Jsont.Object.mem "required" Jsont.bool ~enc:(fun f -> f.required)
  |> Jsont.Object.mem "editable_by_user" Jsont.bool ~enc:(fun f ->
      f.editable_by_user)
  |> Jsont.Object.mem "use_for_user_matching" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun f -> f.use_for_user_matching)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun f -> f.extensions)
  |> Jsont.Object.finish

let profile_fields_response = list_response "custom_fields" profile_field_jsont

let get_profile_fields client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/realm/profile_fields"
    ~codec:profile_fields_response ()

let json_param name = function
  | None -> Ok []
  | Some j ->
      let* v = Codec.encode Jsont.json j in
      Ok [ (name, v) ]

let bool_param name = function
  | None -> []
  | Some value -> [ (name, string_of_bool value) ]

let create_profile_field client ~field_type ~name ?hint ?field_data
    ?display_in_profile_summary ?required ?editable_by_user
    ?use_for_user_matching () =
  let* data = json_param "field_data" field_data in
  Client.request_typed client ~method_:`POST
    ~path:"/api/v1/realm/profile_fields"
    ~params:
      ([
         ("field_type", string_of_int (profile_field_type_to_int field_type));
         ("name", name);
       ]
      @ Option.fold ~none:[] ~some:(fun v -> [ ("hint", v) ]) hint
      @ data
      @ bool_param "display_in_profile_summary" display_in_profile_summary
      @ bool_param "required" required
      @ bool_param "editable_by_user" editable_by_user
      @ bool_param "use_for_user_matching" use_for_user_matching)
    ~codec:(id_response Zulip.Id.Profile_field.jsont)
    ()

let update_profile_field client ~field_id ?name ?hint ?field_data
    ?display_in_profile_summary ?required ?editable_by_user
    ?use_for_user_matching () =
  let* data = json_param "field_data" field_data in
  let params =
    Option.fold ~none:[] ~some:(fun v -> [ ("name", v) ]) name
    @ Option.fold ~none:[] ~some:(fun v -> [ ("hint", v) ]) hint
    @ data
    @ bool_param "display_in_profile_summary" display_in_profile_summary
    @ bool_param "required" required
    @ bool_param "editable_by_user" editable_by_user
    @ bool_param "use_for_user_matching" use_for_user_matching
  in
  if params = [] then
    Error (Error.Invalid_request "profile-field update has no changes")
  else
    Client.request client ~method_:`PATCH
      ~path:
        ("/api/v1/realm/profile_fields/"
        ^ string_of_int (Zulip.Id.Profile_field.to_int field_id))
      ~params ()
    |> unit_result

let delete_profile_field client ~field_id =
  Client.request client ~method_:`DELETE
    ~path:
      ("/api/v1/realm/profile_fields/"
      ^ string_of_int (Zulip.Id.Profile_field.to_int field_id))
    ()
  |> unit_result

let reorder_profile_fields client ~order =
  let* order = Codec.encode (Jsont.list Zulip.Id.Profile_field.jsont) order in
  Client.request client ~method_:`PATCH ~path:"/api/v1/realm/profile_fields"
    ~params:[ ("order", order) ]
    ()
  |> unit_result
