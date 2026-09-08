open Result.Syntax
module Push = Matrix_proto.Push

let rule_path_route = Route.v "/pushrules/global/{kind}/{rule}"
let enabled_rule_path_route = Route.v "/pushrules/global/{kind}/{rule}/enabled"
let actions_rule_path_route = Route.v "/pushrules/global/{kind}/{rule}/actions"

let rule_bindings rule_id =
  [
    ("kind", Push.Kind.to_string (Push.Rule_id.kind rule_id));
    ("rule", Push.Rule_id.id rule_id);
  ]

let rule_path ?(suffix = "") rule_id =
  match suffix with
  | "" -> Route.expand_exn rule_path_route (rule_bindings rule_id)
  | "/enabled" ->
      Route.expand_exn enabled_rule_path_route (rule_bindings rule_id)
  | "/actions" ->
      Route.expand_exn actions_rule_path_route (rule_bindings rule_id)
  | _ -> invalid_arg "Matrix_client.Push.rule_path: unsupported suffix"

let get_push_rules client =
  let* body = Client.Http.get client ~path:"/pushrules/" () in
  Client.Http.decode_response Push.Ruleset.global_jsont body

let get_push_rule client rule_id =
  let* body = Client.Http.get client ~path:(rule_path rule_id) () in
  Client.Http.decode_response (Push.Rule.jsont (Push.Rule_id.kind rule_id)) body

let delete_push_rule client rule_id =
  let+ _ = Client.Http.delete client ~path:(rule_path rule_id) () in
  ()

type add_rule_request = {
  actions : Push.Action.t list;
  conditions : Push.Condition.t list option;
  pattern : string option;
}
[@@warning "-69"]

let add_rule_request_jsont =
  Jsont.Object.(
    map (fun actions conditions pattern -> { actions; conditions; pattern })
    |> mem "actions" (Jsont.list Push.Action.jsont) ~enc:(fun t -> t.actions)
    |> opt_mem "conditions" (Jsont.list Push.Condition.jsont) ~enc:(fun t ->
        t.conditions)
    |> opt_mem "pattern" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.pattern)
    |> finish)

let set_push_rule client rule_id ~actions ?conditions ?pattern ?before ?after ()
    =
  let query =
    List.filter_map Fun.id
      [
        Option.map (fun b -> ("before", b)) before;
        Option.map (fun a -> ("after", a)) after;
      ]
  in
  let query = if query = [] then None else Some query in
  let* body =
    Client.Http.encode_body add_rule_request_jsont
      { actions; conditions; pattern }
  in
  let+ _ = Client.Http.put client ~path:(rule_path rule_id) ~body ?query () in
  ()

type enabled_request = { enabled : bool } [@@warning "-69"]

let enabled_request_jsont =
  Jsont.Object.(
    map (fun enabled -> { enabled })
    |> mem "enabled" Jsont.bool ~enc:(fun t -> t.enabled)
    |> finish)

let set_enabled client rule_id ~enabled =
  let* body = Client.Http.encode_body enabled_request_jsont { enabled } in
  let+ _ =
    Client.Http.put client ~path:(rule_path ~suffix:"/enabled" rule_id) ~body ()
  in
  ()

type actions_request = { actions : Push.Action.t list } [@@warning "-69"]

let actions_request_jsont =
  Jsont.Object.(
    map (fun actions -> { actions })
    |> mem "actions" (Jsont.list Push.Action.jsont) ~enc:(fun t -> t.actions)
    |> finish)

let set_actions client rule_id ~actions =
  let* body = Client.Http.encode_body actions_request_jsont { actions } in
  let+ _ =
    Client.Http.put client ~path:(rule_path ~suffix:"/actions" rule_id) ~body ()
  in
  ()

type pusher_kind = Http | Email

let pusher_kind_jsont =
  Jsont.enum ~kind:"pusher kind" [ ("http", Http); ("email", Email) ]

type pusher_data = { url : string option; format : string option }

let pusher_data_jsont =
  Jsont.Object.(
    map (fun url format -> { url; format })
    |> opt_mem "url" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.url)
    |> opt_mem "format" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.format)
    |> finish)

type pusher = {
  pushkey : string;
  kind : pusher_kind;
  app_id : string;
  app_display_name : string;
  device_display_name : string;
  profile_tag : string option;
  lang : string;
  data : pusher_data;
}

let pusher_jsont =
  Jsont.Object.(
    map
      (fun
        pushkey
        kind
        app_id
        app_display_name
        device_display_name
        profile_tag
        lang
        data
      ->
        {
          pushkey;
          kind;
          app_id;
          app_display_name;
          device_display_name;
          profile_tag;
          lang;
          data;
        })
    |> mem "pushkey" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pushkey)
    |> mem "kind" pusher_kind_jsont ~enc:(fun t -> t.kind)
    |> mem "app_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.app_id)
    |> mem "app_display_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.app_display_name)
    |> mem "device_display_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.device_display_name)
    |> opt_mem "profile_tag" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.profile_tag)
    |> mem "lang" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.lang)
    |> mem "data" pusher_data_jsont ~enc:(fun t -> t.data)
    |> finish)

let pushers_jsont =
  Jsont.Object.(
    map Fun.id
    |> mem "pushers" (Jsont.list pusher_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:Fun.id
    |> finish)

let get_pushers client =
  let* body = Client.Http.get client ~path:"/pushers" () in
  Client.Http.decode_response pushers_jsont body

type set_pusher_request = {
  pushkey : string;
  kind : pusher_kind;
  app_id : string;
  app_display_name : string;
  device_display_name : string;
  profile_tag : string option;
  lang : string;
  data : pusher_data;
  append : bool option;
}
[@@warning "-69"]

let set_pusher_request_jsont =
  Jsont.Object.(
    map
      (fun
        pushkey
        kind
        app_id
        app_display_name
        device_display_name
        profile_tag
        lang
        data
        append
      ->
        {
          pushkey;
          kind;
          app_id;
          app_display_name;
          device_display_name;
          profile_tag;
          lang;
          data;
          append;
        })
    |> mem "pushkey" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pushkey)
    |> mem "kind" pusher_kind_jsont ~enc:(fun t -> t.kind)
    |> mem "app_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.app_id)
    |> mem "app_display_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.app_display_name)
    |> mem "device_display_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.device_display_name)
    |> opt_mem "profile_tag" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.profile_tag)
    |> mem "lang" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.lang)
    |> mem "data" pusher_data_jsont ~enc:(fun t -> t.data)
    |> opt_mem "append" Jsont.bool ~enc:(fun t -> t.append)
    |> finish)

let set_pusher client ~pushkey ~kind ~app_id ~app_display_name
    ~device_display_name ?profile_tag ~lang ~data ?append () =
  let* body =
    Client.Http.encode_body set_pusher_request_jsont
      {
        pushkey;
        kind;
        app_id;
        app_display_name;
        device_display_name;
        profile_tag;
        lang;
        data;
        append;
      }
  in
  let+ _ = Client.Http.post client ~path:"/pushers/set" ~body () in
  ()

let delete_pusher client ~pushkey ~app_id =
  (* The [null] kind has no codec of its own, so the body is built as a
     generic JSON value; [pushkey] and [app_id] are caller-supplied and must
     be escaped by the encoder rather than interpolated into a literal. *)
  let jmem n v = Jsont.Json.mem (Jsont.Json.name n) v in
  let body =
    Jsont.Json.object'
      [
        jmem "pushkey" (Jsont.Json.string pushkey);
        jmem "kind" (Jsont.Json.null ());
        jmem "app_id" (Jsont.Json.string app_id);
      ]
  in
  (* [Jsont.json]'s encoder is total, so the error branch is unreachable. *)
  let body =
    match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json body with
    | Ok s -> s
    | Error _ -> "{}"
  in
  let+ _ = Client.Http.post client ~path:"/pushers/set" ~body () in
  ()
