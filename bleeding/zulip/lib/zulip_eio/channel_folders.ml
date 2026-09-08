type t = {
  id : Zulip.Id.Channel_folder.t;
  name : string;
  order : int option;
  date_created : float option;
  creator_id : Zulip.Id.User.t option;
  description : string;
  rendered_description : string;
  is_archived : bool;
  extensions : Jsont.json;
}

let jsont =
  Jsont.Object.map ~kind:"Zulip channel folder"
    (fun
      id
      name
      order
      date_created
      creator_id
      description
      rendered_description
      is_archived
      extensions
    ->
      {
        id;
        name;
        order;
        date_created;
        creator_id;
        description;
        rendered_description;
        is_archived;
        extensions;
      })
  |> Jsont.Object.mem "id" Zulip.Id.Channel_folder.jsont ~enc:(fun folder ->
      folder.id)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun folder -> folder.name)
  |> Jsont.Object.opt_mem "order" Jsont.int ~enc:(fun folder -> folder.order)
  |> Jsont.Object.mem "date_created"
       (Jsont.option Jsont.number)
       ~dec_absent:(fun () -> None)
       ~enc:(fun folder -> folder.date_created)
  |> Jsont.Object.mem "creator_id"
       (Jsont.option Zulip.Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun folder -> folder.creator_id)
  |> Jsont.Object.mem "description" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun folder -> folder.description)
  |> Jsont.Object.mem "rendered_description" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun folder -> folder.rendered_description)
  |> Jsont.Object.mem "is_archived" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun folder -> folder.is_archived)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun folder ->
      folder.extensions)
  |> Jsont.Object.finish

let raw folder =
  match Jsont.Json.encode' jsont folder with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let ( let* ) = Result.bind
let unit_result result = Result.map (Fun.const ()) result

let id_jsont =
  Jsont.Object.map ~kind:"Zulip channel folder ID" Fun.id
  |> Jsont.Object.mem "channel_folder_id" Zulip.Id.Channel_folder.jsont
       ~enc:Fun.id
  |> Jsont.Object.finish

let folders_jsont =
  Jsont.Object.map ~kind:"Zulip channel folders response" Fun.id
  |> Jsont.Object.mem "channel_folders" (Jsont.list jsont) ~enc:Fun.id
  |> Jsont.Object.finish

let create client ~name ?(description = "") () =
  Client.request_typed client ~method_:`POST
    ~path:"/api/v1/channel_folders/create"
    ~params:[ ("name", name); ("description", description) ]
    ~codec:id_jsont ()

let list client ?include_archived () =
  let params =
    Option.fold ~none:[]
      ~some:(fun value -> [ ("include_archived", string_of_bool value) ])
      include_archived
  in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/channel_folders"
    ~params ~codec:folders_jsont ()

let reorder client ~order =
  let* value = Codec.encode (Jsont.list Zulip.Id.Channel_folder.jsont) order in
  Client.request client ~method_:`PATCH ~path:"/api/v1/channel_folders"
    ~params:[ ("order", value) ]
    ()
  |> unit_result

let update client ~folder_id ?name ?description ?is_archived () =
  let opt key encode =
    Option.fold ~none:[] ~some:(fun value -> [ (key, encode value) ])
  in
  let params =
    opt "name" Fun.id name
    @ opt "description" Fun.id description
    @ opt "is_archived" string_of_bool is_archived
  in
  if params = [] then
    Error (Error.Invalid_request "channel-folder update has no changes")
  else
    Client.request client ~method_:`PATCH
      ~path:
        ("/api/v1/channel_folders/"
        ^ string_of_int (Zulip.Id.Channel_folder.to_int folder_id))
      ~params ()
    |> unit_result

let set_archived client ~folder_id ~archived =
  update client ~folder_id ~is_archived:archived ()
