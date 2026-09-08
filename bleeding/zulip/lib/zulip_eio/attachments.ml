let ( let* ) = Result.bind

module Id = Zulip.Id.Attachment

let mem name value = ((name, Jsont.Meta.none), value)

let json_encode codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let unknown_without known = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.filter (fun ((name, _), _) -> not (List.mem name known)) members,
          meta )
  | _ -> Jsont.Json.object' []

type t = {
  id : Id.t;
  name : string;
  path_id : string;
  size : int;
  create_time : int;
  message_ids : Zulip.Id.Message.t list;
  raw : Jsont.json;
}

let jsont =
  let make id name path_id size create_time message_ids unknown =
    let known =
      [
        mem "id" (Jsont.Json.int (Id.to_int id));
        mem "name" (Jsont.Json.string name);
        mem "path_id" (Jsont.Json.string path_id);
        mem "size" (Jsont.Json.int size);
        mem "create_time" (Jsont.Json.int create_time);
        mem "message_ids"
          (Jsont.Json.list
             (List.map
                (fun id -> Jsont.Json.int (Zulip.Id.Message.to_int id))
                message_ids));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      id;
      name;
      path_id;
      size;
      create_time;
      message_ids;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip attachment" make
  |> Jsont.Object.mem "id" Id.jsont ~enc:(fun (a : t) -> a.id)
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (a : t) -> a.name)
  |> Jsont.Object.mem "path_id" Jsont.string ~enc:(fun (a : t) -> a.path_id)
  |> Jsont.Object.mem "size" Jsont.int ~enc:(fun (a : t) -> a.size)
  |> Jsont.Object.mem "create_time" Jsont.int ~enc:(fun (a : t) ->
      a.create_time)
  |> Jsont.Object.mem "message_ids" (Jsont.list Zulip.Id.Message.jsont)
       ~enc:(fun (a : t) -> a.message_ids)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (a : t) ->
      unknown_without
        [ "id"; "name"; "path_id"; "size"; "create_time"; "message_ids" ]
        a.raw)
  |> Jsont.Object.finish

type page = { attachments : t list; upload_space_used : int; raw : Jsont.json }

let page_jsont =
  let make attachments upload_space_used unknown =
    let known =
      [
        mem "attachments"
          (Jsont.Json.list
             (List.map
                (fun attachment -> json_encode jsont attachment)
                attachments));
        mem "upload_space_used" (Jsont.Json.int upload_space_used);
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      attachments;
      upload_space_used;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip attachments response" make
  |> Jsont.Object.mem "attachments" (Jsont.list jsont) ~enc:(fun (p : page) ->
      p.attachments)
  |> Jsont.Object.mem "upload_space_used" Jsont.int ~enc:(fun (p : page) ->
      p.upload_space_used)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (p : page) ->
      unknown_without [ "attachments"; "upload_space_used" ] p.raw)
  |> Jsont.Object.finish

let list client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/attachments"
    ~codec:page_jsont ()

let delete client ~attachment_id =
  Client.request client ~method_:`DELETE
    ~path:("/api/v1/attachments/" ^ string_of_int (Id.to_int attachment_id))
    ()
  |> Result.map (Fun.const ())

type upload_result = { uri : string; raw : Jsont.json }

let uri_jsont =
  Jsont.Object.map ~kind:"Zulip upload response" (fun uri unknown ->
      let known = [ mem "uri" (Jsont.Json.string uri) ] in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { uri; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun (r : upload_result) -> r.uri)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : upload_result) ->
      unknown_without [ "uri" ] r.raw)
  |> Jsont.Object.finish

let upload_file_detailed client ~filename ~content_type content =
  let* json =
    Client.multipart client ~path:"/api/v1/user_uploads"
      [ Fetch.Form.file ~name:"file" ~filename ~content_type content ]
  in
  Codec.decode uri_jsont json

let upload_file client ~filename ~content_type content =
  let* result = upload_file_detailed client ~filename ~content_type content in
  Ok result.uri

let upload_stream_detailed client ~filename ~content_type ?length source =
  if Option.exists (fun n -> n < 0L) length then
    Error (Error.Invalid_request "Upload length must be nonnegative")
  else
    let* json =
      Client.multipart client ~path:"/api/v1/user_uploads"
        [
          Fetch.Form.stream ~name:"file" ~filename ~content_type ?length source;
        ]
    in
    Codec.decode uri_jsont json

let upload_stream client ~filename ~content_type ?length source =
  let* result =
    upload_stream_detailed client ~filename ~content_type ?length source
  in
  Ok result.uri

type temporary_url_result = { url : string; raw : Jsont.json }

let url_jsont =
  Jsont.Object.map ~kind:"Zulip temporary upload URL response"
    (fun url unknown ->
      let known = [ mem "url" (Jsont.Json.string url) ] in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { url; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun (r : temporary_url_result) ->
      r.url)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : temporary_url_result) -> unknown_without [ "url" ] r.raw)
  |> Jsont.Object.finish

let path ~base ~realm_id ~filename =
  let filename =
    filename |> String.split_on_char '/'
    |> List.filter (fun segment -> segment <> "")
    |> List.map Client.path_segment
    |> String.concat "/"
  in
  base ^ "/" ^ Client.path_segment (string_of_int realm_id) ^ "/" ^ filename

let temporary_url_detailed client ~realm_id ~filename =
  Client.request_typed client ~method_:`GET
    ~path:(path ~base:"/api/v1/user_uploads" ~realm_id ~filename)
    ~codec:url_jsont ()

let temporary_url client ~realm_id ~filename =
  let* result = temporary_url_detailed client ~realm_id ~filename in
  Ok result.url

type thumbnail_status = { has_thumbnail : bool; raw : Jsont.json }

let thumbnail_jsont =
  Jsont.Object.map ~kind:"Zulip thumbnail status response"
    (fun has_thumbnail unknown ->
      let known = [ mem "has_thumbnail" (Jsont.Json.bool has_thumbnail) ] in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { has_thumbnail; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "has_thumbnail" Jsont.bool
       ~enc:(fun (r : thumbnail_status) -> r.has_thumbnail)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : thumbnail_status) ->
         unknown_without [ "has_thumbnail" ] r.raw)
  |> Jsont.Object.finish

let thumbnail_status client ~realm_id ~filename =
  Client.request_typed client ~method_:`GET
    ~path:(path ~base:"/api/v1/thumbnail/status" ~realm_id ~filename)
    ~codec:thumbnail_jsont ()

let has_thumbnail client ~realm_id ~filename =
  let* result = thumbnail_status client ~realm_id ~filename in
  Ok result.has_thumbnail
