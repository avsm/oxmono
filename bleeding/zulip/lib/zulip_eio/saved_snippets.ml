let ( let* ) = Result.bind

module Id = Zulip.Id.Saved_snippet

type t = {
  id : Id.t;
  title : string;
  content : string;
  date_created : int;
  raw : Jsont.json;
}

let id snippet = snippet.id
let title snippet = snippet.title
let content snippet = snippet.content
let date_created snippet = snippet.date_created
let raw snippet = snippet.raw
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

let date_created_jsont =
  Jsont.map ~kind:"Zulip saved-snippet creation timestamp"
    ~dec:(fun number ->
      if
        (not (Float.is_finite number))
        || Float.floor number <> number
        || number <= -9_007_199_254_740_992.
        || number >= 9_007_199_254_740_992.
      then
        Jsont.Error.msgf Jsont.Meta.none
          "saved-snippet creation timestamp must be an exact JSON integer";
      int_of_float number)
    ~enc:Float.of_int Jsont.number

let jsont =
  let make id title content date_created unknown =
    let known =
      [
        mem "id" (Jsont.Json.int (Id.to_int id));
        mem "title" (Jsont.Json.string title);
        mem "content" (Jsont.Json.string content);
        mem "date_created" (Jsont.Json.int date_created);
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    {
      id;
      title;
      content;
      date_created;
      raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
    }
  in
  Jsont.Object.map ~kind:"Zulip saved snippet" make
  |> Jsont.Object.mem "id" Id.jsont ~enc:(fun snippet -> snippet.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun snippet -> snippet.title)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun snippet ->
      snippet.content)
  |> Jsont.Object.mem "date_created" date_created_jsont ~enc:(fun snippet ->
      snippet.date_created)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun snippet ->
      unknown_without [ "id"; "title"; "content"; "date_created" ] snippet.raw)
  |> Jsont.Object.finish

type page = { saved_snippets : t list; raw : Jsont.json }

let page_jsont =
  let make saved_snippets unknown =
    let known =
      [
        mem "saved_snippets"
          (Jsont.Json.list (List.map (json_encode jsont) saved_snippets));
      ]
    in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { saved_snippets; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip saved-snippets response" make
  |> Jsont.Object.mem "saved_snippets" (Jsont.list jsont) ~enc:(fun page ->
      page.saved_snippets)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun page ->
      unknown_without [ "saved_snippets" ] page.raw)
  |> Jsont.Object.finish

type create_result = { id : Id.t; raw : Jsont.json }

let create_result_jsont =
  let make id unknown =
    let known = [ mem "saved_snippet_id" (Jsont.Json.int (Id.to_int id)) ] in
    let unknown =
      match unknown with Jsont.Object (members, _) -> members | _ -> []
    in
    { id; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) }
  in
  Jsont.Object.map ~kind:"Zulip create-saved-snippet response" make
  |> Jsont.Object.mem "saved_snippet_id" Id.jsont ~enc:(fun result -> result.id)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun result ->
      unknown_without [ "saved_snippet_id" ] result.raw)
  |> Jsont.Object.finish

let list client =
  Client.request_typed client ~method_:`GET ~path:"/api/v1/saved_snippets"
    ~codec:page_jsont ()

let create_detailed client ~title ~content =
  Client.request_typed client ~method_:`POST ~path:"/api/v1/saved_snippets"
    ~params:[ ("title", title); ("content", content) ]
    ~codec:create_result_jsont ()

let create client ~title ~content =
  let* result = create_detailed client ~title ~content in
  Ok result.id

let path id = "/api/v1/saved_snippets/" ^ string_of_int (Id.to_int id)

let edit client ~saved_snippet_id ?title ?content () =
  let field name =
    Option.fold ~none:[] ~some:(fun value -> [ (name, value) ])
  in
  Client.request client ~method_:`PATCH ~path:(path saved_snippet_id)
    ~params:(field "title" title @ field "content" content)
    ()
  |> Result.map (Fun.const ())

let delete client ~saved_snippet_id =
  Client.request client ~method_:`DELETE ~path:(path saved_snippet_id) ()
  |> Result.map (Fun.const ())
