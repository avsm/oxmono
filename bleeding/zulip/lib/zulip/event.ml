type t = { id : Id.Event.t; type_ : Event_type.t; data : Jsont.json }

let id t = t.id
let type_ t = t.type_
let data t = t.data

let jsont =
  Jsont.Object.map ~kind:"Zulip queue event" (fun id type_ data ->
      { id; type_; data })
  |> Jsont.Object.mem "id" Id.Event.jsont ~enc:id
  |> Jsont.Object.mem "type" Event_type.jsont ~enc:type_
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:data
  |> Jsont.Object.finish

let pp ppf t =
  Format.fprintf ppf "Event{id=%a; type=%a}" Id.Event.pp t.id Event_type.pp
    t.type_

let create ~id ~type_ ~data =
  let codec =
    Jsont.map ~kind:"event payload object"
      ~dec:(function
        | Jsont.Object (members, _) as data
          when List.for_all
                 (fun ((name, _), _) -> name <> "id" && name <> "type")
                 members ->
            data
        | _ ->
            Jsont.Error.msgf Jsont.Meta.none
              "expected an object without id or type")
      ~enc:Fun.id Jsont.json
  in
  Result.map (fun data -> { id; type_; data }) (Jsont.Json.decode' codec data)
