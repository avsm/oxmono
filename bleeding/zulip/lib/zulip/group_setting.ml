type t =
  | Group of Id.User_group.t
  | Direct of { members : Id.User.t list; subgroups : Id.User_group.t list }

type update = { new_ : t; old : t option }

type direct = {
  direct_members : Id.User.t list;
  direct_subgroups : Id.User_group.t list;
}

let direct_jsont =
  Jsont.Object.map ~kind:"Zulip direct group setting"
    (fun direct_members direct_subgroups ->
      { direct_members; direct_subgroups })
  |> Jsont.Object.mem "direct_members" (Jsont.list Id.User.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun s -> s.direct_members)
  |> Jsont.Object.mem "direct_subgroups"
       (Jsont.list Id.User_group.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun s -> s.direct_subgroups)
  |> Jsont.Object.finish

let jsont =
  let dec json =
    match Jsont.Json.decode' Id.User_group.jsont json with
    | Ok id -> Group id
    | Error _ -> (
        match Jsont.Json.decode' direct_jsont json with
        | Ok value ->
            Direct
              {
                members = value.direct_members;
                subgroups = value.direct_subgroups;
              }
        | Error error -> raise (Jsont.Error error))
  in
  let enc = function
    | Group id -> Jsont.Json.int (Id.User_group.to_int id)
    | Direct { members; subgroups } -> (
        match
          Jsont.Json.encode' direct_jsont
            { direct_members = members; direct_subgroups = subgroups }
        with
        | Ok json -> json
        | Error error -> raise (Jsont.Error error))
  in
  Jsont.map ~kind:"Zulip group setting" ~dec ~enc Jsont.json

let update_jsont =
  Jsont.Object.map ~kind:"Zulip group setting update" (fun new_ old ->
      { new_; old })
  |> Jsont.Object.mem "new" jsont ~enc:(fun u -> u.new_)
  |> Jsont.Object.opt_mem "old" jsont ~enc:(fun u -> u.old)
  |> Jsont.Object.finish
