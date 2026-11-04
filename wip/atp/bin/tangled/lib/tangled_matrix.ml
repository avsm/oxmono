(* SPDX-License-Identifier: ISC *)

let nsid = "org.recoil.tangled.matrix"

type t = {
  repo : string;
  bot : string;
  room : string;
  alias : string option;
  created_at : string;
}

let jsont =
  Jsont.Object.map ~kind:nsid (fun _type repo bot room alias created_at ->
      { repo; bot; room; alias; created_at })
  |> Jsont.Object.mem "$type" Jsont.string ~enc:(fun _ -> nsid)
  |> Jsont.Object.mem "repo" Jsont.string ~enc:(fun t -> t.repo)
  |> Jsont.Object.mem "bot" Jsont.string ~enc:(fun t -> t.bot)
  |> Jsont.Object.mem "room" Jsont.string ~enc:(fun t -> t.room)
  |> Jsont.Object.opt_mem "alias" Jsont.string ~enc:(fun t -> t.alias)
  |> Jsont.Object.mem "createdAt" Jsont.string ~enc:(fun t -> t.created_at)
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish

(* [shaped c ~server_required s] checks the shape the Matrix specification
   gives identifiers: a sigil, a nonempty local part without a colon, and a
   server name after the first colon. *)
let shaped c ~server_required s =
  String.length s >= 2
  && s.[0] = c
  && String.length s <= 255
  && String.for_all (fun c -> c > ' ' && c <> '\127') s
  &&
  match String.index_opt s ':' with
  | Some i -> i > 1 && i < String.length s - 1
  | None -> not server_required

let valid_user_id = shaped '@' ~server_required:true
let valid_room_id = shaped '!' ~server_required:false
let valid_alias = shaped '#' ~server_required:true

let server_name s =
  match String.index_opt s ':' with
  | Some i when i < String.length s - 1 ->
      Some (String.sub s (i + 1) (String.length s - i - 1))
  | _ -> None

let list api ~did =
  Tangled_api.list_records api ~did ~collection:nsid ()
  |> List.filter_map (fun (r : Tangled_api.Atproto.Repo.ListRecords.record) ->
      match Tangled_api.decode jsont r.value with
      | b
        when Tangled_api.rkey_of_uri r.uri = b.repo
             && valid_user_id b.bot && valid_room_id b.room ->
          Some b
      | _ -> None
      | exception _ -> None)
