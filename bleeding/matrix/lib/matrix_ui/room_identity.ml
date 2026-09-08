module Id = Matrix_proto.Id
module Base = Matrix_client.Base_client

type violation = Verification_violation | Pin_violation
type member = { user_id : Id.User_id.t; violation : violation }

type t = {
  own_user : Id.User_id.t;
  encryption : Matrix_eio.Encryption.t option;
  rooms : (string, member Observable.List.t) Hashtbl.t;
}

let create ~own_user ?encryption () =
  { own_user; encryption; rooms = Hashtbl.create 16 }

let user_id member = member.user_id
let violation member = member.violation

let members t room_id =
  let key = Id.Room_id.to_string room_id in
  match Hashtbl.find_opt t.rooms key with
  | Some members -> members
  | None ->
      let members = Observable.List.create [] in
      Hashtbl.add t.rooms key members;
      members

let equal_member left right =
  Id.User_id.equal left.user_id right.user_id
  && left.violation = right.violation

let refresh_room t state room_id =
  let target =
    match t.encryption with
    | None -> []
    | Some encryption ->
        (Base.members state room_id
        |> List.filter (fun user_id ->
            not (Id.User_id.equal user_id t.own_user))
        |> List.filter (fun user_id ->
            Matrix_eio.Encryption.identity_status encryption user_id
            = Some Matrix_eio.Encryption.Verification_violation)
        |> List.sort_uniq Id.User_id.compare
        |> List.map (fun user_id ->
            { user_id; violation = Verification_violation }))
        @ (Base.members state room_id
          |> List.filter (fun user_id ->
              not (Id.User_id.equal user_id t.own_user))
          |> List.filter
               (Matrix_eio.Encryption.identity_has_pin_violation encryption)
          |> List.filter (fun user_id ->
              Matrix_eio.Encryption.identity_status encryption user_id
              <> Some Matrix_eio.Encryption.Verification_violation)
          |> List.map (fun user_id -> { user_id; violation = Pin_violation }))
  in
  Observable.List.reconcile_by ~key:user_id ~equal:equal_member
    (members t room_id) target

let refresh t state =
  let room_ids =
    List.map (fun (room : Base.room_info) -> room.room_id) (Base.rooms state)
  in
  let known =
    Hashtbl.fold
      (fun key _ ids ->
        match Id.Room_id.of_string key with
        | Ok id -> id :: ids
        | Error _ -> ids)
      t.rooms room_ids
  in
  List.iter (refresh_room t state) (List.sort_uniq Id.Room_id.compare known)
