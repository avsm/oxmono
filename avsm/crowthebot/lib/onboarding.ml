module V = Matrix_eio.Verification_service
module Log = Diagnostics.Log

type t = {
  store : Store.t;
  self : string;
  check_room : string -> bool;
  room_label : string -> string;
  send_admin : string -> unit;
  verify : unit -> unit;
  respond : flow_id:string -> accept:bool -> unit;
  notified : (string, unit) Hashtbl.t;
  mutable verification_started : bool;
  mutex : Eio.Mutex.t;
}

let create ~store ~self ~check_room ~room_label ~send_admin ~verify ~respond =
  { store; self; check_room; room_label; send_admin; verify; respond;
    notified = Hashtbl.create 8;
    verification_started = false; mutex = Eio.Mutex.create () }

let request t ~room =
  Persistence.locked t.mutex (fun () ->
    if not (List.mem room (Store.rooms t.store)) && t.check_room room then begin
      Store.request_room t.store room;
      if List.mem room (Store.room_requests t.store)
         && not (Hashtbl.mem t.notified room) then begin
        let label = t.room_label room in
        t.send_admin
          (Printf.sprintf
             "I've joined %s. Reply `room approve %s` to enable me there, or \
              `room reject %s`. Only approved people can use me."
             label label label);
        Hashtbl.replace t.notified room ();
        if not t.verification_started then begin
          t.verify ();
          t.verification_started <- true
        end
      end
    end)

let prompt t ~flow_id (p : V.prompt) =
  if Matrix_proto.Id.User_id.to_string p.their_user_id <> Store.admin t.store
  then t.respond ~flow_id ~accept:false
  else begin
    t.send_admin
      (Verification.prompt p ~confirmation:
        "Compare these with your Matrix client. Confirm there if they match, \
         or cancel there if they differ. Crow confirms its side automatically.");
    t.respond ~flow_id ~accept:true
  end

let result t outcome =
  let user, message = match outcome with
    | V.Verified { user_id; _ } ->
        user_id, "Device verified. Group rooms still need `room approve ROOM`."
    | V.Publication_failed { user_id; _ } ->
        user_id, "Verification publication failed. Reply `verify` to retry."
    | V.Cancelled { user_id; code } ->
        user_id, ("Verification cancelled: "
                  ^ Matrix_client.Verification.Cancel_code.reason code
                  ^ ". Reply `verify` to retry.")
  in
  if Matrix_proto.Id.User_id.to_string user = Store.admin t.store then begin
    t.send_admin message
  end

let words text =
  String.split_on_char ' ' text |> List.filter (fun s -> s <> "")

let handle t ~direct ~send (e : Engine.event) =
  let command = Address.command ~self:t.self ~mentioned:false ~direct e.body in
  let args = Option.fold ~none:[] ~some:words command in
  match args with
  | ("rooms" | "room" | "verify") :: _ ->
      if direct && e.sender = Store.admin t.store
         && (Store.person t.store e.sender).allowed
         && Store.claim t.store ~room:e.room ~event:e.id then begin
        match args with
        | [ "rooms" ] ->
            let rooms = Store.room_requests t.store in
            send (if rooms = [] then "No pending rooms."
                  else "Pending rooms:\n"
                    ^ String.concat "\n" (List.map t.room_label rooms))
        | "room" :: ("approve" | "reject" as decision) :: target
          when target <> [] ->
            let target = String.concat " " target in
            let pending = Store.room_requests t.store in
            let matches =
              if List.mem target pending then [target]
              else List.filter (fun room -> t.room_label room = target) pending
            in
            (match matches with
             | [] -> send "No pending request for that room. Use `rooms`."
             | _ :: _ :: _ ->
                 send "Several pending rooms have that name. Use the room ID."
             | [room] ->
                 if decision = "approve" && not (t.check_room room) then
                   send "Crow and the admin must still be members of that group."
                 else begin
                   Store.decide_room t.store ~actor:e.sender ~room
                     ~approve:(decision = "approve");
                   Log.info (fun m ->
                     m "Room approval room=%S actor=%S approved=%b"
                       room e.sender (decision = "approve"));
                   send ((if decision = "approve" then "Enabled " else "Rejected ")
                         ^ t.room_label room)
                 end)
        | [ "verify" ] -> t.verify ()
        | _ -> send "Usage: rooms | room approve|reject ROOM | verify"
      end;
      true
  | _ -> false
