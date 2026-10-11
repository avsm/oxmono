module V = Matrix_eio.Verification_service
module Log = Diagnostics.Log

type t = {
  store : Store.t;
  self : string;
  now : unit -> float;
  check_room : string -> bool;
  send_admin : string -> unit;
  verify : unit -> unit;
  respond : flow_id:string -> accept:bool -> unit;
  notified : (string, unit) Hashtbl.t;
  prompts : (string, float) Hashtbl.t;
  mutable verification_started : bool;
  mutex : Eio.Mutex.t;
}

let create ~store ~self ~now ~check_room ~send_admin ~verify ~respond =
  { store; self; now; check_room; send_admin; verify; respond;
    notified = Hashtbl.create 8; prompts = Hashtbl.create 2;
    verification_started = false; mutex = Eio.Mutex.create () }

let request t ~room =
  Persistence.locked t.mutex (fun () ->
    if not (List.mem room (Store.rooms t.store)) && t.check_room room then begin
      Store.request_room t.store room;
      if List.mem room (Store.room_requests t.store)
         && not (Hashtbl.mem t.notified room) then begin
        t.send_admin
          (Printf.sprintf
             "I've joined %s. Reply `room approve %s` to enable me there, or \
              `room reject %s`. Only approved people can use me."
             room room room);
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
    Hashtbl.replace t.prompts flow_id (t.now () +. 600.);
    t.send_admin
      (Verification.prompt p ~confirmation:
        ("Compare all seven emoji or all three numbers in your Matrix client. "
         ^ "Reply `verify yes " ^ flow_id ^ "` if they match, or `verify no "
         ^ flow_id ^ "` otherwise."))
  end

let result t outcome =
  let user, message = match outcome with
    | V.Verified { user_id; _ } ->
        user_id, "Device verified. Group rooms still need `room approve ID`."
    | V.Publication_failed { user_id; _ } ->
        user_id, "Verification publication failed. Reply `verify` to retry."
    | V.Cancelled { user_id; code } ->
        user_id, ("Verification cancelled: "
                  ^ Matrix_client.Verification.Cancel_code.reason code
                  ^ ". Reply `verify` to retry.")
  in
  if Matrix_proto.Id.User_id.to_string user = Store.admin t.store then begin
    Hashtbl.clear t.prompts;
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
                  else "Pending rooms:\n" ^ String.concat "\n" rooms)
        | [ "room"; ("approve" | "reject" as decision); room ] ->
            if not (List.mem room (Store.room_requests t.store)) then
              send "No pending request for that room. Use `rooms`."
            else if decision = "approve" && not (t.check_room room) then
              send "Crow and the admin must still be members of that group."
            else begin
              Store.decide_room t.store ~actor:e.sender ~room
                ~approve:(decision = "approve");
              Log.info (fun m -> m "Room approval room=%S actor=%S approved=%b"
                room e.sender (decision = "approve"));
              send ((if decision = "approve" then "Enabled " else "Rejected ")
                    ^ room)
            end
        | [ "verify" ] -> t.verify ()
        | [ "verify"; ("yes" | "no" as answer); flow_id ] ->
            (match Hashtbl.find_opt t.prompts flow_id with
             | Some until when t.now () < until ->
                 Hashtbl.remove t.prompts flow_id;
                 t.respond ~flow_id ~accept:(answer = "yes");
                 send "Verification response sent."
             | _ -> send "No current SAS prompt with that ID. Reply `verify`.")
        | _ -> send "Usage: rooms | room approve|reject ID | verify [yes|no FLOW]"
      end;
      true
  | _ -> false
