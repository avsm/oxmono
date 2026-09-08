module M = Matrix_client
module Id = Matrix_proto.Id

(* [matrix-chat.client] draws randomness from a caller-supplied source; a fixed
   byte stream keeps this example's transaction identifiers reproducible. *)
let secure_random =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type recorded = { meth : string; url : string; body : string option }

let record log (req : Fetch.Middleware.request) =
  let body =
    match req.body with
    | Fetch.Empty -> None
    | Fetch.String s -> Some s
    | Fetch.Stream _ -> Some "<stream>"
  in
  log :=
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      body;
    }
    :: !log

let print_log log =
  List.iter
    (fun r ->
      Printf.printf "%s %s%s\n" r.meth r.url
        (match r.body with None -> "" | Some b -> " " ^ b))
    (List.rev !log)

let client_of fetch =
  let config =
    M.Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  M.Client.create ~config ~fetch ~random:(M.Random.of_env secure_random)

(* A client that answers one scripted body per request, in order, recording
   every request it saw in [log]. *)
let scripted log bodies =
  let remaining = ref bodies in
  Fetch_mock.client (fun req ->
      record log req;
      match !remaining with
      | [] -> Fetch_mock.respond "{}" req
      | body :: rest ->
          remaining := rest;
          Fetch_mock.respond body req)

let room_id = Id.Room_id.of_string_exn "!demo:hs.example"

let () =
  Eio_mock.Backend.run @@ fun () ->
  let log = ref [] in
  let fetch =
    scripted log
      [
        {|{"user_id":"@alice:hs.example","access_token":"tok","device_id":"DEV1"}|};
        {|{"event_id":"$1:hs.example"}|};
      ]
  in
  let client = client_of fetch in
  let session =
    match M.Auth.login_password client ~user:"alice" ~password:"pw" () with
    | Ok s -> s
    | Error e ->
        Printf.printf "login failed: %s\n" (M.Error.to_string e);
        exit 1
  in
  let logged_in = M.Client.with_session client session in
  (match M.Messages.send_text logged_in ~room_id ~body:"hello" () with
  | Ok event_id -> Printf.printf "sent %s\n" (Id.Event_id.to_string event_id)
  | Error e -> Printf.printf "send failed: %s\n" (M.Error.to_string e));
  print_log log;
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        record log req;
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"not a member of the room"}|} req)
  in
  let logged_in = M.Client.with_session (client_of fetch) session in
  match M.Messages.send_text logged_in ~room_id ~body:"hello" () with
  | Ok _ -> assert false
  | Error e -> Format.printf "error: %a\n" M.Error.pp e
