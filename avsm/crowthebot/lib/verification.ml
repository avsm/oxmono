module M = Matrix_eio
module V = M.Verification_service
module Flow = Matrix_client.Verification.Flow
module Id = Matrix_proto.Id

let affirmative text =
  match String.lowercase_ascii (String.trim text) with
  | "y" | "yes" -> true
  | _ -> false

let prompt (p : V.prompt) =
  let a, b, c = p.decimals in
  Printf.sprintf
    "Verify %s (device %s)\n\
     %s\n\
     Numbers: %d %d %d\n\
     Do all seven emoji (or all three numbers) match the other client? [y/N] "
    (Id.User_id.to_string p.their_user_id)
    (Option.fold ~none:"unknown" ~some:Id.Device_id.to_string p.their_device_id)
    (String.concat "\n"
       (List.map
          (fun (e : V.emoji) -> "  " ^ e.symbol ^ "  " ^ e.description)
          p.emoji))
    a b c

let run ~env ~client ~encryption ?private_identity ~target ~listen ?room ~ask ()
    =
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 600.)
  @@ fun () ->
  let sync = M.Sync_service.of_user ~user_id:(M.Client.user_id client) () in
  let outcome, resolved = Eio.Promise.create () in
  let selected = ref false in
  let matches user = Id.User_id.equal target user in
  let result_user = function
    | V.Verified { user_id; _ }
    | V.Publication_failed { user_id; _ }
    | V.Cancelled { user_id; _ } ->
        user_id
  in
  let verification =
    V.create ~client ~encryption ?private_identity
      ~methods:[ Matrix_client.Verification.Method.Sas_v1 ]
      ~room_members:(M.Sync_service.members sync)
      ~confirm:(fun p -> !selected && matches p.their_user_id && ask (prompt p))
      ~on_result:(fun result ->
        if
          !selected
          && matches (result_user result)
          && not (Eio.Promise.is_resolved outcome)
        then Eio.Promise.resolve resolved result)
      ()
  in
  (* A fresh key query supplies the peer device and cross-signing identity
     before an incoming request can reach the service. *)
  M.Encryption.receive_keys_query encryption
    (M.Keys.query_keys client
       ~users:[ (M.Client.user_id client, []); (target, []) ]
       ());
  let ready = ref false in
  M.Sync_service.on_response sync (fun _ _ _ -> ready := true);
  let sleep () = Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.1 in
  let work () =
    while not !ready do
      sleep ()
    done;
    if listen then
      Printf.printf "Waiting for %s to request verification...\n%!"
        (Id.User_id.to_string target)
    else begin
      selected := true;
      (match room with
      | None -> ignore (V.request verification client target)
      | Some room_id ->
          ignore
            (V.request_in_room verification client ~room_id
               ~methods:[ Matrix_client.Verification.Method.Sas_v1 ]
               target));
      Printf.printf "Verification requested from %s. Accept in your client.\n%!"
        (Id.User_id.to_string target)
    end;
    while not (Eio.Promise.is_resolved outcome) do
      List.iter
        (fun session ->
          if
            Flow.session_stage session = Flow.Requested
            && not (Flow.session_we_requested session)
          then
            if
              listen && (not !selected)
              && matches (Flow.session_their_user_id session)
              &&
              match room with
              | None -> true
              | Some room ->
                  Matrix_client.Verification.Transaction.room_id
                    (Flow.session_transaction session)
                  = Some room
            then begin
              selected := true;
              let device =
                Option.fold ~none:"unknown" ~some:Id.Device_id.to_string
                  (Flow.session_their_device_id session)
              in
              if
                ask
                  (Printf.sprintf
                     "Accept verification from %s, device %s? [y/N] "
                     (Id.User_id.to_string target)
                     device)
              then V.accept verification client session
              else
                V.cancel verification client session
                  Matrix_client.Verification.Cancel_code.User
            end
            else
              V.cancel verification client session
                Matrix_client.Verification.Cancel_code.User)
        (V.sessions verification);
      sleep ()
    done;
    Eio.Promise.await outcome
  in
  (* [first] cancels the sync fiber after success, refusal, or timeout. *)
  Eio.Fiber.first work (fun () ->
      Eio.Switch.run @@ fun sw ->
      M.Sync_service.run ~sw ~clock:(Eio.Stdenv.clock env) client sync
        ~encryption ~verification
        ~on_change:(fun _ _ -> ())
        ();
      Eio.Fiber.await_cancel ())
