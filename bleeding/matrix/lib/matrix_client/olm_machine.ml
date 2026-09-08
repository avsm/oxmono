module Public = Crypto_key.Curve25519.Public

type t = {
  account : Olm_account.t;
  mutable sessions : (Public.t * Olm_session.t list) list;
}

let of_account account = { account; sessions = [] }
let create ~random () = of_account (Olm_account.create ~random ())
let account t = t.account
let peer t key = List.find_opt (fun (k, _) -> Public.equal k key) t.sessions

(* A peer's sessions, normally most recently used first, or none held. *)
let sessions_for t key = match peer t key with Some (_, l) -> l | None -> []

let selection_compare a b =
  match
    Ptime.compare
      (Olm_session.last_received_at b)
      (Olm_session.last_received_at a)
  with
  | 0 -> String.compare (Olm_session.session_id a) (Olm_session.session_id b)
  | n -> n

let lru_compare a b =
  match
    Ptime.compare (Olm_session.last_used_at b) (Olm_session.last_used_at a)
  with
  | 0 -> String.compare (Olm_session.session_id a) (Olm_session.session_id b)
  | n -> n

(* Keep the four most recently used sessions for a peer.  The session ID
   tie-break makes reloads deterministic, while filtering IDs first prevents
   duplicate records from consuming the retention budget. *)
let normalize_sessions sessions =
  let sorted = List.sort lru_compare sessions in
  let rec loop seen kept = function
    | [] -> List.rev kept
    | session :: rest ->
        let id = Olm_session.session_id session in
        if List.mem id seen then loop seen kept rest
        else if List.length kept = 4 then List.rev kept
        else loop (id :: seen) (session :: kept) rest
  in
  loop [] [] sorted

let find_olm_session t ~their_identity_key =
  match sessions_for t their_identity_key with
  | [] -> None
  | sessions -> Some (List.hd (List.sort selection_compare sessions))

(* Oldest first per peer, so that replaying this list through
   {!store_olm_session} (which conses each session onto the front) restores
   the original most-recent-first order rather than reversing it. *)
let olm_sessions t = List.concat_map (fun (_, l) -> List.rev l) t.sessions

let store t ~their_identity_key session =
  let existing = sessions_for t their_identity_key in
  t.sessions <-
    ( their_identity_key,
      normalize_sessions
        (session
        :: List.filter
             (fun old ->
               not
                 (String.equal
                    (Olm_session.session_id old)
                    (Olm_session.session_id session)))
             existing) )
    :: List.filter
         (fun (k, _) -> not (Public.equal k their_identity_key))
         t.sessions

let store_olm_session t session =
  store t ~their_identity_key:(Olm_session.their_identity_key session) session

let create_olm_session ~random t ~their_identity_key ~their_one_time_key =
  match
    Olm_session.create_outbound ~random t.account ~their_identity_key
      ~their_one_time_key
  with
  | Error _ as e -> e
  | Ok session ->
      store t ~their_identity_key session;
      Ok session

let create_inbound_session t ~their_identity_key ~ciphertext =
  match
    Olm_session.create_inbound t.account ~their_identity_key ~ciphertext
  with
  | Error _ as e -> e
  | Ok (session, plaintext) ->
      store t ~their_identity_key session;
      Ok (session, plaintext)

let encrypt_to_device ~random t ~their_identity_key ~their_one_time_key
    ~plaintext =
  let session =
    match find_olm_session t ~their_identity_key with
    | Some s -> Ok s
    | None ->
        create_olm_session ~random t ~their_identity_key ~their_one_time_key
  in
  Result.bind session (fun s ->
      Result.map
        (fun message ->
          store t ~their_identity_key s;
          message)
        (Olm_session.encrypt ~random s plaintext))

let decrypt_to_device ~random t ~their_identity_key
    (message : Olm_session.message) =
  let existing = sessions_for t their_identity_key in
  let rec try_sessions = function
    | [] -> (
        match message.message_type with
        | Olm_session.Pre_key -> (
            match
              create_inbound_session t ~their_identity_key
                ~ciphertext:message.ciphertext
            with
            | Ok (_, plaintext) -> Ok plaintext
            | Error e -> Error e)
        | Olm_session.Normal -> Error Olm_error.No_session)
    | s :: tl -> (
        match Olm_session.decrypt ~random s message with
        | Ok plaintext ->
            store t ~their_identity_key s;
            Ok plaintext
        | Error _ -> try_sessions tl)
  in
  try_sessions existing
