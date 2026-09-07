type auth_mode =
  | Signature_auth of Apubt.Signing.t
  | OAuth_auth of { instance : string; token : string }

type t = {
  actor_uri : string;
  auth : auth_mode;
  public_key : X509.Public_key.t option;
}

let resolve ?actor_uri ?key_id ?pem ?(format = `Rfc9421) session =
  let ( let* ) = Result.bind in
  let actor = match actor_uri, session with
    | Some actor, _ -> Some actor
    | None, Some s -> Some s.Apub_auth_session.actor_uri
    | None, None -> None in
  let* actor_uri = match actor with
    | None -> Error "No actor configured. Use apub auth login or auth setup."
    | Some actor -> (match Fetch.Middleware.Url.of_string actor with
        | Ok _ -> Ok actor | Error _ -> Error "Actor must be an absolute HTTP(S) URI") in
  let same_actor = match session with
    | None -> true
    | Some s -> Uri.equal (Uri.canonicalize (Uri.of_string actor_uri))
        (Uri.canonicalize (Uri.of_string s.actor_uri)) in
  let* () = if not same_actor && (Option.is_none pem || Option.is_none key_id)
    then Error "Changing the saved actor requires both an explicit key file and key ID"
    else Ok () in
  match key_id, pem, session with
  | None, None, Some s when Apub_auth_session.has_oauth s ->
      Ok { actor_uri; public_key = None;
           auth = OAuth_auth { instance = Option.get s.oauth_instance;
                               token = Option.get s.oauth_access_token } }
  | _ ->
      let key_id = match key_id, session with
        | Some _ as value, _ -> value | None, Some s -> s.key_id | _ -> None in
      let pem = match pem, session with
        | Some _ as value, _ -> value | None, Some s -> s.private_key_pem | _ -> None in
      match key_id, pem with
      | Some key_id, Some pem ->
          let* () = match Fetch.Middleware.Url.of_string key_id with
            | Ok _ -> Ok () | Error _ -> Error "Key ID must be an absolute HTTP(S) URI" in
          let* private_key = match X509.Private_key.decode_pem pem with
            | Ok (`RSA _ as key) -> Ok key
            | Ok _ -> Error "Expected an RSA private key"
            | Error (`Msg error) -> Error error in
          let* signing = Apubt.Signing.from_pem ~format ~key_id ~pem () in
          Ok { actor_uri; auth = Signature_auth signing;
               public_key = Some (X509.Private_key.public private_key) }
      | _ -> Error "Incomplete signing credentials: configure both a key file and key ID"

let validate_actor t actor =
  let module P = Apubt.Proto in
  let fail message = raise (Apubt.E (Apubt.Error.Invalid_actor message)) in
  if not (Uriz.equal (Uriz.of_string_exn t.actor_uri) (P.Actor.id actor)) then
    fail "Fetched actor identity does not match the selected actor";
  match t.auth, t.public_key with
  | Signature_auth signing, Some expected ->
      (match P.Actor.public_key actor with
       | None -> fail "Actor does not advertise a signing public key"
       | Some key ->
           if not (Uriz.equal (P.Public_key.owner key) (P.Actor.id actor)) ||
              not (Uriz.equal (P.Public_key.id key) (Uriz.of_string_exn (Apubt.Signing.key_id signing))) then
             fail "Signing key ID or owner does not match the actor";
           match X509.Public_key.decode_pem (P.Public_key.public_key_pem key) with
           | Ok actual when X509.Public_key.fingerprint actual = X509.Public_key.fingerprint expected -> ()
           | _ -> fail "Private key does not match the actor's advertised public key")
  | _ -> ()
