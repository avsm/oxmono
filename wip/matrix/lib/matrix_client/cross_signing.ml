module Id = Matrix_proto.Id
module Key_id = Crypto_key.Key_id
module Ed25519 = Crypto_key.Ed25519

let canonical_json = Matrix_proto.Signed_json.canonical_json
let json_for_signing = Matrix_proto.Signed_json.json_for_signing
let jstring = Jsont.Json.string
let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobject mems = Jsont.Json.object' mems

let jkey_map l =
  jobject (List.map (fun (k, v) -> jmem (Key_id.to_string k) (jstring v)) l)

let jstring_list l = Jsont.Json.list (List.map jstring l)

type local_trust = Verified | Blacklisted | Ignored | Unset

type own_identity_state =
  | Never_verified
  | Verification_violation
  | Identity_verified

type role = Keys.key_usage =
  | Master
  | Self_signing
  | User_signing
  | Other of string

let ed25519 = "ed25519"
let ed25519_key_id id = Key_id.v ~algorithm:ed25519 ~id

let signing_key_id pub =
  ed25519_key_id (Crypto_key.Ed25519.Public.to_base64 pub)

(* The object a cross-signing key's signatures cover, which is everything
   but [signatures] and [unsigned]. *)
let cross_signing_signed_json (k : Keys.cross_signing_key) =
  jobject
    [
      jmem "keys" (jkey_map k.keys);
      jmem "usage" (jstring_list (List.map Keys.key_usage_to_string k.usage));
      jmem "user_id" (jstring (Id.User_id.to_string k.user_id));
    ]

let sign_json secret json =
  Ed25519.Private.sign secret (canonical_json (json_for_signing json))

let find_signature ~(signatures : Keys.signatures) ~user_id ~key_id =
  match List.find_opt (fun (u, _) -> Id.User_id.equal u user_id) signatures with
  | None -> None
  | Some (_, sigs) ->
      Option.map snd (List.find_opt (fun (k, _) -> Key_id.equal k key_id) sigs)

let add_signature ~signer_user_id ~key_id ~signature
    (signatures : Keys.signatures) : Keys.signatures =
  let mine =
    match
      List.find_opt (fun (u, _) -> Id.User_id.equal u signer_user_id) signatures
    with
    | None -> []
    | Some (_, sigs) ->
        List.filter (fun (k, _) -> not (Key_id.equal k key_id)) sigs
  in
  let others =
    List.filter
      (fun (u, _) -> not (Id.User_id.equal u signer_user_id))
      signatures
  in
  (signer_user_id, (key_id, signature) :: mine) :: others

type key = { key_role : role; key_published : Keys.cross_signing_key }

let key ~role published = { key_role = role; key_published = published }
let role t = t.key_role
let published t = t.key_published
let key_user_id t = t.key_published.Keys.user_id

(* The published key id and the key it names are read together, so a
   signature is checked against the signer's own key material rather than
   against whatever key id the signature block carries. *)
let key_entry t =
  List.find_opt
    (fun (k, _) -> String.equal (Key_id.algorithm k) ed25519)
    t.key_published.Keys.keys

let key_ed25519 t =
  match key_entry t with
  | None -> None
  | Some (_, value) -> Result.to_option (Ed25519.Public.of_base64 value)

let verify_signed_json ~public_key ~signature ~json =
  Ed25519.Public.verify public_key ~signature
    ~data:(canonical_json (json_for_signing json))

let verify_key ~signer ~signed =
  match key_entry signer with
  | None -> false
  | Some (key_id, value) -> (
      match Ed25519.Public.of_base64 value with
      | Error (`Msg _) -> false
      | Ok public_key -> (
          match
            find_signature ~signatures:signed.key_published.Keys.signatures
              ~user_id:signer.key_published.Keys.user_id ~key_id
          with
          | None -> false
          | Some signature ->
              verify_signed_json ~public_key ~signature
                ~json:(cross_signing_signed_json signed.key_published)))

type private_identity = {
  identity_user_id : Id.User_id.t;
  mutable master : Ed25519.Private.t option;
  mutable self_signing : Ed25519.Private.t option;
  mutable user_signing : Ed25519.Private.t option;
}

let create_private_identity ~user_id =
  {
    identity_user_id = user_id;
    master = None;
    self_signing = None;
    user_signing = None;
  }

let generate_private_keys ~random t =
  let generate () = fst (Ed25519.generate ~random ()) in
  t.master <- Some (generate ());
  t.self_signing <- Some (generate ());
  t.user_signing <- Some (generate ())

let identity_user_id t = t.identity_user_id
let master_secret t = t.master
let self_signing_secret t = t.self_signing
let user_signing_secret t = t.user_signing
let master_public t = Option.map Ed25519.Private.public t.master
let set_user_signing_secret t secret = t.user_signing <- secret

type private_identity_import_error =
  | Invalid_secret of role * string
  | Public_key_mismatch of role

let pp_private_identity_import_error ppf = function
  | Invalid_secret (role, reason) ->
      Format.fprintf ppf "invalid %s cross-signing secret: %s"
        (Keys.key_usage_to_string role)
        reason
  | Public_key_mismatch role ->
      Format.fprintf ppf "%s cross-signing secret has the wrong public key"
        (Keys.key_usage_to_string role)

let private_identity_of_secrets ~user_id ~expected_master ~expected_self_signing
    ~expected_user_signing ~master ~self_signing ~user_signing =
  (* Decode every supplied seed before checking any public key.  In particular,
     don't install a prefix of the keys if a later secret is malformed or does
     not belong to the published identity.  This mirrors the Rust SDK's
     [PrivateCrossSigningIdentity::import_secrets] transaction. *)
  let decode role = function
    | None -> Ok None
    | Some encoded -> (
        match Matrix_proto.Base64.decode encoded with
        | Error (`Msg reason) -> Error (Invalid_secret (role, reason))
        | Ok bytes -> (
            match Ed25519.Private.of_bytes bytes with
            | Error (`Msg reason) -> Error (Invalid_secret (role, reason))
            | Ok secret -> Ok (Some secret)))
  in
  let decoded_master = decode Master master in
  let decoded_self_signing = decode Self_signing self_signing in
  let decoded_user_signing = decode User_signing user_signing in
  match (decoded_master, decoded_self_signing, decoded_user_signing) with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
  | Ok master, Ok self_signing, Ok user_signing -> (
      let check role expected = function
        | None -> Ok None
        | Some secret ->
            let actual = Ed25519.Private.public secret in
            if Ed25519.Public.equal expected actual then Ok (Some secret)
            else Error (Public_key_mismatch role)
      in
      let checked_master = check Master expected_master master in
      let checked_self_signing =
        check Self_signing expected_self_signing self_signing
      in
      let checked_user_signing =
        check User_signing expected_user_signing user_signing
      in
      match (checked_master, checked_self_signing, checked_user_signing) with
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
      | Ok master, Ok self_signing, Ok user_signing ->
          Ok { identity_user_id = user_id; master; self_signing; user_signing })

let private_identity_of_secrets_unchecked ~user_id ~master ~self_signing
    ~user_signing =
  let decode role encoded =
    match Matrix_proto.Base64.decode encoded with
    | Error (`Msg reason) -> Error (Invalid_secret (role, reason))
    | Ok bytes -> (
        match Ed25519.Private.of_bytes bytes with
        | Error (`Msg reason) -> Error (Invalid_secret (role, reason))
        | Ok secret -> Ok (Some secret))
  in
  (* Decode everything before constructing the identity.  A malformed later
     seed therefore cannot expose a partially imported key hierarchy. *)
  let decoded_master = decode Master master in
  let decoded_self_signing = decode Self_signing self_signing in
  let decoded_user_signing = decode User_signing user_signing in
  match (decoded_master, decoded_self_signing, decoded_user_signing) with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
  | Ok master, Ok self_signing, Ok user_signing ->
      Ok { identity_user_id = user_id; master; self_signing; user_signing }

let sign_cross_signing_key ~signer ~signer_user_id (k : Keys.cross_signing_key)
    =
  let signature = sign_json signer (cross_signing_signed_json k) in
  let key_id = signing_key_id (Ed25519.Private.public signer) in
  {
    k with
    Keys.signatures =
      add_signature ~signer_user_id ~key_id ~signature k.Keys.signatures;
  }

type upload = {
  master_key : Keys.cross_signing_key;
  self_signing_key : Keys.cross_signing_key;
  user_signing_key : Keys.cross_signing_key;
}

let published_of_secret ~user_id ~role secret : Keys.cross_signing_key =
  let value = Ed25519.Public.to_base64 (Ed25519.Private.public secret) in
  {
    Keys.user_id;
    usage = [ role ];
    keys = [ (ed25519_key_id value, value) ];
    signatures = [];
  }

let build_upload t =
  match (t.master, t.self_signing, t.user_signing) with
  | Some master, Some self_signing, Some user_signing ->
      let user_id = t.identity_user_id in
      let sign k =
        sign_cross_signing_key ~signer:master ~signer_user_id:user_id k
      in
      Some
        {
          master_key = published_of_secret ~user_id ~role:Master master;
          (* The self- and user-signing keys are worthless to a peer until
             the master key has signed them. *)
          self_signing_key =
            sign (published_of_secret ~user_id ~role:Self_signing self_signing);
          user_signing_key =
            sign (published_of_secret ~user_id ~role:User_signing user_signing);
        }
  | _ -> None

type device = {
  dev_keys : Keys.device_keys;
  mutable dev_local_trust : local_trust;
  mutable dev_trusted : bool;
}

let create_device keys =
  { dev_keys = keys; dev_local_trust = Unset; dev_trusted = false }

let device_keys t = t.dev_keys
let device_id t = t.dev_keys.Keys.device_id
let device_user_id t = t.dev_keys.Keys.user_id
let device_algorithms t = t.dev_keys.Keys.algorithms
let device_public_keys t = t.dev_keys.Keys.keys
let device_signatures t = t.dev_keys.Keys.signatures
let device_local_trust t = t.dev_local_trust
let set_device_local_trust t trust = t.dev_local_trust <- trust
let device_cross_signing_trusted t = t.dev_trusted

let device_ed25519 t =
  let key_id = Key_id.of_device ~algorithm:ed25519 (device_id t) in
  match
    List.find_opt (fun (k, _) -> Key_id.equal k key_id) t.dev_keys.Keys.keys
  with
  | None -> None
  | Some (_, value) -> Result.to_option (Ed25519.Public.of_base64 value)

let device_signed_json t =
  let fields =
    [
      jmem "algorithms" (jstring_list (device_algorithms t));
      jmem "device_id" (jstring (Id.Device_id.to_string (device_id t)));
      jmem "keys" (jkey_map (device_public_keys t));
      jmem "user_id" (jstring (Id.User_id.to_string (device_user_id t)));
    ]
  in
  let fields =
    match (device_keys t).Keys.dehydrated with
    | None -> fields
    | Some value -> jmem "dehydrated" (Jsont.Json.bool value) :: fields
  in
  jobject fields

let verify_device_signature ~self_signing_key ~device =
  let user_id = device_user_id device in
  if not (Id.User_id.equal (key_user_id self_signing_key) user_id) then false
  else
    match key_entry self_signing_key with
    | None -> false
    | Some (key_id, value) -> (
        match Ed25519.Public.of_base64 value with
        | Error (`Msg _) -> false
        | Ok public_key -> (
            match
              find_signature ~signatures:(device_signatures device) ~user_id
                ~key_id
            with
            | None -> false
            | Some signature ->
                verify_signed_json ~public_key ~signature
                  ~json:(device_signed_json device)))

let update_device_trust ~self_signing_key device =
  device.dev_trusted <- verify_device_signature ~self_signing_key ~device;
  device.dev_trusted

let is_device_verified device =
  match device.dev_local_trust with
  | Blacklisted -> false
  | Verified -> true
  | Ignored | Unset -> device.dev_trusted

let sign_device ~signer ~signer_user_id device =
  let signature = sign_json signer (device_signed_json device) in
  let key_id = signing_key_id (Ed25519.Private.public signer) in
  {
    device with
    dev_keys =
      {
        device.dev_keys with
        Keys.signatures =
          add_signature ~signer_user_id ~key_id ~signature
            (device_signatures device);
      };
  }

let sign_device_keys ~signer ~signer_user_id keys =
  device_keys (sign_device ~signer ~signer_user_id (create_device keys))

type own_identity = {
  own_user_id : Id.User_id.t;
  own_master_key : key;
  own_self_signing_key : key;
  own_user_signing_key : key;
  own_state : own_identity_state;
}

let own_identity ~user_id ~master_key ~self_signing_key ~user_signing_key
    ?(state = Never_verified) () =
  {
    own_user_id = user_id;
    own_master_key = master_key;
    own_self_signing_key = self_signing_key;
    own_user_signing_key = user_signing_key;
    own_state = state;
  }

let own_user_id t = t.own_user_id
let own_master_key t = t.own_master_key
let own_self_signing_key t = t.own_self_signing_key
let own_user_signing_key t = t.own_user_signing_key
let own_identity_state t = t.own_state
let is_own_identity_verified t = t.own_state = Identity_verified

type other_identity = {
  other_user_id : Id.User_id.t;
  other_master_key : key;
  other_self_signing_key : key;
  mutable pinned_master_key : key option;
}

let other_identity ~user_id ~master_key ~self_signing_key ?pinned_master_key ()
    =
  {
    other_user_id = user_id;
    other_master_key = master_key;
    other_self_signing_key = self_signing_key;
    pinned_master_key;
  }

let other_user_id t = t.other_user_id
let other_master_key t = t.other_master_key
let other_self_signing_key t = t.other_self_signing_key
let pinned_master_key t = t.pinned_master_key

let is_other_identity_verified ~our_user_signing_key t =
  verify_key ~signer:our_user_signing_key ~signed:t.other_master_key

let pin_master_key t = t.pinned_master_key <- Some t.other_master_key

let has_identity_changed t =
  match t.pinned_master_key with
  | None -> false
  | Some pinned -> (
      match (key_ed25519 t.other_master_key, key_ed25519 pinned) with
      | Some a, Some b -> not (Ed25519.Public.equal a b)
      | _ -> true)

let verify_master_trust ~ours ~theirs =
  verify_key ~signer:ours.own_master_key ~signed:ours.own_user_signing_key
  && verify_key ~signer:ours.own_user_signing_key
       ~signed:theirs.other_master_key
  && verify_key ~signer:theirs.other_master_key
       ~signed:theirs.other_self_signing_key

let verify_device_trust_chain ~ours ~theirs ~device =
  verify_master_trust ~ours ~theirs
  && verify_device_signature ~self_signing_key:theirs.other_self_signing_key
       ~device
