module Ev = Matrix_proto.Event
module Id = Matrix_proto.Id
module Key_id = Crypto_key.Key_id
module Curve25519 = Crypto_key.Curve25519
module Cancel_code = Verification_base.Cancel_code
module Transaction = Verification_base.Transaction
module Message = Verification_base.Message

let b64e s = Matrix_proto.Base64.encode s
let canonical_json = Matrix_proto.Signed_json.canonical_json

module Mac_method = struct
  type t = Hkdf_hmac_sha256_v2 | Hkdf_hmac_sha256

  let to_string = function
    | Hkdf_hmac_sha256_v2 -> "hkdf-hmac-sha256.v2"
    | Hkdf_hmac_sha256 -> "hkdf-hmac-sha256"

  let of_string = function
    | "hkdf-hmac-sha256.v2" -> Some Hkdf_hmac_sha256_v2
    | "hkdf-hmac-sha256" -> Some Hkdf_hmac_sha256
    | _ -> None

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let all = [ Hkdf_hmac_sha256_v2; Hkdf_hmac_sha256 ]
end

type emoji = { number : int; symbol : string; description : string }

let emoji number symbol description = { number; symbol; description }

let emoji_table =
  [|
    emoji 0 "\xf0\x9f\x90\xb6" "Dog";
    emoji 1 "\xf0\x9f\x90\xb1" "Cat";
    emoji 2 "\xf0\x9f\xa6\x81" "Lion";
    emoji 3 "\xf0\x9f\x90\x8e" "Horse";
    emoji 4 "\xf0\x9f\xa6\x84" "Unicorn";
    emoji 5 "\xf0\x9f\x90\xb7" "Pig";
    emoji 6 "\xf0\x9f\x90\x98" "Elephant";
    emoji 7 "\xf0\x9f\x90\xb0" "Rabbit";
    emoji 8 "\xf0\x9f\x90\xbc" "Panda";
    emoji 9 "\xf0\x9f\x90\x93" "Rooster";
    emoji 10 "\xf0\x9f\x90\xa7" "Penguin";
    emoji 11 "\xf0\x9f\x90\xa2" "Turtle";
    emoji 12 "\xf0\x9f\x90\x9f" "Fish";
    emoji 13 "\xf0\x9f\x90\x99" "Octopus";
    emoji 14 "\xf0\x9f\xa6\x8b" "Butterfly";
    emoji 15 "\xf0\x9f\x8c\xb7" "Flower";
    emoji 16 "\xf0\x9f\x8c\xb3" "Tree";
    emoji 17 "\xf0\x9f\x8c\xb5" "Cactus";
    emoji 18 "\xf0\x9f\x8d\x84" "Mushroom";
    emoji 19 "\xf0\x9f\x8c\x8f" "Globe";
    emoji 20 "\xf0\x9f\x8c\x99" "Moon";
    emoji 21 "\xe2\x98\x81\xef\xb8\x8f" "Cloud";
    emoji 22 "\xf0\x9f\x94\xa5" "Fire";
    emoji 23 "\xf0\x9f\x8d\x8c" "Banana";
    emoji 24 "\xf0\x9f\x8d\x8e" "Apple";
    emoji 25 "\xf0\x9f\x8d\x93" "Strawberry";
    emoji 26 "\xf0\x9f\x8c\xbd" "Corn";
    emoji 27 "\xf0\x9f\x8d\x95" "Pizza";
    emoji 28 "\xf0\x9f\x8e\x82" "Cake";
    emoji 29 "\xe2\x9d\xa4\xef\xb8\x8f" "Heart";
    emoji 30 "\xf0\x9f\x98\x80" "Smiley";
    emoji 31 "\xf0\x9f\xa4\x96" "Robot";
    emoji 32 "\xf0\x9f\x8e\xa9" "Hat";
    emoji 33 "\xf0\x9f\x91\x93" "Glasses";
    emoji 34 "\xf0\x9f\x94\xa7" "Spanner";
    emoji 35 "\xf0\x9f\x8e\x85" "Santa";
    emoji 36 "\xf0\x9f\x91\x8d" "Thumbs Up";
    emoji 37 "\xe2\x98\x82\xef\xb8\x8f" "Umbrella";
    emoji 38 "\xe2\x8c\x9b" "Hourglass";
    emoji 39 "\xe2\x8f\xb0" "Clock";
    emoji 40 "\xf0\x9f\x8e\x81" "Gift";
    emoji 41 "\xf0\x9f\x92\xa1" "Light Bulb";
    emoji 42 "\xf0\x9f\x93\x95" "Book";
    emoji 43 "\xe2\x9c\x8f\xef\xb8\x8f" "Pencil";
    emoji 44 "\xf0\x9f\x93\x8e" "Paperclip";
    emoji 45 "\xe2\x9c\x82\xef\xb8\x8f" "Scissors";
    emoji 46 "\xf0\x9f\x94\x92" "Lock";
    emoji 47 "\xf0\x9f\x94\x91" "Key";
    emoji 48 "\xf0\x9f\x94\xa8" "Hammer";
    emoji 49 "\xe2\x98\x8e\xef\xb8\x8f" "Telephone";
    emoji 50 "\xf0\x9f\x8f\x81" "Flag";
    emoji 51 "\xf0\x9f\x9a\x82" "Train";
    emoji 52 "\xf0\x9f\x9a\xb2" "Bicycle";
    emoji 53 "\xe2\x9c\x88\xef\xb8\x8f" "Aeroplane";
    emoji 54 "\xf0\x9f\x9a\x80" "Rocket";
    emoji 55 "\xf0\x9f\x8f\x86" "Trophy";
    emoji 56 "\xe2\x9a\xbd" "Ball";
    emoji 57 "\xf0\x9f\x8e\xb8" "Guitar";
    emoji 58 "\xf0\x9f\x8e\xba" "Trumpet";
    emoji 59 "\xf0\x9f\x94\x94" "Bell";
    emoji 60 "\xe2\x9a\x93" "Anchor";
    emoji 61 "\xf0\x9f\x8e\xa7" "Headphones";
    emoji 62 "\xf0\x9f\x93\x81" "Folder";
    emoji 63 "\xf0\x9f\x93\x8c" "Pin";
  |]

let emoji_indices bytes =
  if String.length bytes < 6 then []
  else
    let b i = Char.code bytes.[i] in
    [
      (b 0 lsr 2) land 0x3f;
      ((b 0 land 0x03) lsl 4) lor ((b 1 lsr 4) land 0x0f);
      ((b 1 land 0x0f) lsl 2) lor ((b 2 lsr 6) land 0x03);
      b 2 land 0x3f;
      (b 3 lsr 2) land 0x3f;
      ((b 3 land 0x03) lsl 4) lor ((b 4 lsr 4) land 0x0f);
      ((b 4 land 0x0f) lsl 2) lor ((b 5 lsr 6) land 0x03);
    ]

let emoji_of_bytes bytes =
  List.map (fun i -> emoji_table.(i)) (emoji_indices bytes)

let decimals_of_bytes bytes =
  if String.length bytes < 5 then (0, 0, 0)
  else
    let b i = Char.code bytes.[i] in
    let first = ((b 0 lsl 5) lor (b 1 lsr 3)) + 1000 in
    let second =
      (((b 1 land 0x07) lsl 10) lor (b 2 lsl 2) lor (b 3 lsr 6)) + 1000
    in
    let third = (((b 3 land 0x3f) lsl 7) lor (b 4 lsr 1)) + 1000 in
    (first, second, third)

let key_agreement_curve25519_hkdf = "curve25519-hkdf-sha256"
let hash_sha256 = "sha256"
let sas_decimal = "decimal"
let sas_emoji = "emoji"
let our_key_agreement_protocols = [ key_agreement_curve25519_hkdf ]
let our_hashes = [ hash_sha256 ]
let our_short_authentication_string = [ sas_decimal; sas_emoji ]
let default_timeout_ms = 600_000L
let pick ours theirs = List.find_opt (fun x -> List.mem x theirs) ours
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let hmac_sha256 ~key s = Digestif.SHA256.(to_raw_string (hmac_string ~key s))

let hkdf ~info ~ikm len =
  let prk = Hkdf.extract ~hash:`SHA256 ~salt:"" ikm in
  Hkdf.expand ~hash:`SHA256 ~prk ~info len

let commitment ~public_key ~start_json =
  b64e
    (sha256
       (Curve25519.Public.to_base64 public_key ^ canonical_json start_json))

(* [hkdf-hmac-sha256] reproduces a libolm bug in which the input buffer was
   reused as the output buffer, so the encoder overran its own input after
   the first few bytes. It is kept only for peers that do not offer
   [hkdf-hmac-sha256.v2]. *)
let mac_base64_libolm mac =
  let out = Buffer.create 43 in
  Buffer.add_string out (b64e (String.sub mac 0 3));
  let bytes_from_mac = ref 2 in
  List.iter
    (fun i ->
      let from_mac = String.sub mac (i - !bytes_from_mac) !bytes_from_mac in
      let current = Buffer.contents out in
      let take = 3 - !bytes_from_mac in
      let from_out = String.sub current (String.length current - take) take in
      Buffer.add_string out (b64e (from_out ^ from_mac));
      decr bytes_from_mac)
    [ 6; 9 ];
  List.iter
    (fun i ->
      let current = Buffer.contents out in
      Buffer.add_string out (b64e (String.sub current i 3)))
    [ 9; 12; 15; 18; 21; 24; 27 ];
  let current = Buffer.contents out in
  Buffer.add_string out (b64e (String.sub current 30 2));
  Buffer.contents out

let encode_mac ~mac_method raw =
  match (mac_method : Mac_method.t) with
  | Hkdf_hmac_sha256 -> mac_base64_libolm raw
  | Hkdf_hmac_sha256_v2 -> b64e raw

let calculate_mac ~shared ~mac_method ~info input =
  let key = hkdf ~info ~ikm:shared 32 in
  encode_mac ~mac_method (hmac_sha256 ~key input)

type identity = {
  user_id : Id.User_id.t;
  device_id : Id.Device_id.t;
  device_key : Crypto_key.Ed25519.Public.t;
  master_key : Crypto_key.Ed25519.Public.t option;
}

let identity ~user_id ~device_id ~device_key ?master_key () =
  { user_id; device_id; device_key; master_key }

let user_string i = Id.User_id.to_string i.user_id
let device_string i = Id.Device_id.to_string i.device_id
let ed25519 = "ed25519"

let identity_keys i =
  let device = Crypto_key.Ed25519.Public.to_base64 i.device_key in
  (Key_id.of_device ~algorithm:ed25519 i.device_id, device)
  ::
  (match i.master_key with
  | Some m ->
      let value = Crypto_key.Ed25519.Public.to_base64 m in
      [ (Key_id.v ~algorithm:ed25519 ~id:value, value) ]
  | None -> [])

type stage =
  | Start_sent
  | Accept_sent
  | Key_sent
  | Sas_ready
  | Mac_sent
  | Waiting_done
  | Done
  | Cancelled of Cancel_code.t

type t = {
  transaction : Transaction.t;
  we_started : bool;
  ours : identity;
  theirs : identity;
  started_at : Ev.Timestamp.t;
  timeout : int64;
  stage : stage;
  offered_mac_methods : Mac_method.t list;
  key_agreement_protocol : string;
  hash : string;
  mac_method : Mac_method.t;
  short_authentication_string : string list;
  start_json : Jsont.json;
  our_secret : Curve25519.Secret.t;
  our_public : Curve25519.Public.t;
  their_public : Curve25519.Public.t option;
  their_commitment : string option;
  shared : string option;
  we_confirmed : bool;
  their_mac_verified : bool;
  we_sent_done : bool;
  they_sent_done : bool;
  verified_keys : (Key_id.t * string) list;
}

type step = { sas : t; send : Message.t list }

let stage t = t.stage
let transaction t = t.transaction
let flow_id t = Transaction.id t.transaction
let we_started t = t.we_started
let our_identity t = t.ours
let their_identity t = t.theirs
let mac_method t = t.mac_method
let verified_keys t = t.verified_keys
let is_done t = t.stage = Done
let cancel_code t = match t.stage with Cancelled c -> Some c | _ -> None
let is_cancelled t = match t.stage with Cancelled _ -> true | _ -> false

let pp_stage ppf = function
  | Start_sent -> Format.pp_print_string ppf "start-sent"
  | Accept_sent -> Format.pp_print_string ppf "accept-sent"
  | Key_sent -> Format.pp_print_string ppf "key-sent"
  | Sas_ready -> Format.pp_print_string ppf "sas-ready"
  | Mac_sent -> Format.pp_print_string ppf "mac-sent"
  | Waiting_done -> Format.pp_print_string ppf "waiting-done"
  | Done -> Format.pp_print_string ppf "done"
  | Cancelled c -> Format.fprintf ppf "cancelled(%a)" Cancel_code.pp c

let pp ppf t =
  Format.fprintf ppf "@[<h>sas %a %a@]" Transaction.pp t.transaction pp_stage
    t.stage

(* The info string for the SAS bytes is ordered by who sent the start. *)
let sas_info t their_public =
  let side i key =
    Printf.sprintf "%s|%s|%s" (user_string i) (device_string i) key
  in
  let ours = side t.ours (Curve25519.Public.to_base64 t.our_public) in
  let theirs = side t.theirs (Curve25519.Public.to_base64 their_public) in
  let first, second = if t.we_started then (ours, theirs) else (theirs, ours) in
  Printf.sprintf "MATRIX_KEY_VERIFICATION_SAS|%s|%s|%s" first second (flow_id t)

let mac_info ~sender ~receiver ~flow_id =
  Printf.sprintf "MATRIX_KEY_VERIFICATION_MAC%s%s%s%s%s" (user_string sender)
    (device_string sender) (user_string receiver) (device_string receiver)
    flow_id

let sas_bytes t =
  match (t.shared, t.their_public) with
  | Some shared, Some their_public ->
      Some (hkdf ~info:(sas_info t their_public) ~ikm:shared 6)
  | _ -> None

let emoji t =
  if List.mem sas_emoji t.short_authentication_string then
    Option.map emoji_of_bytes (sas_bytes t)
  else None

let decimals t =
  if List.mem sas_decimal t.short_authentication_string then
    Option.map decimals_of_bytes (sas_bytes t)
  else None

let cancel_with ?reason t code =
  {
    sas = { t with stage = Cancelled code };
    send = [ Message.cancel ?reason t.transaction code ];
  }

let cancel ?reason t code =
  match t.stage with
  | Cancelled _ | Done -> { sas = t; send = [] }
  | _ -> cancel_with ?reason t code

let tick t ~now =
  match t.stage with
  | Cancelled _ | Done -> { sas = t; send = [] }
  | _ ->
      if
        Int64.sub (Ev.Timestamp.to_ms now) (Ev.Timestamp.to_ms t.started_at)
        > t.timeout
      then cancel_with t Cancel_code.Timeout
      else { sas = t; send = [] }

let empty ~random ~transaction ~we_started ~ours ~theirs ~now ~timeout ~stage
    ~offered_mac_methods ~start_json =
  let our_secret, our_public = Curve25519.generate ~random () in
  {
    transaction;
    we_started;
    ours;
    theirs;
    started_at = now;
    timeout = Option.value timeout ~default:default_timeout_ms;
    stage;
    offered_mac_methods;
    key_agreement_protocol = key_agreement_curve25519_hkdf;
    hash = hash_sha256;
    mac_method = Mac_method.Hkdf_hmac_sha256_v2;
    short_authentication_string = our_short_authentication_string;
    start_json;
    our_secret;
    our_public;
    their_public = None;
    their_commitment = None;
    shared = None;
    we_confirmed = false;
    their_mac_verified = false;
    we_sent_done = false;
    they_sent_done = false;
    verified_keys = [];
  }

let start_content ~transaction ~ours ~mac_methods =
  Ev.Key_verification_start_content.make ~from_device:(device_string ours)
    ~method_:"m.sas.v1"
    ?transaction_id:(Transaction.transaction_id transaction)
    ~key_agreement_protocols:our_key_agreement_protocols ~hashes:our_hashes
    ~message_authentication_codes:(List.map Mac_method.to_string mac_methods)
    ~short_authentication_string:our_short_authentication_string
    ?relates_to:(Transaction.relates_to transaction)
    ()

let json_of_start content =
  match Jsont.Json.encode Ev.Key_verification_start_content.jsont content with
  | Ok json -> json
  (* The content was decoded from JSON or built here, so the encoder has
     nothing to reject. *)
  | Error m -> invalid_arg ("Matrix_client.Verification.Sas: " ^ m)

let start ~random ~now ?timeout ~transaction ~ours ~theirs
    ?(mac_methods = Mac_method.all) () =
  let content = start_content ~transaction ~ours ~mac_methods in
  let t =
    empty ~random ~transaction ~we_started:true ~ours ~theirs ~now ~timeout
      ~stage:Start_sent ~offered_mac_methods:mac_methods
      ~start_json:(json_of_start content)
  in
  { sas = t; send = [ Message.v transaction (Message.Start content) ] }

let from_start ~random ~now ?timeout ~transaction ~ours ~theirs content =
  let start_json = json_of_start content in
  let t =
    empty ~random ~transaction ~we_started:false ~ours ~theirs ~now ~timeout
      ~stage:Accept_sent ~offered_mac_methods:Mac_method.all ~start_json
  in
  let list f = Option.value (f content) ~default:[] in
  if Ev.Key_verification_start_content.method_ content <> "m.sas.v1" then
    cancel_with t Cancel_code.Unknown_method
  else
    let their_macs =
      List.filter_map Mac_method.of_string
        (list Ev.Key_verification_start_content.message_authentication_codes)
    in
    match
      ( pick our_key_agreement_protocols
          (list Ev.Key_verification_start_content.key_agreement_protocols),
        pick our_hashes (list Ev.Key_verification_start_content.hashes),
        pick Mac_method.all their_macs,
        List.filter
          (fun m ->
            List.mem m
              (list
                 Ev.Key_verification_start_content.short_authentication_string))
          our_short_authentication_string )
    with
    | Some ka, Some hash, Some mac, (_ :: _ as sas) ->
        let t =
          {
            t with
            key_agreement_protocol = ka;
            hash;
            mac_method = mac;
            short_authentication_string = sas;
          }
        in
        let accept =
          Ev.Key_verification_accept_content.make
            ?transaction_id:(Transaction.transaction_id transaction)
            ~method_:"m.sas.v1" ~key_agreement_protocol:ka ~hash
            ~message_authentication_code:(Mac_method.to_string mac)
            ~short_authentication_string:sas
            ~commitment:(commitment ~public_key:t.our_public ~start_json)
            ?relates_to:(Transaction.relates_to transaction)
            ()
        in
        { sas = t; send = [ Message.v transaction (Message.Accept accept) ] }
    | _ -> cancel_with t Cancel_code.Unknown_method

let key_message t =
  Message.v t.transaction
    (Message.Key
       (Ev.Key_verification_key_content.make
          ?transaction_id:(Transaction.transaction_id t.transaction)
          ~key:(Curve25519.Public.to_base64 t.our_public)
          ?relates_to:(Transaction.relates_to t.transaction)
          ()))

let establish t their_public =
  match Curve25519.key_exchange ~secret:t.our_secret ~public:their_public with
  | Ok shared ->
      Ok { t with shared = Some shared; their_public = Some their_public }
  | Error (`Msg _) -> Error Cancel_code.Invalid_message

let mac_content t =
  match t.shared with
  | None -> None
  | Some shared ->
      let info =
        mac_info ~sender:t.ours ~receiver:t.theirs ~flow_id:(flow_id t)
      in
      let entries =
        List.map
          (fun (key_id, key) ->
            ( Key_id.to_string key_id,
              calculate_mac ~shared ~mac_method:t.mac_method
                ~info:(info ^ Key_id.to_string key_id)
                key ))
          (identity_keys t.ours)
      in
      let ids = List.sort String.compare (List.map fst entries) in
      let keys =
        calculate_mac ~shared ~mac_method:t.mac_method ~info:(info ^ "KEY_IDS")
          (String.concat "," ids)
      in
      Some
        (Ev.Key_verification_mac_content.make
           ?transaction_id:(Transaction.transaction_id t.transaction)
           ~mac:entries ~keys
           ?relates_to:(Transaction.relates_to t.transaction)
           ())

let verify_their_mac t content =
  match t.shared with
  | None -> Error Cancel_code.Unexpected_message
  | Some shared ->
      let info =
        mac_info ~sender:t.theirs ~receiver:t.ours ~flow_id:(flow_id t)
      in
      let mac = Ev.Key_verification_mac_content.mac content in
      let ids = List.sort String.compare (List.map fst mac) in
      let expected_keys =
        calculate_mac ~shared ~mac_method:t.mac_method ~info:(info ^ "KEY_IDS")
          (String.concat "," ids)
      in
      if
        not
          (Olm_primitives.ct_equal expected_keys
             (Ev.Key_verification_mac_content.keys content))
      then Error Cancel_code.Key_mismatch
      else
        let known =
          List.map
            (fun (key_id, key) -> (Key_id.to_string key_id, (key_id, key)))
            (identity_keys t.theirs)
        in
        let rec check acc = function
          | [] -> Ok acc
          | (key_id, their_mac) :: rest -> (
              match List.assoc_opt key_id known with
              | None -> check acc rest
              | Some (id, key) ->
                  let expected =
                    calculate_mac ~shared ~mac_method:t.mac_method
                      ~info:(info ^ key_id) key
                  in
                  if Olm_primitives.ct_equal expected their_mac then
                    check ((id, key) :: acc) rest
                  else Error Cancel_code.Key_mismatch)
        in
        Result.bind (check [] mac) (fun verified ->
            if verified = [] then Error Cancel_code.Key_mismatch
            else Ok (List.rev verified))

let maybe_done t =
  if t.we_confirmed && t.their_mac_verified && not t.we_sent_done then
    let t = { t with we_sent_done = true } in
    let stage = if t.they_sent_done then Done else Waiting_done in
    { sas = { t with stage }; send = [ Message.done_ t.transaction ] }
  else { sas = t; send = [] }

let confirm t =
  match t.stage with
  | Sas_ready -> (
      match mac_content t with
      | None -> cancel_with t Cancel_code.Unexpected_message
      | Some content ->
          let t = { t with we_confirmed = true; stage = Mac_sent } in
          let mac = Message.v t.transaction (Message.Mac content) in
          let after = maybe_done t in
          { sas = after.sas; send = mac :: after.send })
  | Cancelled _ | Done -> { sas = t; send = [] }
  | _ -> cancel_with t Cancel_code.Unexpected_message

let mismatch t = cancel t Cancel_code.Mismatched_sas

let handle_accept t content =
  if t.stage <> Start_sent || not t.we_started then
    cancel_with t Cancel_code.Unexpected_message
  else
    let ka =
      Ev.Key_verification_accept_content.key_agreement_protocol content
    in
    let hash = Ev.Key_verification_accept_content.hash content in
    let mac =
      Mac_method.of_string
        (Ev.Key_verification_accept_content.message_authentication_code content)
    in
    let sas =
      Ev.Key_verification_accept_content.short_authentication_string content
    in
    let sas_ok =
      sas <> []
      && List.for_all (fun m -> List.mem m our_short_authentication_string) sas
    in
    (* The peer may only pick from what this side actually offered. *)
    let mac_ok =
      match mac with
      | Some m -> List.exists (Mac_method.equal m) t.offered_mac_methods
      | None -> false
    in
    if
      (not (List.mem ka our_key_agreement_protocols))
      || (not (List.mem hash our_hashes))
      || (not mac_ok) || not sas_ok
    then cancel_with t Cancel_code.Unknown_method
    else
      let t =
        {
          t with
          key_agreement_protocol = ka;
          hash;
          mac_method = Option.get mac;
          short_authentication_string = sas;
          their_commitment =
            Some (Ev.Key_verification_accept_content.commitment content);
          stage = Key_sent;
        }
      in
      { sas = t; send = [ key_message t ] }

let handle_key t content =
  match
    Curve25519.Public.of_base64 (Ev.Key_verification_key_content.key content)
  with
  | Error (`Msg _) -> cancel_with t Cancel_code.Invalid_message
  | Ok their_public -> (
      match t.stage with
      | Key_sent -> (
          (* This side started, so the peer committed to its key in the
             accept, and the commitment is checked before the key is used. *)
          let expected =
            commitment ~public_key:their_public ~start_json:t.start_json
          in
          match t.their_commitment with
          | Some c when not (Olm_primitives.ct_equal c expected) ->
              cancel_with t Cancel_code.Mismatched_commitment
          | _ -> (
              match establish t their_public with
              | Error code -> cancel_with t code
              | Ok t -> { sas = { t with stage = Sas_ready }; send = [] }))
      | Accept_sent -> (
          match establish t their_public with
          | Error code -> cancel_with t code
          | Ok t ->
              let t = { t with stage = Sas_ready } in
              { sas = t; send = [ key_message t ] })
      | _ -> cancel_with t Cancel_code.Unexpected_message)

let handle_mac t content =
  match t.stage with
  | Sas_ready | Mac_sent | Waiting_done -> (
      match verify_their_mac t content with
      | Error code -> cancel_with t code
      | Ok verified ->
          maybe_done
            { t with their_mac_verified = true; verified_keys = verified })
  | _ -> cancel_with t Cancel_code.Unexpected_message

let handle_done t =
  let t = { t with they_sent_done = true } in
  match t.stage with
  | Waiting_done -> { sas = { t with stage = Done }; send = [] }
  | Done | Cancelled _ -> { sas = t; send = [] }
  | Start_sent | Accept_sent | Key_sent | Sas_ready | Mac_sent ->
      cancel_with t Cancel_code.Unexpected_message

let handle t ~now msg =
  match t.stage with
  | Done | Cancelled _ -> { sas = t; send = [] }
  | _ -> (
      let ticked = tick t ~now in
      if is_cancelled ticked.sas then ticked
      else
        match Message.payload msg with
        | Message.Cancel c ->
            (* A cancel is never answered, on either side. *)
            {
              sas =
                {
                  t with
                  stage =
                    Cancelled
                      (Cancel_code.of_string
                         (Ev.Key_verification_cancel_content.code c));
                };
              send = [];
            }
        | Message.Accept c -> handle_accept t c
        | Message.Key c -> handle_key t c
        | Message.Mac c -> handle_mac t c
        | Message.Done _ -> handle_done t
        | Message.Start _ | Message.Ready _ | Message.Request _
        | Message.Request_in_room _ ->
            cancel_with t Cancel_code.Unexpected_message)
