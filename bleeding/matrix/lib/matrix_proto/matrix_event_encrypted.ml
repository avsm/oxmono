open Matrix_id

let olm_algorithm =
  Matrix_event_extensible.Encryption_algorithm.(
    to_string Olm_v1_curve25519_aes_sha2)

let megolm_algorithm =
  Matrix_event_extensible.Encryption_algorithm.(to_string Megolm_v1_aes_sha2)

(* The algorithm member is checked rather than read: each of the shapes below
   is the content of one algorithm, so any other value means the caller is
   holding the wrong codec. *)
let algorithm_jsont expected =
  Jsont.map
    ~dec:(fun s ->
      if String.equal s expected then ()
      else
        Jsont.Error.msg Jsont.Meta.none
          (Printf.sprintf "unexpected algorithm %S, expected %S" s expected))
    ~enc:(fun () -> expected)
    Matrix_json.Codec.string

module Olm_message_type = struct
  type t = Pre_key | Normal

  let to_int = function Pre_key -> 0 | Normal -> 1

  let of_int = function
    | 0 -> Ok Pre_key
    | 1 -> Ok Normal
    | n -> Error (`Msg (Printf.sprintf "unknown Olm message type %d" n))

  let equal a b = a = b

  let pp ppf t =
    Format.pp_print_string ppf
      (match t with Pre_key -> "pre-key" | Normal -> "normal")

  let jsont =
    Jsont.map
      ~dec:(fun n ->
        match of_int n with
        | Ok t -> t
        | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m)
      ~enc:to_int Matrix_json.Codec.int
end

module Olm_ciphertext = struct
  type entry = {
    recipient_key : string;
    message_type : Olm_message_type.t;
    body : string;
  }

  type t = entry list

  let find t ~recipient_key =
    List.find_opt (fun e -> String.equal e.recipient_key recipient_key) t

  type message = { message_type : Olm_message_type.t; body : string }

  let message_jsont =
    Jsont.Object.(
      map (fun message_type body -> { message_type; body })
      |> mem "type" Olm_message_type.jsont ~enc:(fun m -> m.message_type)
      |> mem "body" Matrix_json.Codec.string ~enc:(fun m -> m.body)
      |> finish)

  let jsont =
    Matrix_string_map.jsont message_jsont
    |> Jsont.map
         ~dec:(fun value ->
           (List.map (fun (recipient_key, m) ->
                { recipient_key; message_type = m.message_type; body = m.body }))
             value)
         ~enc:(fun value ->
           (List.map (fun e ->
                ( e.recipient_key,
                  { message_type = e.message_type; body = e.body } )))
             value)
end

module Olm_plaintext = struct
  type t = {
    event_type : string;
    content : Jsont.json;
    sender : User_id.t;
    sender_ed25519 : string;
    recipient : User_id.t;
    recipient_ed25519 : string;
    sender_device_keys : Jsont.json option;
  }

  (* [keys] and [recipient_keys] are objects so that further key types can be
     added; only [ed25519] is defined. *)
  let ed25519_object_jsont =
    Jsont.Object.(
      map Fun.id
      |> mem "ed25519" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:Fun.id
      |> finish)

  let jsont =
    Jsont.Object.(
      map
        (fun
          event_type
          content
          sender
          sender_ed25519
          recipient
          recipient_ed25519
          stable_sender_device_keys
          unstable_sender_device_keys
        ->
          let sender_device_keys =
            match stable_sender_device_keys with
            | Some _ -> stable_sender_device_keys
            | None -> unstable_sender_device_keys
          in
          {
            event_type;
            content;
            sender;
            sender_ed25519;
            recipient;
            recipient_ed25519;
            sender_device_keys;
          })
      |> mem "type" Matrix_json.Codec.string ~enc:(fun t -> t.event_type)
      |> mem "content" Matrix_json.Codec.json ~enc:(fun t -> t.content)
      |> mem "sender" User_id.jsont ~enc:(fun t -> t.sender)
      |> mem "keys" ed25519_object_jsont
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.sender_ed25519)
      |> mem "recipient" User_id.jsont ~enc:(fun t -> t.recipient)
      |> mem "recipient_keys" ed25519_object_jsont
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.recipient_ed25519)
      |> opt_mem "sender_device_keys" Matrix_json.Codec.json ~enc:(fun t ->
          t.sender_device_keys)
      |> opt_mem "org.matrix.msc4147.device_keys" Matrix_json.Codec.json
           ~enc:(fun _ -> None)
      |> finish)
end

module Encrypted = struct
  module Olm = struct
    type t = { sender_key : string; ciphertext : Olm_ciphertext.t }

    let jsont =
      Jsont.Object.(
        map (fun () sender_key ciphertext -> { sender_key; ciphertext })
        |> mem "algorithm"
             (algorithm_jsont olm_algorithm)
             ~dec_absent:(fun () -> ())
             ~enc:(fun _ -> ())
        |> mem "sender_key" Matrix_json.Codec.string ~enc:(fun t ->
            t.sender_key)
        |> mem "ciphertext" Olm_ciphertext.jsont ~enc:(fun t -> t.ciphertext)
        |> finish)
  end

  module Megolm = struct
    type t = {
      sender_key : string option;
      session_id : Session_id.t;
      device_id : Device_id.t option;
      ciphertext : string;
    }

    let jsont =
      Jsont.Object.(
        map (fun () sender_key session_id device_id ciphertext ->
            { sender_key; session_id; device_id; ciphertext })
        |> mem "algorithm"
             (algorithm_jsont megolm_algorithm)
             ~dec_absent:(fun () -> ())
             ~enc:(fun _ -> ())
        |> opt_mem "sender_key" Matrix_json.Codec.string ~enc:(fun t ->
            t.sender_key)
        |> mem "session_id" Session_id.jsont ~enc:(fun t -> t.session_id)
        |> opt_mem "device_id" Device_id.jsont ~enc:(fun t -> t.device_id)
        |> mem "ciphertext" Matrix_json.Codec.string ~enc:(fun t ->
            t.ciphertext)
        |> finish)
  end
end

module Room_key_content = struct
  type t = {
    room_id : Room_id.t;
    session_id : Session_id.t;
    session_key : string;
    shared_history : bool;
  }

  let jsont =
    Jsont.Object.(
      map
        (fun
          ()
          room_id
          session_id
          session_key
          stable_shared_history
          unstable_shared_history
        ->
          {
            room_id;
            session_id;
            session_key;
            shared_history =
              Option.value stable_shared_history
                ~default:(Option.value unstable_shared_history ~default:false);
          })
      |> mem "algorithm"
           (algorithm_jsont megolm_algorithm)
           ~dec_absent:(fun () -> ())
           ~enc:(fun _ -> ())
      |> mem "room_id" Room_id.jsont ~enc:(fun t -> t.room_id)
      |> mem "session_id" Session_id.jsont ~enc:(fun t -> t.session_id)
      |> mem "session_key" Matrix_json.Codec.string ~enc:(fun t ->
          t.session_key)
      |> opt_mem "m.shared_history" Jsont.bool ~enc:(fun t ->
          Some t.shared_history)
      |> opt_mem "org.matrix.msc3061.shared_history" Jsont.bool ~enc:(fun _ ->
          None)
      |> finish)
end

module Forwarded_room_key_content = struct
  type t = {
    room_id : Room_id.t;
    sender_key : string;
    session_id : Session_id.t;
    session_key : string;
    sender_claimed_ed25519_key : string;
    forwarding_curve25519_key_chain : string list;
  }

  let jsont =
    Jsont.Object.(
      map
        (fun
          ()
          room_id
          sender_key
          session_id
          session_key
          sender_claimed_ed25519_key
          forwarding_curve25519_key_chain
        ->
          {
            room_id;
            sender_key;
            session_id;
            session_key;
            sender_claimed_ed25519_key;
            forwarding_curve25519_key_chain;
          })
      |> mem "algorithm"
           (algorithm_jsont megolm_algorithm)
           ~dec_absent:(fun () -> ())
           ~enc:(fun _ -> ())
      |> mem "room_id" Room_id.jsont ~enc:(fun t -> t.room_id)
      |> mem "sender_key" Matrix_json.Codec.string ~enc:(fun t -> t.sender_key)
      |> mem "session_id" Session_id.jsont ~enc:(fun t -> t.session_id)
      |> mem "session_key" Matrix_json.Codec.string ~enc:(fun t ->
          t.session_key)
      |> mem "sender_claimed_ed25519_key" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.sender_claimed_ed25519_key)
      |> mem "forwarding_curve25519_key_chain"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.forwarding_curve25519_key_chain)
      |> finish)
end
