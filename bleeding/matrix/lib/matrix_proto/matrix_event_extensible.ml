open Matrix_event_core

module Recommendation = struct
  type t = Ban | Unknown of string

  let to_string = function Ban -> "m.ban" | Unknown s -> s
  let of_string = function "m.ban" -> Ban | s -> Unknown s
  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"recommendation" ~enc:to_string (fun s ->
        Ok (of_string s))
end

module Policy_rule_content = struct
  type t = {
    entity : string;
    reason : string;
    recommendation : Recommendation.t;
  }

  let jsont =
    Jsont.Object.(
      map (fun entity reason recommendation ->
          { entity; reason; recommendation })
      |> mem "entity" Matrix_json.Codec.string ~enc:(fun t -> t.entity)
      |> mem "reason" Matrix_json.Codec.string ~enc:(fun t -> t.reason)
      |> mem "recommendation" Recommendation.jsont ~enc:(fun t ->
          t.recommendation)
      |> finish)
end

module Marked_unread_content = struct
  type t = { unread : bool }

  let jsont =
    Jsont.Object.(
      map (fun unread -> { unread })
      |> mem "unread" Jsont.bool
           ~dec_absent:(fun () -> false)
           ~enc:(fun t -> t.unread)
      |> finish)
end

module Encryption_algorithm = struct
  type t = Olm_v1_curve25519_aes_sha2 | Megolm_v1_aes_sha2 | Unknown of string

  let to_string = function
    | Olm_v1_curve25519_aes_sha2 -> "m.olm.v1.curve25519-aes-sha2"
    | Megolm_v1_aes_sha2 -> "m.megolm.v1.aes-sha2"
    | Unknown s -> s

  let of_string = function
    | "m.olm.v1.curve25519-aes-sha2" -> Olm_v1_curve25519_aes_sha2
    | "m.megolm.v1.aes-sha2" -> Megolm_v1_aes_sha2
    | s -> Unknown s

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"algorithm" ~enc:to_string (fun s ->
        Ok (of_string s))
end

module Encrypted_content = struct
  type t = {
    algorithm : Encryption_algorithm.t;
    sender_key : string;
    ciphertext : Jsont.json;
    session_id : string option;
    device_id : string option;
  }

  let jsont =
    Jsont.Object.(
      map (fun algorithm sender_key ciphertext session_id device_id ->
          { algorithm; sender_key; ciphertext; session_id; device_id })
      |> mem "algorithm" Encryption_algorithm.jsont ~enc:(fun t -> t.algorithm)
      |> mem "sender_key" Matrix_json.Codec.string ~enc:(fun t -> t.sender_key)
      |> mem "ciphertext" Matrix_json.Codec.json ~enc:(fun t -> t.ciphertext)
      |> opt_mem "session_id" Matrix_json.Codec.string ~enc:(fun t ->
          t.session_id)
      |> opt_mem "device_id" Matrix_json.Codec.string ~enc:(fun t ->
          t.device_id)
      |> finish)
end

module Reaction_content = struct
  type t = { relates_to : Relates_to.t }

  let jsont =
    Jsont.Object.(
      map (fun relates_to -> { relates_to })
      |> mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Beacon_info_content = struct
  type t = {
    description : string option;
    live : bool;
    timeout : int64;
    timestamp : Timestamp.t option;
    asset_type : string option;
  }

  let asset_jsont =
    Jsont.Object.(
      map (fun typ -> typ)
      |> mem "type" Matrix_json.Codec.string ~enc:Fun.id
      |> finish)

  let jsont =
    Jsont.Object.(
      map (fun description live timeout timestamp asset ->
          {
            description;
            live;
            timeout;
            timestamp;
            asset_type = Option.map (fun (x : string) -> x) asset;
          })
      |> opt_mem "description" Matrix_json.Codec.string ~enc:(fun t ->
          t.description)
      |> mem "live" Jsont.bool
           ~dec_absent:(fun () -> true)
           ~enc:(fun t -> t.live)
      |> mem "timeout" Matrix_json.Codec.int64 ~enc:(fun t -> t.timeout)
      |> opt_mem "org.matrix.msc3488.ts" Timestamp.jsont ~enc:(fun t ->
          t.timestamp)
      |> opt_mem "org.matrix.msc3488.asset" asset_jsont ~enc:(fun t ->
          Option.map (fun x -> x) t.asset_type)
      |> finish)
end

module Beacon_content = struct
  type location = { uri : string; description : string option }

  let location_jsont =
    Jsont.Object.(
      map (fun uri description -> { uri; description })
      |> mem "uri" Matrix_json.Codec.string ~enc:(fun t -> t.uri)
      |> opt_mem "description" Matrix_json.Codec.string ~enc:(fun t ->
          t.description)
      |> finish)

  type t = {
    location : location;
    timestamp : Timestamp.t;
    relates_to : Relates_to.t;
  }

  let jsont =
    Jsont.Object.(
      map (fun location timestamp relates_to ->
          { location; timestamp; relates_to })
      |> mem "org.matrix.msc3488.location" location_jsont ~enc:(fun t ->
          t.location)
      |> mem "org.matrix.msc3488.ts" Timestamp.jsont ~enc:(fun t -> t.timestamp)
      |> mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Poll_start_content = struct
  type poll_answer = { id : string; text : string }

  let poll_answer_jsont =
    Jsont.Object.(
      map (fun id text -> { id; text })
      |> mem "id" Matrix_json.Codec.string ~enc:(fun t -> t.id)
      |> mem "org.matrix.msc1767.text" Matrix_json.Codec.string ~enc:(fun t ->
          t.text)
      |> finish)

  type poll_kind = Disclosed | Undisclosed

  let poll_kind_jsont =
    Jsont.enum
      [
        ("org.matrix.msc3381.poll.disclosed", Disclosed);
        ("org.matrix.msc3381.poll.undisclosed", Undisclosed);
      ]

  type poll_start = {
    question : string;
    kind : poll_kind;
    max_selections : int;
    answers : poll_answer list;
  }

  let poll_start_jsont =
    Jsont.Object.(
      map (fun question kind max_selections answers ->
          { question; kind; max_selections; answers })
      |> mem "question" Matrix_json.Codec.string ~enc:(fun t -> t.question)
      |> mem "kind" poll_kind_jsont
           ~dec_absent:(fun () -> Disclosed)
           ~enc:(fun t -> t.kind)
      |> mem "max_selections" Matrix_json.Codec.int
           ~dec_absent:(fun () -> 1)
           ~enc:(fun t -> t.max_selections)
      |> mem "answers" (Jsont.list poll_answer_jsont) ~enc:(fun t -> t.answers)
      |> finish)

  type t = { poll_start : poll_start; text : string }

  let jsont =
    Jsont.Object.(
      map (fun poll_start text -> { poll_start; text })
      |> mem "org.matrix.msc3381.poll.start" poll_start_jsont ~enc:(fun t ->
          t.poll_start)
      |> mem "org.matrix.msc1767.text" Matrix_json.Codec.string ~enc:(fun t ->
          t.text)
      |> finish)
end

module Poll_response_content = struct
  type t = { relates_to : Relates_to.t; answers : string list }

  let jsont =
    Jsont.Object.(
      map (fun relates_to answers -> { relates_to; answers })
      |> mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> mem "org.matrix.msc3381.poll.response"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.answers)
      |> finish)
end

module Poll_end_content = struct
  type t = { relates_to : Relates_to.t; text : string }

  let jsont =
    Jsont.Object.(
      map (fun relates_to text -> { relates_to; text })
      |> mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> mem "org.matrix.msc1767.text" Matrix_json.Codec.string ~enc:(fun t ->
          t.text)
      |> finish)
end
