open Matrix_id
open Matrix_event_core

module Key_verification_request_content = struct
  type t = {
    from_device : string;
    methods : string list;
    transaction_id : Transaction_id.t option;
    timestamp : Timestamp.t option;
  }

  let make ~from_device ~methods ?transaction_id ?timestamp () =
    { from_device; methods; transaction_id; timestamp }

  let from_device t = t.from_device
  let methods t = t.methods
  let transaction_id t = t.transaction_id
  let timestamp t = t.timestamp

  let pp ppf t =
    Format.fprintf ppf "from_device: %s, methods: [%s]" t.from_device
      (String.concat ", " t.methods)

  let jsont =
    Jsont.Object.(
      map (fun from_device methods transaction_id timestamp ->
          { from_device; methods; transaction_id; timestamp })
      |> mem "from_device" Matrix_json.Codec.string ~enc:(fun t ->
          t.from_device)
      |> mem "methods"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.methods)
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> opt_mem "timestamp" Timestamp.jsont ~enc:(fun t -> t.timestamp)
      |> finish)
end

module Key_verification_request_message_content = struct
  type t = {
    body : string;
    from_device : string;
    methods : string list;
    to_ : string;
    format : string option;
    formatted_body : string option;
  }

  let msgtype = "m.key.verification.request"

  let make ?(body = "") ~from_device ~methods ~to_ ?format ?formatted_body () =
    { body; from_device; methods; to_; format; formatted_body }

  let body t = t.body
  let from_device t = t.from_device
  let methods t = t.methods
  let to_ t = t.to_
  let format t = t.format
  let formatted_body t = t.formatted_body

  let pp ppf t =
    Format.fprintf ppf "to: %s, from_device: %s, methods: [%s]" t.to_
      t.from_device
      (String.concat ", " t.methods)

  let jsont =
    Jsont.Object.(
      map (fun body from_device methods to_ format formatted_body _msgtype ->
          { body; from_device; methods; to_; format; formatted_body })
      |> mem "body" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.body)
      |> mem "from_device" Matrix_json.Codec.string ~enc:(fun t ->
          t.from_device)
      |> mem "methods"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.methods)
      |> mem "to" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.to_)
      |> opt_mem "format" Matrix_json.Codec.string ~enc:(fun t -> t.format)
      |> opt_mem "formatted_body" Matrix_json.Codec.string ~enc:(fun t ->
          t.formatted_body)
      |> mem "msgtype" Matrix_json.Codec.string
           ~dec_absent:(fun () -> msgtype)
           ~enc:(fun _ -> msgtype)
      |> finish)
end

module Key_verification_ready_content = struct
  type t = {
    from_device : string;
    methods : string list;
    transaction_id : Transaction_id.t option;
    relates_to : Relates_to.t option;
  }

  let make ~from_device ~methods ?transaction_id ?relates_to () =
    { from_device; methods; transaction_id; relates_to }

  let from_device t = t.from_device
  let methods t = t.methods
  let transaction_id t = t.transaction_id
  let relates_to t = t.relates_to

  let pp ppf t =
    Format.fprintf ppf "from_device: %s, methods: [%s]" t.from_device
      (String.concat ", " t.methods)

  let jsont =
    Jsont.Object.(
      map (fun from_device methods transaction_id relates_to ->
          { from_device; methods; transaction_id; relates_to })
      |> mem "from_device" Matrix_json.Codec.string ~enc:(fun t ->
          t.from_device)
      |> mem "methods"
           (Jsont.list Matrix_json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.methods)
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_start_content = struct
  type t = {
    from_device : string;
    method_ : string;
    transaction_id : Transaction_id.t option;
    next_method : string option;
    key_agreement_protocols : string list option;
    hashes : string list option;
    message_authentication_codes : string list option;
    short_authentication_string : string list option;
    secret : string option;
    relates_to : Relates_to.t option;
  }

  let make ~from_device ~method_ ?transaction_id ?next_method
      ?key_agreement_protocols ?hashes ?message_authentication_codes
      ?short_authentication_string ?secret ?relates_to () =
    {
      from_device;
      method_;
      transaction_id;
      next_method;
      key_agreement_protocols;
      hashes;
      message_authentication_codes;
      short_authentication_string;
      secret;
      relates_to;
    }

  let from_device t = t.from_device
  let method_ t = t.method_
  let transaction_id t = t.transaction_id
  let next_method t = t.next_method
  let key_agreement_protocols t = t.key_agreement_protocols
  let hashes t = t.hashes
  let message_authentication_codes t = t.message_authentication_codes
  let short_authentication_string t = t.short_authentication_string
  let secret t = t.secret
  let relates_to t = t.relates_to

  let pp ppf t =
    Format.fprintf ppf "from_device: %s, method: %s" t.from_device t.method_

  let jsont =
    Jsont.Object.(
      map
        (fun
          from_device
          method_
          transaction_id
          next_method
          key_agreement_protocols
          hashes
          message_authentication_codes
          short_authentication_string
          secret
          relates_to
        ->
          {
            from_device;
            method_;
            transaction_id;
            next_method;
            key_agreement_protocols;
            hashes;
            message_authentication_codes;
            short_authentication_string;
            secret;
            relates_to;
          })
      |> mem "from_device" Matrix_json.Codec.string ~enc:(fun t ->
          t.from_device)
      |> mem "method" Matrix_json.Codec.string ~enc:(fun t -> t.method_)
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> opt_mem "next_method" Matrix_json.Codec.string ~enc:(fun t ->
          t.next_method)
      |> opt_mem "key_agreement_protocols" (Jsont.list Matrix_json.Codec.string)
           ~enc:(fun t -> t.key_agreement_protocols)
      |> opt_mem "hashes" (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
          t.hashes)
      |> opt_mem "message_authentication_codes"
           (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
             t.message_authentication_codes)
      |> opt_mem "short_authentication_string"
           (Jsont.list Matrix_json.Codec.string) ~enc:(fun t ->
             t.short_authentication_string)
      |> opt_mem "secret" Matrix_json.Codec.string ~enc:(fun t -> t.secret)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_accept_content = struct
  type t = {
    transaction_id : Transaction_id.t option;
    method_ : string;
    key_agreement_protocol : string;
    hash : string;
    message_authentication_code : string;
    short_authentication_string : string list;
    commitment : string;
    relates_to : Relates_to.t option;
  }

  let make ?transaction_id ~method_ ~key_agreement_protocol ~hash
      ~message_authentication_code ~short_authentication_string ~commitment
      ?relates_to () =
    {
      transaction_id;
      method_;
      key_agreement_protocol;
      hash;
      message_authentication_code;
      short_authentication_string;
      commitment;
      relates_to;
    }

  let transaction_id t = t.transaction_id
  let method_ t = t.method_
  let key_agreement_protocol t = t.key_agreement_protocol
  let hash t = t.hash
  let message_authentication_code t = t.message_authentication_code
  let short_authentication_string t = t.short_authentication_string
  let commitment t = t.commitment
  let relates_to t = t.relates_to

  let pp ppf t =
    Format.fprintf ppf "%s/%s/%s" t.key_agreement_protocol t.hash
      t.message_authentication_code

  let jsont =
    Jsont.Object.(
      map
        (fun
          transaction_id
          method_
          key_agreement_protocol
          hash
          message_authentication_code
          short_authentication_string
          commitment
          relates_to
        ->
          {
            transaction_id;
            method_;
            key_agreement_protocol;
            hash;
            message_authentication_code;
            short_authentication_string;
            commitment;
            relates_to;
          })
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> mem "method" Matrix_json.Codec.string ~enc:(fun t -> t.method_)
      |> mem "key_agreement_protocol" Matrix_json.Codec.string ~enc:(fun t ->
          t.key_agreement_protocol)
      |> mem "hash" Matrix_json.Codec.string ~enc:(fun t -> t.hash)
      |> mem "message_authentication_code" Matrix_json.Codec.string
           ~enc:(fun t -> t.message_authentication_code)
      |> mem "short_authentication_string" (Jsont.list Matrix_json.Codec.string)
           ~enc:(fun t -> t.short_authentication_string)
      |> mem "commitment" Matrix_json.Codec.string ~enc:(fun t -> t.commitment)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_key_content = struct
  type t = {
    transaction_id : Transaction_id.t option;
    key : string;
    relates_to : Relates_to.t option;
  }

  let make ?transaction_id ~key ?relates_to () =
    { transaction_id; key; relates_to }

  let transaction_id t = t.transaction_id
  let key t = t.key
  let relates_to t = t.relates_to
  let pp ppf t = Format.fprintf ppf "key: %s" t.key

  let jsont =
    Jsont.Object.(
      map (fun transaction_id key relates_to ->
          { transaction_id; key; relates_to })
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> mem "key" Matrix_json.Codec.string ~enc:(fun t -> t.key)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_mac_content = struct
  type t = {
    transaction_id : Transaction_id.t option;
    mac : (string * string) list;
    keys : string;
    relates_to : Relates_to.t option;
  }

  let make ?transaction_id ~mac ~keys ?relates_to () =
    { transaction_id; mac; keys; relates_to }

  let transaction_id t = t.transaction_id
  let mac t = t.mac
  let keys t = t.keys
  let relates_to t = t.relates_to

  let pp ppf t =
    Format.fprintf ppf "mac: [%s]" (String.concat ", " (List.map fst t.mac))

  let jsont =
    Jsont.Object.(
      map (fun transaction_id mac keys relates_to ->
          { transaction_id; mac; keys; relates_to })
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> mem "mac" (Matrix_string_map.jsont Matrix_json.Codec.string)
           ~enc:(fun t -> t.mac)
      |> mem "keys" Matrix_json.Codec.string ~enc:(fun t -> t.keys)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_cancel_content = struct
  type t = {
    transaction_id : Transaction_id.t option;
    code : string;
    reason : string;
    relates_to : Relates_to.t option;
  }

  let make ?transaction_id ~code ~reason ?relates_to () =
    { transaction_id; code; reason; relates_to }

  let transaction_id t = t.transaction_id
  let code t = t.code
  let reason t = t.reason
  let relates_to t = t.relates_to
  let pp ppf t = Format.fprintf ppf "%s: %s" t.code t.reason

  let jsont =
    Jsont.Object.(
      map (fun transaction_id code reason relates_to ->
          { transaction_id; code; reason; relates_to })
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> mem "code" Matrix_json.Codec.string ~enc:(fun t -> t.code)
      |> mem "reason" Matrix_json.Codec.string
           ~dec_absent:(fun () -> "")
           ~enc:(fun t -> t.reason)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end

module Key_verification_done_content = struct
  type t = {
    transaction_id : Transaction_id.t option;
    relates_to : Relates_to.t option;
  }

  let make ?transaction_id ?relates_to () = { transaction_id; relates_to }
  let transaction_id t = t.transaction_id
  let relates_to t = t.relates_to
  let pp ppf _t = Format.fprintf ppf "done"

  let jsont =
    Jsont.Object.(
      map (fun transaction_id relates_to -> { transaction_id; relates_to })
      |> opt_mem "transaction_id" Transaction_id.jsont ~enc:(fun t ->
          t.transaction_id)
      |> opt_mem "m.relates_to" Relates_to.jsont ~enc:(fun t -> t.relates_to)
      |> finish)
end
