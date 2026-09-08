module type S = sig
  @@ portable
  type t : immutable_data
  val of_string : string -> (t, [> `Msg of string ]) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val jsont : t Jsont.t
end

(* The five derived operations every identifier shares, written once over the
   two that differ. *)
module Derived (X : sig
  @@ portable
  type t : immutable_data
  val kind : string
  val of_string : string -> (t, [> `Msg of string ]) result
  val to_string : t -> string
end) =
struct
  let of_string_exn s =
    match X.of_string s with
    | Ok t -> t
    | Error (`Msg msg) ->
        invalid_arg (Printf.sprintf "invalid %s %S: %s" X.kind s msg)

  let equal a b = String.equal (X.to_string a) (X.to_string b)
  let compare a b = String.compare (X.to_string a) (X.to_string b)
  let pp ppf t = Format.pp_print_string ppf (X.to_string t)

  let jsont =
    Jsont.of_of_string ~kind:X.kind ~enc:X.to_string (fun s ->
        match X.of_string s with Ok t -> Ok t | Error (`Msg msg) -> Error msg)
end

module Server_name = struct
  type t = string

  let valid_port s =
    s <> ""
    && String.for_all (function '0' .. '9' -> true | _ -> false) s
    &&
    match int_of_string_opt s with
    | Some port -> port <= 65535
    | None -> false

  let valid_dns_host host =
    if host = "" then false
    else
      match Domain_name.of_string host with
      | Error _ -> false
      | Ok host -> Result.is_ok (Domain_name.host host)

  let valid_host host =
    match Ipaddr.V4.of_string host with
    | Ok _ -> true
    | Error _ -> valid_dns_host host

  let valid_server_name s =
    if s = "" then false
    else if s.[0] = '[' then
      match String.index_opt s ']' with
      | None -> false
      | Some close ->
          let address = String.sub s 1 (close - 1) in
          let suffix = String.sub s (close + 1) (String.length s - close - 1) in
          address <> ""
          && Result.is_ok (Ipaddr.V6.of_string address)
          && (suffix = ""
             || String.length suffix > 1
                && suffix.[0] = ':'
                && valid_port (String.sub suffix 1 (String.length suffix - 1)))
    else
      match String.index_opt s ':' with
      | None -> valid_host s
      | Some colon ->
          let host = String.sub s 0 colon in
          let port = String.sub s (colon + 1) (String.length s - colon - 1) in
          valid_host host && valid_port port

  let of_string s =
    if valid_server_name s then Ok s else Error (`Msg "not a server name")

  let to_string t = t

  include Derived (struct
    type nonrec t = t

    let kind = "server_name"
    let of_string = of_string
    let to_string = to_string
  end)
end

module User_id = struct
  type t = { localpart : string; server_name : Server_name.t }

  let sigil = '@'

  (* The specification's permitted set. Historical identifiers may hold any
     other printable ASCII byte except the colon, and the specification
     requires that they be accepted, so [of_string] admits both and
     [is_spec_conformant] tells them apart. *)
  let is_permitted_char = function
    | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '=' | '-' | '/' | '+' -> true
    | _ -> false

  let of_string s =
    if String.length s > 255 then Error (`Msg "too long")
    else if String.length s < 2 then Error (`Msg "too short")
    else if s.[0] <> sigil then Error (`Msg "must start with @")
    else
      match String.index_opt s ':' with
      | None -> Error (`Msg "missing colon separator")
      | Some colon_pos -> (
          let localpart = String.sub s 1 (colon_pos - 1) in
          let server_part =
            String.sub s (colon_pos + 1) (String.length s - colon_pos - 1)
          in
          if not (String.is_valid_utf_8 localpart) then
            Error (`Msg "invalid characters in localpart")
          else if
            String.contains localpart ':' || String.contains localpart '\000'
          then Error (`Msg "invalid characters in localpart")
          else
            match Server_name.of_string server_part with
            | Error _ -> Error (`Msg "invalid server name")
            | Ok server_name -> Ok { localpart; server_name })

  let to_string { localpart; server_name } =
    Printf.sprintf "%c%s:%s" sigil localpart (Server_name.to_string server_name)

  let localpart t = t.localpart
  let server_name t = t.server_name

  let is_spec_conformant t =
    t.localpart <> "" && String.for_all is_permitted_char t.localpart

  include Derived (struct
    type nonrec t = t

    let kind = "user_id"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Room_id = struct
  type t = { opaque_id : string; server_name : Server_name.t option }

  let sigil = '!'

  let of_string s =
    if String.length s > 255 then Error (`Msg "too long")
    else if String.length s < 2 then Error (`Msg "too short")
    else if s.[0] <> sigil then Error (`Msg "must start with !")
    else
      let rest = String.sub s 1 (String.length s - 1) in
      if (not (String.is_valid_utf_8 rest)) || String.contains rest '\000' then
        Error (`Msg "invalid opaque id")
      else
        match String.index_opt rest ':' with
        | None -> Ok { opaque_id = rest; server_name = None }
        | Some colon_pos -> (
            let localpart = String.sub rest 0 colon_pos in
            let server_part =
              String.sub rest (colon_pos + 1)
                (String.length rest - colon_pos - 1)
            in
            match Server_name.of_string server_part with
            | Ok server_name ->
                Ok { opaque_id = localpart; server_name = Some server_name }
            | Error _ ->
                (* Current room IDs are opaque after the sigil.  A colon only
                   identifies a legacy origin when its suffix is itself a
                   server name; otherwise it remains part of that opaque ID. *)
                Ok { opaque_id = rest; server_name = None })

  let to_string { opaque_id; server_name } =
    match server_name with
    | None -> Printf.sprintf "%c%s" sigil opaque_id
    | Some server_name ->
        Printf.sprintf "%c%s:%s" sigil opaque_id
          (Server_name.to_string server_name)

  let opaque_id t = t.opaque_id
  let server_name t = t.server_name

  include Derived (struct
    type nonrec t = t

    let kind = "room_id"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Event_id = struct
  type t =
    | V1 of { opaque_id : string; server_name : Server_name.t }
    | V4 of { opaque_id : string }

  let sigil = '$'

  (* Room version 4 dropped the [:server_name] suffix, so the presence of a
     colon is what distinguishes the two encodings. *)
  let of_string s =
    if String.length s > 255 then Error (`Msg "too long")
    else if String.length s < 2 then Error (`Msg "too short")
    else if s.[0] <> sigil then Error (`Msg "must start with $")
    else
      let rest = String.sub s 1 (String.length s - 1) in
      match String.index_opt rest ':' with
      | None -> Ok (V4 { opaque_id = rest })
      | Some colon_pos -> (
          let opaque_id = String.sub rest 0 colon_pos in
          let server_part =
            String.sub rest (colon_pos + 1) (String.length rest - colon_pos - 1)
          in
          match Server_name.of_string server_part with
          | Error _ -> Error (`Msg "invalid server name")
          | Ok server_name -> Ok (V1 { opaque_id; server_name }))

  let to_string = function
    | V1 { opaque_id; server_name } ->
        Printf.sprintf "%c%s:%s" sigil opaque_id
          (Server_name.to_string server_name)
    | V4 { opaque_id } -> Printf.sprintf "%c%s" sigil opaque_id

  include Derived (struct
    type nonrec t = t

    let kind = "event_id"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Room_alias = struct
  type t = { alias : string; server_name : Server_name.t }

  let sigil = '#'

  let of_string s =
    if String.length s > 255 then Error (`Msg "too long")
    else if String.length s < 2 then Error (`Msg "too short")
    else if s.[0] <> sigil then Error (`Msg "must start with #")
    else
      match String.index_opt s ':' with
      | None -> Error (`Msg "missing colon separator")
      | Some colon_pos -> (
          let alias = String.sub s 1 (colon_pos - 1) in
          let server_part =
            String.sub s (colon_pos + 1) (String.length s - colon_pos - 1)
          in
          if (not (String.is_valid_utf_8 alias)) || String.contains alias '\000'
          then Error (`Msg "invalid alias")
          else
            match Server_name.of_string server_part with
            | Error _ -> Error (`Msg "invalid server name")
            | Ok server_name -> Ok { alias; server_name })

  let to_string { alias; server_name } =
    Printf.sprintf "%c%s:%s" sigil alias (Server_name.to_string server_name)

  let alias t = t.alias
  let server_name t = t.server_name

  include Derived (struct
    type nonrec t = t

    let kind = "room_alias"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Device_id = struct
  type t = string

  let of_string s = if s = "" then Error (`Msg "empty") else Ok s
  let to_string t = t

  include Derived (struct
    type nonrec t = t

    let kind = "device_id"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Session_id = struct
  type t = string

  let of_string s = if s = "" then Error (`Msg "empty") else Ok s
  let to_string t = t

  include Derived (struct
    type nonrec t = t

    let kind = "session_id"
    let of_string = of_string
    let to_string = to_string
  end)
end

module Transaction_id = struct
  type t = string

  let v s = s
  let of_string s = if s = "" then Error (`Msg "empty") else Ok s
  let to_string t = t

  let of_bytes b =
    let buf = Buffer.create (2 * String.length b) in
    String.iter
      (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
      b;
    Buffer.contents buf

  include Derived (struct
    type nonrec t = t

    let kind = "transaction_id"
    let of_string = of_string
    let to_string = to_string
  end)
end
