module Role = struct
  type t = Owner | Administrator | Moderator | Member | Guest | Other of int

  let of_int = function
    | 100 -> Owner
    | 200 -> Administrator
    | 300 -> Moderator
    | 400 -> Member
    | 600 -> Guest
    | n -> Other n

  let to_int = function
    | Owner -> 100
    | Administrator -> 200
    | Moderator -> 300
    | Member -> 400
    | Guest -> 600
    | Other n -> n

  let equal a b = Int.equal (to_int a) (to_int b)
  let compare a b = Int.compare (to_int a) (to_int b)
  let pp ppf t = Format.pp_print_int ppf (to_int t)

  let jsont =
    Jsont.map ~kind:"Zulip user role" ~dec:of_int ~enc:to_int Json_integer.jsont
end

module Bot_type = struct
  type t =
    | Generic
    | Incoming_webhook
    | Outgoing_webhook
    | Embedded
    | Other of int

  let of_int = function
    | 1 -> Generic
    | 2 -> Incoming_webhook
    | 3 -> Outgoing_webhook
    | 4 -> Embedded
    | n -> Other n

  let to_int = function
    | Generic -> 1
    | Incoming_webhook -> 2
    | Outgoing_webhook -> 3
    | Embedded -> 4
    | Other n -> n

  let equal a b = Int.equal (to_int a) (to_int b)
  let compare a b = Int.compare (to_int a) (to_int b)
  let pp ppf t = Format.pp_print_int ppf (to_int t)

  let jsont =
    Jsont.map ~kind:"Zulip bot type" ~dec:of_int ~enc:to_int Json_integer.jsont
end

type profile_field_value = { value : string; rendered_value : string option }

type t = {
  user_id : Id.User.t;
  email : string;
  full_name : string;
  delivery_email : string option;
  is_active : bool;
  is_admin : bool;
  is_owner : bool;
  is_guest : bool;
  is_billing_admin : bool;
  is_bot : bool;
  bot_type : Bot_type.t option;
  bot_owner_id : Id.User.t option;
  avatar_url : string option;
  avatar_version : int option;
  timezone : string option;
  date_joined : string option;
  role : Role.t option;
  is_imported_stub : bool;
  is_deleted : bool;
  profile_data : (Id.Profile_field.t * profile_field_value) list option;
  extensions : Jsont.json;
  raw_json : Jsont.json option;
}

let empty_object = Jsont.Object ([], Jsont.Meta.none)

let create ~user_id ~email ~full_name ?delivery_email ?(is_active = true)
    ?(is_admin = false) ?(is_owner = false) ?(is_guest = false)
    ?(is_billing_admin = false) ?(is_bot = false) ?bot_type ?bot_owner_id
    ?avatar_url ?avatar_version ?timezone ?date_joined ?role
    ?(is_imported_stub = false) ?(is_deleted = false) ?profile_data () =
  {
    user_id;
    email;
    full_name;
    delivery_email;
    is_active;
    is_admin;
    is_owner;
    is_guest;
    is_billing_admin;
    is_bot;
    bot_type;
    bot_owner_id;
    avatar_url;
    avatar_version;
    timezone;
    date_joined;
    role;
    is_imported_stub;
    is_deleted;
    profile_data;
    extensions = empty_object;
    raw_json = None;
  }

let user_id t = t.user_id
let email t = t.email
let full_name t = t.full_name
let delivery_email t = t.delivery_email
let is_active t = t.is_active
let is_admin t = t.is_admin
let is_owner t = t.is_owner
let is_guest t = t.is_guest
let is_billing_admin t = t.is_billing_admin
let is_bot t = t.is_bot
let bot_type t = t.bot_type
let bot_owner_id t = t.bot_owner_id
let avatar_url t = t.avatar_url
let avatar_version t = t.avatar_version
let timezone t = t.timezone
let date_joined t = t.date_joined
let role t = t.role
let is_imported_stub t = t.is_imported_stub
let is_deleted t = t.is_deleted
let profile_data t = t.profile_data

let profile_field_value_jsont =
  Jsont.Object.map ~kind:"Zulip user profile field value"
    (fun value rendered_value -> { value; rendered_value })
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun v -> v.value)
  |> Jsont.Object.opt_mem "rendered_value" Jsont.string ~enc:(fun v ->
      v.rendered_value)
  |> Jsont.Object.finish

let profile_data_jsont =
  let dec = function
    | Jsont.Object (members, _) ->
        List.map
          (fun ((field_id, meta), value) ->
            let field_id =
              match int_of_string_opt field_id with
              | Some id when id >= 0 && id <= 9_007_199_254_740_991 ->
                  Id.Profile_field.of_int id
              | _ ->
                  Jsont.Error.msgf meta "invalid custom profile field ID %S"
                    field_id
            in
            match Jsont.Json.decode' profile_field_value_jsont value with
            | Ok value -> (field_id, value)
            | Error error -> raise (Jsont.Error error))
          members
    | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
  in
  let enc fields =
    let member (field_id, value) =
      match Jsont.Json.encode' profile_field_value_jsont value with
      | Ok value ->
          ( (string_of_int (Id.Profile_field.to_int field_id), Jsont.Meta.none),
            value )
      | Error error -> raise (Jsont.Error error)
    in
    Jsont.Object (List.map member fields, Jsont.Meta.none)
  in
  Jsont.map ~kind:"Zulip user profile data" ~dec ~enc Jsont.json

let json_of codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

let modeled_json t =
  let mem name value = ((name, Jsont.Meta.none), value) in
  let opt name codec = function
    | None -> []
    | Some value -> [ mem name (json_of codec value) ]
  in
  let extensions =
    match t.extensions with Jsont.Object (members, _) -> members | _ -> []
  in
  Jsont.Object
    ( [
        mem "user_id" (json_of Id.User.jsont t.user_id);
        mem "email" (Jsont.Json.string t.email);
        mem "full_name" (Jsont.Json.string t.full_name);
        mem "is_active" (Jsont.Json.bool t.is_active);
        mem "is_admin" (Jsont.Json.bool t.is_admin);
        mem "is_owner" (Jsont.Json.bool t.is_owner);
        mem "is_guest" (Jsont.Json.bool t.is_guest);
        mem "is_billing_admin" (Jsont.Json.bool t.is_billing_admin);
        mem "is_bot" (Jsont.Json.bool t.is_bot);
        mem "is_imported_stub" (Jsont.Json.bool t.is_imported_stub);
        mem "is_deleted" (Jsont.Json.bool t.is_deleted);
      ]
      @ opt "delivery_email" (Jsont.option Jsont.string) (Some t.delivery_email)
      @ opt "bot_type" (Jsont.option Bot_type.jsont) (Some t.bot_type)
      @ opt "bot_owner_id" (Jsont.option Id.User.jsont) (Some t.bot_owner_id)
      @ opt "avatar_url" (Jsont.option Jsont.string) (Some t.avatar_url)
      @ opt "avatar_version" (Jsont.option Jsont.int) (Some t.avatar_version)
      @ opt "timezone" Jsont.string t.timezone
      @ opt "date_joined" Jsont.string t.date_joined
      @ opt "role" Role.jsont t.role
      @ opt "profile_data" profile_data_jsont t.profile_data
      @ extensions,
      Jsont.Meta.none )

let raw t = Option.value t.raw_json ~default:(modeled_json t)

let base_jsont =
  let make user_id email full_name delivery_email is_active is_admin is_owner
      is_guest is_billing_admin is_bot bot_type bot_owner_id avatar_url
      avatar_version timezone date_joined role is_imported_stub is_deleted
      profile_data extensions =
    {
      user_id;
      email;
      full_name;
      delivery_email;
      is_active;
      is_admin;
      is_owner;
      is_guest;
      is_billing_admin;
      is_bot;
      bot_type;
      bot_owner_id;
      avatar_url;
      avatar_version;
      timezone;
      date_joined;
      role;
      is_imported_stub;
      is_deleted;
      profile_data;
      extensions;
      raw_json = None;
    }
  in
  Jsont.Object.map ~kind:"Zulip user" make
  |> Jsont.Object.mem "user_id" Id.User.jsont ~enc:user_id
  |> Jsont.Object.mem "email" Jsont.string ~enc:email
  |> Jsont.Object.mem "full_name" Jsont.string ~enc:full_name
  |> Jsont.Object.mem "delivery_email"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:delivery_email
  |> Jsont.Object.mem "is_active" Jsont.bool
       ~dec_absent:(fun () -> true)
       ~enc:is_active
  |> Jsont.Object.mem "is_admin" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_admin
  |> Jsont.Object.mem "is_owner" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_owner
  |> Jsont.Object.mem "is_guest" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_guest
  |> Jsont.Object.mem "is_billing_admin" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_billing_admin
  |> Jsont.Object.mem "is_bot" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_bot
  |> Jsont.Object.mem "bot_type"
       (Jsont.option Bot_type.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:bot_type
  |> Jsont.Object.mem "bot_owner_id"
       (Jsont.option Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:bot_owner_id
  |> Jsont.Object.mem "avatar_url"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:avatar_url
  |> Jsont.Object.mem "avatar_version" (Jsont.option Jsont.int)
       ~dec_absent:(fun () -> None)
       ~enc:avatar_version
  |> Jsont.Object.opt_mem "timezone" Jsont.string ~enc:timezone
  |> Jsont.Object.opt_mem "date_joined" Jsont.string ~enc:date_joined
  |> Jsont.Object.opt_mem "role" Role.jsont ~enc:role
  |> Jsont.Object.mem "is_imported_stub" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_imported_stub
  |> Jsont.Object.mem "is_deleted" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:is_deleted
  |> Jsont.Object.opt_mem "profile_data" profile_data_jsont ~enc:profile_data
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun t -> t.extensions)
  |> Jsont.Object.finish

let jsont =
  let dec json =
    match Jsont.Json.decode' base_jsont json with
    | Ok user -> { user with raw_json = Some json }
    | Error error -> raise (Jsont.Error error)
  in
  let enc user =
    match user.raw_json with
    | Some json -> json
    | None -> (
        match Jsont.Json.encode' base_jsont user with
        | Ok json -> json
        | Error error -> raise (Jsont.Error error))
  in
  Jsont.map ~kind:"Zulip user" ~dec ~enc Jsont.json

let pp ppf t =
  Format.fprintf ppf "User{id=%a; email=%S; full_name=%S}" Id.User.pp t.user_id
    t.email t.full_name
