(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 8620 Section 2: both [capabilities] and [accountCapabilities] are
   typed "String[Object]", so a value that is not a JSON object is a
   malformed Session rather than an extension to ignore. *)
let capability_object_jsont =
  let kind = "Capability object" in
  let dec = function
    | Jsont.Object _ as json -> json
    | json ->
        Jsont.Error.msgf (Jsont.Json.meta json)
          "%s: expected an object but found %s" kind
          (Jsont.Sort.to_string (Jsont.Json.sort json))
  in
  Jsont.map ~kind ~dec ~enc:Fun.id Jsont.json

let capabilities_jsont = Proto_json_map.of_string capability_object_jsont

(* Known capabilities at session scope have schemas of their own. Validate
   them while decoding the session so a malformed advertised capability cannot
   later become indistinguishable from an absent one. *)
let validated_capabilities_jsont ~required ~(kind : string) ~decode =
  let validate capabilities =
    List.iter
      (fun uri ->
        if not (List.mem_assoc uri capabilities) then
          Jsont.Error.msgf Jsont.Meta.none "%s: missing mandatory capability %S"
            kind uri)
      required;
    List.iter
      (fun (uri, json) ->
        match decode uri json with
        | Ok _ -> ()
        | Error msg ->
            Jsont.Error.msgf (Jsont.Json.meta json) "invalid capability %S: %s"
              uri msg)
      capabilities;
    capabilities
  in
  Jsont.map ~kind ~dec:validate ~enc:validate capabilities_jsont

let session_capabilities_jsont =
  validated_capabilities_jsont ~required:[ Proto_capability.core ]
    ~kind:"Session capabilities"
    ~decode:Proto_capability.session_capability_of_json

let account_capabilities_jsont =
  validated_capabilities_jsont ~required:[] ~kind:"Account capabilities"
    ~decode:Proto_capability.account_capability_of_json

let decode_capability jsont uri capabilities =
  match List.assoc_opt uri capabilities with
  | None -> None
  | Some json -> Result.to_option (Jsont.Json.decode' jsont json)

module Account = struct
  type t = {
    name : string;
    is_personal : bool;
    is_read_only : bool;
    account_capabilities : (string * Jsont.json) list;
    unknown : Proto_unknown.t;
  }

  let unknown_member t name = Proto_unknown.find t.unknown name

  let make name is_personal is_read_only account_capabilities unknown =
    { name; is_personal; is_read_only; account_capabilities; unknown }

  let jsont =
    let kind = "Account" in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun a -> a.name)
    |> Jsont.Object.mem "isPersonal" Jsont.bool ~enc:(fun a -> a.is_personal)
    |> Jsont.Object.mem "isReadOnly" Jsont.bool ~enc:(fun a -> a.is_read_only)
    |> Jsont.Object.mem "accountCapabilities" account_capabilities_jsont
         ~enc:(fun a -> a.account_capabilities)
    (* RFC 8620 Section 2: "The client MUST ignore any properties it does not
       understand"; keeping them lets a re-encoded Account carry them on. *)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun a -> a.unknown)
    |> Jsont.Object.finish
end

type t = {
  capabilities : (string * Jsont.json) list;
  accounts : (Proto_id.t * Account.t) list;
  primary_accounts : (string * Proto_id.t) list;
  username : string;
  api_url : string;
  download_url : string;
  upload_url : string;
  event_source_url : string;
  state : string;
  unknown : Proto_unknown.t;
}

let unknown_member t name = Proto_unknown.find t.unknown name

let make capabilities accounts primary_accounts username api_url download_url
    upload_url event_source_url state unknown =
  {
    capabilities;
    accounts;
    primary_accounts;
    username;
    api_url;
    download_url;
    upload_url;
    event_source_url;
    state;
    unknown;
  }

let jsont =
  let kind = "Session" in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "capabilities" session_capabilities_jsont ~enc:(fun s ->
      s.capabilities)
  |> Jsont.Object.mem "accounts" (Proto_json_map.of_id Account.jsont)
       ~enc:(fun s -> s.accounts)
  |> Jsont.Object.mem "primaryAccounts"
       (Proto_json_map.of_string Proto_id.jsont) ~enc:(fun s ->
         s.primary_accounts)
  |> Jsont.Object.mem "username" Jsont.string ~enc:(fun s -> s.username)
  |> Jsont.Object.mem "apiUrl" Jsont.string ~enc:(fun s -> s.api_url)
  |> Jsont.Object.mem "downloadUrl" Jsont.string ~enc:(fun s -> s.download_url)
  |> Jsont.Object.mem "uploadUrl" Jsont.string ~enc:(fun s -> s.upload_url)
  |> Jsont.Object.mem "eventSourceUrl" Jsont.string ~enc:(fun s ->
      s.event_source_url)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun s -> s.state)
  (* RFC 8620 Section 2: "other properties MAY be included on the Session
     object.  Clients MUST ignore any properties they are not expecting." *)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun s -> s.unknown)
  |> Jsont.Object.finish

let find_account id session =
  Option.map snd
    (List.find_opt (fun (aid, _) -> Proto_id.equal aid id) session.accounts)

let primary_account_for capability session =
  List.assoc_opt capability session.primary_accounts

let has_capability uri session = List.mem_assoc uri session.capabilities

let core_capability session =
  decode_capability Proto_capability.Core.jsont Proto_capability.core
    session.capabilities

let mail_capability account =
  decode_capability Proto_capability.Mail.account_jsont Proto_capability.mail
    account.Account.account_capabilities

let submission_capability account =
  decode_capability Proto_capability.Submission.account_jsont
    Proto_capability.submission account.Account.account_capabilities

let contacts_capability account =
  decode_capability Proto_capability.Contacts.account_jsont
    Proto_capability.contacts account.Account.account_capabilities
