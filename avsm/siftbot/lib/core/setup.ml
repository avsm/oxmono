module P = Jmap_eio.Profile
module S = Siftbot_setup

let checked = function
  | Ok v -> v
  | Error _ -> failwith "Cannot access the shared JMAP profile store"

let describe store (p : Profile.t) : S.profile =
  let jp = P.load store p.jmap_profile in
  {
    name = p.name;
    jmap_profile = p.jmap_profile;
    url = (match jp with Ok p -> P.session_url p | Error _ -> "");
    account = Option.value p.account ~default:"";
    chat_model = Option.value p.chat_model ~default:"";
    has_token =
      (match jp with
      | Ok p -> (
          match P.credential p with Bearer _ -> true | Basic _ -> false)
      | Error _ -> false);
  }

let optional s = match String.trim s with "" -> None | s -> Some s
let fail s = invalid_arg s

let save store (p : S.profile) token fresh =
  if not (P.valid_name p.name && P.valid_name p.jmap_profile) then
    fail "Profile names use letters, digits, dots, underscores and hyphens";
  if fresh && Profile.exists p.name then
    fail "That Siftbot profile already exists. Select it and press e to edit.";
  if (not fresh) && not (Profile.exists p.name) then
    fail "This profile was removed. Reload profiles and retry.";
  let url = String.trim p.url in
  let module U = Fetch.Middleware.Url in
  (match U.of_string url with
  | Ok u
    when U.scheme u = `Https && (not (U.has_fragment u)) && not (U.has_query u)
    ->
      ()
  | _ ->
      fail
        "Use an HTTPS session URL without user information, a query or a \
         fragment");
  let local =
    if Profile.exists p.name then Profile.load p.name
    else Profile.defaults ~name:p.name ~jmap_profile:p.jmap_profile
  in
  let token =
    if token <> "" then token
    else
      match P.load store p.jmap_profile with
      | Ok existing when P.session_url existing = url -> (
          match P.credential existing with
          | Bearer token -> token
          | Basic _ -> fail "Enter a bearer access token for this profile")
      | _ -> fail "Enter an access token for a new or changed server URL"
  in
  let jp =
    match P.v ~name:p.jmap_profile ~session_url:url (Bearer token) with
    | Ok p -> p
    | Error _ -> fail "Invalid session URL or bearer access token"
  in
  let local =
    {
      local with
      jmap_profile = p.jmap_profile;
      account = optional p.account;
      chat_model = optional p.chat_model;
    }
  in
  checked (P.save store jp);
  (try Profile.save local
   with _ ->
     failwith
       "JMAP login saved, but local settings failed. Check permissions and \
        retry.");
  describe store local

let handle env request =
  try
    let store = checked (P.xdg_store env) in
    match request with
    | S.List ->
        S.Profiles
          (List.map
             (fun n -> describe store (Profile.load n))
             (Profile.names ()))
    | Save (p, token, fresh) -> S.Saved (save store p token fresh)
  with
  | Invalid_argument message | Failure message -> S.Error message
  | _ ->
      S.Error "Cannot read or save profiles. Check configuration permissions."
