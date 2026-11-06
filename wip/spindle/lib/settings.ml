(* SPDX-License-Identifier: ISC *)
open Json
module S = Httpz.Res

type defaults = { repo : (string * int) list; user : (string * int) list }

type t = {
  store : Store.t;
  now : unit -> float;
  defaults : defaults;
  lock : Eio.Mutex.t;
      (* Serializes this module's reads that are followed by writes, such as
         spending an invite use or adding minutes, since the store yields
         between them. *)
}

let v ~store ~now ~defaults =
  { store; now; defaults; lock = Eio.Mutex.create () }

let stamp t =
  match Ptime.of_float_s (t.now ()) with
  | Some time -> Ptime.to_rfc3339 ~tz_offset_s:0 time
  | None -> invalid "clock out of range"

let fail = Xrpc_error.fail

(* Keys join a DID and a name with a space, which neither may contain. *)
let key a b = a ^ " " ^ b

let split k =
  match String.index_opt k ' ' with
  | Some i -> (String.sub k 0 i, String.sub k (i + 1) (String.length k - i - 1))
  | None -> (k, "")

let in_repo t namespace repo =
  List.filter_map
    (fun (k, v) ->
      let r, name = split k in
      if r = repo then Some (name, v) else None)
    (Store.list t.store namespace)

let note_owner t ~repo ~owner =
  if Store.get t.store "repo-owner" repo <> Some owner then
    Store.put t.store "repo-owner" repo owner

let owner_of t repo = Store.get t.store "repo-owner" repo

let repos_of t owner =
  List.filter_map
    (fun (repo, o) -> if o = owner then Some repo else None)
    (Store.list t.store "repo-owner")

(* Secrets *)

let secrets t repo =
  List.map (fun (k, v) -> (k, get "value" (decode v))) (in_repo t "secret" repo)

let list_secrets t repo =
  obj
    [
      ( "secrets",
        arr
          (List.map
             (fun (k, v) ->
               let v = decode v in
               obj
                 [
                   ("key", str k);
                   ("createdAt", required "createdAt" v);
                   ("createdBy", required "createdBy" v);
                 ])
             (in_repo t "secret" repo)) );
    ]

let identifier k =
  k <> ""
  && (match k.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       k

let check_secret ~key:k ~value =
  if String.length k < 1 || String.length k > 50 then
    fail S.Bad_request "InvalidRequest"
      "secret key length must be between 1 and 50";
  Option.iter
    (fun value ->
      if not (identifier k) then
        fail S.Bad_request "InvalidRequest" "key is not a valid identifier";
      if String.length value < 1 || String.length value > 4096 then
        fail S.Bad_request "InvalidRequest"
          "secret value length must be between 1 and 4096")
    value

(* Every secret goes to every workflow of its repository, so a repository
   holds a bounded number of them. *)
let max_secrets = 100

let add_secret t ~repo ~by k value =
  check_secret ~key:k ~value:(Some value);
  Lock.protect t.lock @@ fun () ->
  if Store.get t.store "secret" (key repo k) <> None then
    fail S.Conflict "SecretAlreadyExists"
      "SecretAlreadyExists: key already present";
  if List.length (in_repo t "secret" repo) >= max_secrets then
    fail S.Too_many_requests "SecretLimitExceeded"
      "a repository holds at most 100 secrets";
  Store.put t.store "secret" (key repo k)
    (encode
       (obj
          [
            ("value", str value);
            ("createdAt", str (stamp t));
            ("createdBy", str by);
          ]))

let remove_secret t ~repo k =
  check_secret ~key:k ~value:None;
  if Store.get t.store "secret" (key repo k) = None then
    fail S.Not_found "SecretNotFound" "secret not found";
  Store.delete t.store "secret" (key repo k)

(* Bans *)

let banned t d = Store.get t.store "ban" d <> None

let bans t =
  obj
    [
      ( "bans",
        arr
          (List.map
             (fun (d, created) ->
               obj [ ("did", str d); ("createdAt", str created) ])
             (List.sort compare (Store.list t.store "ban"))) );
    ]

let ban t d =
  ignore (did d);
  if not (banned t d) then Store.put t.store "ban" d (stamp t)

let unban t d =
  ignore (did d);
  if not (banned t d) then fail S.Bad_request "BanNotFound" "ban not found";
  Store.delete t.store "ban" d

(* Invite codes *)

let invite_ttl = 2. *. 86400.

let random_bytes n =
  In_channel.with_open_bin "/dev/urandom" (fun ic -> really_input_string ic n)

let random_hex n =
  let bytes = random_bytes n in
  String.concat ""
    (List.init n (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i])))

let invite t ~by ~uses ~for_account =
  if uses < 1 || uses > 1000 then
    fail S.Bad_request "InvalidRequest" "useCount must be between 1 and 1000";
  Option.iter (fun d -> ignore (did d)) for_account;
  let now = t.now () in
  (* Expired codes are swept whenever a new one is minted. *)
  List.iter
    (fun (code, raw) ->
      if Json.number (required "expiresAt" (decode raw)) <= now then
        Store.delete t.store "invite" code)
    (Store.list t.store "invite");
  let code = random_hex 16 in
  let expires = now +. invite_ttl in
  Store.put t.store "invite" code
    (encode
       (obj
          ([
             ("createdBy", str by);
             ("uses", int uses);
             ("expiresAt", Jsont.Json.number expires);
           ]
          @ Option.fold ~none:[]
              ~some:(fun d -> [ ("forAccount", str d) ])
              for_account)));
  obj
    [
      ("code", str code);
      ( "expiresAt",
        str
          (Ptime.to_rfc3339 ~tz_offset_s:0
             (Option.get (Ptime.of_float_s expires))) );
    ]

let redeem t code ~actor =
  if banned t actor then
    fail S.Forbidden "AccessControl"
      ("DID does not have sufficient access permissions for this operation: "
     ^ actor);
  let unknown () =
    fail S.Bad_request "InviteNotFound"
      "this invite code is unknown, already spent, or expired"
  in
  Lock.protect t.lock @@ fun () ->
  match Store.get t.store "invite" code with
  | None -> unknown ()
  | Some _ when Store.get t.store "allow" actor <> None ->
      (* An account already allowed spends no use, so one account cannot
         use up a code meant for several. *)
      ()
  | Some raw ->
      let v = decode raw in
      if Json.number (required "expiresAt" v) <= t.now () then unknown ();
      (match field "forAccount" v with
      | Some d when string d <> actor -> unknown ()
      | _ -> ());
      let uses = Json.integer (required "uses" v) - 1 in
      let puts =
        ("allow", actor, "")
        ::
        (if uses > 0 then
           [
             ( "invite",
               code,
               encode
                 (obj
                    (("uses", int uses)
                    :: List.filter (fun (k, _) -> k <> "uses") (members v))) );
           ]
         else [])
      in
      Store.batch t.store ~puts
        ~deletes:(if uses > 0 then [] else [ ("invite", code) ])

(* Quotas *)

type scope = Repo | User

let limit t scope d resource =
  match Store.get t.store "quota" (key d resource) with
  | Some "-1" -> None
  | Some n -> Some (int_of_string n)
  | None -> (
      let defaults =
        match scope with Repo -> t.defaults.repo | User -> t.defaults.user
      in
      match List.assoc_opt resource defaults with
      | Some n when n > 0 -> Some n
      | _ -> None)

let set_limit t d resource limit =
  ignore (did d);
  if
    resource = ""
    || String.length resource > 64
    || (not (String.is_valid_utf_8 resource))
    || String.exists (fun c -> Char.code c < 32 || c = '\127') resource
  then fail S.Bad_request "Generic" "invalid resource name";
  let value =
    match limit with
    | None -> "-1"
    | Some n when n > 0 -> string_of_int n
    | Some n ->
        fail S.Bad_request "Generic"
          (Printf.sprintf "limit must be positive: %d" n)
  in
  Store.put t.store "quota" (key d resource) value

let unset_limit t d resource =
  ignore (did d);
  Store.delete t.store "quota" (key d resource)

let limits t =
  obj
    [
      ( "limits",
        arr
          (List.map
             (fun (k, v) ->
               let d, resource = split k in
               obj
                 [
                   ("did", str d);
                   ("resource", str resource);
                   ("limit", int (int_of_string v));
                 ])
             (List.sort compare (Store.list t.store "quota"))) );
    ]

let month t =
  match Ptime.of_float_s (t.now ()) with
  | Some time ->
      let (y, m, _), _ = Ptime.to_date_time time in
      Printf.sprintf "%04d-%02d" y m
  | None -> invalid "clock out of range"

let minutes t d =
  match Store.get t.store "minutes" (key (month t) d) with
  | Some n -> int_of_string n
  | None -> 0

let add_minutes t dids n =
  if n > 0 then
    let month = month t in
    let used d =
      Option.fold ~none:0 ~some:int_of_string
        (Store.get t.store "minutes" (key month d))
    in
    Lock.protect t.lock (fun () ->
        Store.batch t.store
          ~puts:
            (List.map
               (fun d -> ("minutes", key month d, string_of_int (used d + n)))
               (List.sort_uniq compare dids))
          ~deletes:[])

let wipe t repo =
  let deletes namespace =
    List.filter_map
      (fun (k, _) ->
        let a, b = split k in
        if a = repo || b = repo then Some (namespace, k) else None)
      (Store.list t.store namespace)
  in
  Store.batch t.store ~puts:[]
    ~deletes:(deletes "secret" @ deletes "quota" @ deletes "minutes")
