(* SPDX-License-Identifier: ISC *)
open Json
module S = Httpz.Res

let fail = Xrpc_error.fail

let events =
  [
    "push";
    "repository:renamed";
    "pull_request:created";
    "pull_request:resubmitted";
    "pull_request:merged";
    "pull_request:closed";
    "pull_request:reopened";
  ]

type t = {
  store : Store.t;
  settings : Settings.t;
  fetch : Fetch.plain;
  allow_private : bool;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sw : Eio.Switch.t;
  workers : Eio.Semaphore.t;
  lock : Eio.Mutex.t;
  mutable queued : int;
  hosts : (string, host) Hashtbl.t;
}

(* A host's deliveries in flight and waiting. *)
and host = { slots : Eio.Semaphore.t; mutable waiting : int }

(* Addresses a webhook may reach, unless private ones are allowed for a
   development network. *)
let public_v4 b =
  let o i = Char.code b.[i] in
  not
    (o 0 = 0
    || o 0 = 10
    || o 0 = 127
    || (o 0 = 100 && o 1 land 0xc0 = 64)
    || (o 0 = 169 && o 1 = 254)
    || (o 0 = 172 && o 1 land 0xf0 = 16)
    || (o 0 = 192 && o 1 = 0 && o 2 = 0)
    || (o 0 = 192 && o 1 = 168)
    || (o 0 = 198 && o 1 land 0xfe = 18)
    || o 0 >= 224)

let public_v6 b =
  let o i = Char.code b.[i] in
  let zero n = String.for_all (( = ) '\000') (String.sub b 0 n) in
  if zero 10 && o 10 = 0xff && o 11 = 0xff then public_v4 (String.sub b 12 4)
  else
    not
      (zero 15
      || o 0 land 0xfe = 0xfc
      || (o 0 = 0xfe && o 1 land 0xc0 = 0x80)
      || o 0 = 0xff)

let public (addr : Eio.Net.Ipaddr.v4v6) =
  Eio.Net.Ipaddr.fold
    ~v4:(fun (a : [ `V4 ] Eio.Net.Ipaddr.t) -> public_v4 (a :> string))
    ~v6:(fun (a : [ `V6 ] Eio.Net.Ipaddr.t) -> public_v6 (a :> string))
    addr

let connect ~allow_private net ~sw ~host ~port : Fetch_httpz.conn =
  let addrs =
    Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
    |> List.filter (function
      | `Tcp (ip, _) -> allow_private || public ip
      | _ -> false)
  in
  match addrs with
  | [] -> failwith ("no public address for " ^ host)
  | addr :: _ -> (Eio.Net.connect ~sw net addr :> Fetch_httpz.conn)

let v ~sw ~store ~settings ~allow_private system =
  let net = system#net in
  let fetch =
    Fetch_httpz.v ~clock:system#mono_clock
      ~connect:(connect ~allow_private net)
      ~https:Httpz_tls.system ~max_response:(1024 * 1024)
      ~connect_timeout:(Duration.of_sec 10) net ()
  in
  {
    store;
    settings;
    fetch = Fetch.restrict fetch;
    allow_private;
    clock = system#clock;
    sw;
    workers = Eio.Semaphore.make 8;
    lock = Eio.Mutex.create ();
    hosts = Hashtbl.create 16;
    queued = 0;
  }

let now t = Eio.Time.now t.clock

let stamp t =
  Ptime.to_rfc3339 ~tz_offset_s:0 (Option.get (Ptime.of_float_s (now t)))

let check_url t url =
  let url = String.trim url in
  let uri =
    try Httpz_uri.of_string_exn url
    with _ -> fail S.Bad_request "Generic" "invalid URL"
  in
  (match Httpz_uri.scheme uri with
  | This ("http" | "https") -> ()
  | _ -> fail S.Bad_request "Generic" "URL must use http or https");
  let host =
    match Httpz_uri.encoded_host uri with
    | This h when h <> "" -> h
    | _ -> fail S.Bad_request "Generic" "URL must include a host"
  in
  (* A name is checked again when connecting, against what it resolves to. *)
  (if not t.allow_private then
     let literal =
       if String.length host > 2 && host.[0] = '[' then
         String.sub host 1 (String.length host - 2)
       else host
     in
     match Unix.inet_addr_of_string literal with
     | exception Failure _ -> ()
     | ip ->
         if not (public (Eio_unix.Net.Ipaddr.of_unix ip)) then
           fail S.Bad_request "Generic"
             "URL host is a reserved or private address");
  url

let check_events names =
  let names = List.sort_uniq compare (List.map String.trim names) in
  List.iter
    (fun e ->
      if not (List.mem e events) then
        fail S.Bad_request "InvalidEvent"
          (Printf.sprintf "unknown webhook event %S" e))
    names;
  if names = [] then
    fail S.Bad_request "NoEventsSelected" "at least one event must be specified";
  names

(* Webhooks are kept by a decimal ID, padded so that keys sort by age. *)
let pad n = Printf.sprintf "%012d" n

let id_of s =
  match int_of_string_opt s with
  | Some n when n > 0 && string_of_int n = s -> pad n
  | _ -> fail S.Bad_request "InvalidRequest" "invalid webhook id"

let find t ~repo id =
  let key = id_of id in
  match Store.get t.store "webhook" key with
  | Some raw when get "repo" (decode raw) = repo -> (key, decode raw)
  | _ -> fail S.Not_found "WebhookNotFound" "webhook not found"

let all t =
  List.map (fun (k, v) -> (k, decode v)) (Store.list t.store "webhook")

let view (key, w) =
  obj
    [
      ("id", str (string_of_int (int_of_string key)));
      ("url", required "url" w);
      ("active", required "active" w);
      ("events", required "events" w);
      ("createdAt", required "createdAt" w);
      ("updatedAt", required "updatedAt" w);
    ]

let counts t =
  List.fold_left
    (fun acc (_, w) ->
      let r = get "repo" w in
      (r, 1 + Option.value ~default:0 (List.assoc_opt r acc))
      :: List.remove_assoc r acc)
    [] (all t)

let list t ~repo =
  obj
    [
      ( "webhooks",
        arr
          (List.rev
             (List.filter_map
                (fun (k, w) ->
                  if get "repo" w = repo then Some (view (k, w)) else None)
                (all t))) );
    ]

let create t ~(repo : Catalog.repo) body =
  let url = check_url t (get "url" body) in
  let events = check_events (strings "events" body) in
  let active =
    match field "active" body with Some (Jsont.Bool (b, _)) -> b | _ -> true
  in
  let secret =
    Option.fold ~none:""
      ~some:(fun s -> String.trim (string s))
      (field "secret" body)
  in
  Eio.Mutex.use_rw ~protect:true t.lock @@ fun () ->
  let hooks = all t in
  let count p = List.length (List.filter (fun (_, w) -> p w) hooks) in
  let over scope did n =
    match Settings.limit t.settings scope did "webhooks" with
    | Some limit -> n >= limit
    | None -> false
  in
  if
    over Settings.Repo repo.did (count (fun w -> get "repo" w = repo.did))
    || over Settings.User repo.owner
         (count (fun w -> get "owner" w = repo.owner))
  then
    fail S.Too_many_requests "WebhookLimitExceeded"
      "this repository or account has reached its webhook limit";
  let next =
    1
    + Option.fold ~none:0 ~some:int_of_string
        (Store.get t.store "seq" "webhook")
  in
  let created = stamp t in
  Store.batch t.store ~deletes:[]
    ~puts:
      [
        ("seq", "webhook", string_of_int next);
        ( "webhook",
          pad next,
          encode
            (obj
               [
                 ("repo", str repo.did);
                 ("owner", str repo.owner);
                 ("url", str url);
                 ("events", arr (List.map str events));
                 ("active", bool active);
                 ("secret", str secret);
                 ("createdAt", str created);
                 ("updatedAt", str created);
               ]) );
      ];
  obj [ ("id", str (string_of_int next)) ]

let update t ~repo body =
  let key, w = find t ~repo (get "webhook" body) in
  let set k v w = obj ((k, v) :: List.remove_assoc k (members w)) in
  let w =
    match field "url" body with
    | Some u when String.trim (string u) <> "" ->
        set "url" (str (check_url t (string u))) w
    | _ -> w
  in
  let w =
    match field "secret" body with
    | Some s -> set "secret" (str (String.trim (string s))) w
    | None -> w
  in
  let w =
    match field "active" body with Some a -> set "active" a w | None -> w
  in
  let w =
    match strings "events" body with
    | [] -> w
    | names -> set "events" (arr (List.map str (check_events names))) w
  in
  Store.put t.store "webhook" key (encode (set "updatedAt" (str (stamp t)) w))

let deliveries_of t key =
  List.filter
    (fun (k, _) -> String.starts_with ~prefix:(key ^ " ") k)
    (Store.list t.store "delivery")

let delete t ~repo id =
  let key, _ = find t ~repo id in
  Store.batch t.store ~puts:[]
    ~deletes:
      (("webhook", key)
      :: List.map (fun (k, _) -> ("delivery", k)) (deliveries_of t key))

let wipe t repo =
  List.iter
    (fun (key, w) ->
      if get "repo" w = repo then
        delete t ~repo (string_of_int (int_of_string key)))
    (all t)

let unpadded s =
  let e = Mask.base64 s in
  let n = ref (String.length e) in
  while !n > 0 && e.[!n - 1] = '=' do
    decr n
  done;
  String.sub e 0 !n

let bytes s = obj [ ("$bytes", str (unpadded s)) ]

let deliveries t ~repo ~ids ~limit =
  if ids = [] then
    fail S.Bad_request "InvalidRequest" "at least one webhook id is required";
  if List.length ids > 50 then
    fail S.Bad_request "Generic" "at most 50 webhooks per request";
  let keys = List.map id_of ids in
  obj
    [
      ( "deliveries",
        arr
          (List.concat_map
             (fun key ->
               match Store.get t.store "webhook" key with
               | Some raw when get "repo" (decode raw) = repo ->
                   deliveries_of t key |> List.rev
                   |> List.filteri (fun i _ -> i < limit)
                   |> List.map (fun (_, d) ->
                       let d = decode d in
                       let opt k f =
                         match field k d with
                         | Some (Jsont.String ("", _)) | None -> []
                         | Some v -> [ (k, f v) ]
                       in
                       obj
                         ([
                            ("id", required "id" d);
                            ("webhook", str (string_of_int (int_of_string key)));
                            ("event", required "event" d);
                            ("url", required "url" d);
                            ("success", required "success" d);
                            ("createdAt", required "createdAt" d);
                          ]
                         @ opt "requestBody" (fun v -> bytes (string v))
                         @ opt "responseBody" (fun v -> bytes (string v))
                         @
                         match field "responseCode" d with
                         | Some (Jsont.Number (n, _)) when n > 0. ->
                             [ ("responseCode", int (int_of_float n)) ]
                         | _ -> []))
               | _ -> [])
             keys) );
    ]

let uuid () =
  let b = Bytes.of_string (Settings.random_bytes 16) in
  Bytes.set b 6 (Char.chr (Char.code (Bytes.get b 6) land 0x0f lor 0x40));
  Bytes.set b 8 (Char.chr (Char.code (Bytes.get b 8) land 0x3f lor 0x80));
  let h =
    String.concat ""
      (List.init 16 (fun i -> Printf.sprintf "%02x" (Char.code (Bytes.get b i))))
  in
  String.concat "-"
    [
      String.sub h 0 8;
      String.sub h 8 4;
      String.sub h 12 4;
      String.sub h 16 4;
      String.sub h 20 12;
    ]

let signature secret body =
  "sha256=" ^ Digestif.SHA256.(to_hex (hmac_string ~key:secret body))

(* One attempt. Transport errors and server errors may be retried. *)
let attempt t ~url ~headers body =
  match
    Eio.Time.with_timeout_exn t.clock 10. (fun () ->
        Fetch.with_response ~headers ~body:(Fetch.String body) ~redirects:0
          t.fetch `POST url (fun r ->
            (Fetch.status r, Network.read_limited (Fetch.body r) 10240)))
  with
  | (code, _) as answer ->
      if code >= 500 then Error (Some answer) else Ok answer
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e -> Error (Some (0, Printexc.to_string e))

let record t key ~id ~event ~url ~body ~code ~response ~success =
  let seq =
    1
    + Option.fold ~none:0 ~some:int_of_string
        (Store.get t.store "seq" "delivery")
  in
  let stale =
    let mine = deliveries_of t key in
    let excess = List.length mine + 1 - 50 in
    List.filteri (fun i _ -> i < excess) mine
    |> List.map (fun (k, _) -> ("delivery", k))
  in
  Store.batch t.store ~deletes:stale
    ~puts:
      [
        ("seq", "delivery", string_of_int seq);
        ( "delivery",
          key ^ " " ^ pad seq,
          encode
            (obj
               [
                 ("id", str id);
                 ("event", str event);
                 ("url", str url);
                 ("requestBody", str body);
                 ("responseCode", int code);
                 ("responseBody", str response);
                 ("success", bool success);
                 ("createdAt", str (stamp t));
               ]) );
      ]

let send t key w ~event ~agent ~full_name body =
  let url = get "url" w in
  let id = uuid () in
  let secret = get "secret" w in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", agent);
      ("X-Tangled-Event", event);
      ("X-Tangled-Hook-ID", string_of_int (int_of_string key));
      ("X-Tangled-Delivery", id);
      ("X-Tangled-Repo", full_name);
    ]
    @
    if secret = "" then []
    else [ ("X-Tangled-Signature-256", signature secret body) ]
  in
  let headers =
    List.fold_right
      (fun (k, value) acc ->
        Fetch.Header.( :: ) (Fetch.Header.raw k value, acc))
      headers
      Fetch.Header.[]
  in
  let rec go n delay =
    match attempt t ~url ~headers body with
    | Ok (code, response) -> (code, response, code >= 200 && code < 300)
    | Error answer when n < 3 ->
        ignore answer;
        Eio.Time.sleep t.clock delay;
        go (n + 1) (Float.min 10. (delay *. 2.))
    | Error (Some (code, response)) ->
        ( code,
          (if code >= 500 then Printf.sprintf "server error: %d" code
           else response),
          false )
    | Error None -> (0, "", false)
  in
  let code, response, success = go 1 1. in
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      record t key ~id ~event ~url ~body ~code ~response ~success)

(* Deliveries run in the background, eight at once, with at most 1024
   waiting. One host has at most two in flight and 64 waiting, so a dead host
   cannot hold up the rest, as in the reference spindle. Past either limit a
   failed delivery is recorded instead. *)
let enqueue t key w ~event ~agent ~full_name body =
  let url = get "url" w in
  let name = try Network.origin ~allow_http:true url with Invalid _ -> url in
  let host =
    match Hashtbl.find_opt t.hosts name with
    | Some h -> h
    | None -> { slots = Eio.Semaphore.make 2; waiting = 0 }
  in
  if t.queued >= 1024 || host.waiting >= 64 then
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        record t key ~id:(uuid ()) ~event ~url ~body ~code:0
          ~response:"delivery queue saturated" ~success:false)
  else (
    t.queued <- t.queued + 1;
    host.waiting <- host.waiting + 1;
    Hashtbl.replace t.hosts name host;
    Eio.Fiber.fork ~sw:t.sw (fun () ->
        Fun.protect ~finally:(fun () ->
            t.queued <- t.queued - 1;
            host.waiting <- host.waiting - 1;
            if host.waiting = 0 then Hashtbl.remove t.hosts name)
        @@ fun () ->
        Eio.Semaphore.acquire host.slots;
        Fun.protect ~finally:(fun () -> Eio.Semaphore.release host.slots)
        @@ fun () ->
        Eio.Semaphore.acquire t.workers;
        Fun.protect ~finally:(fun () -> Eio.Semaphore.release t.workers)
        @@ fun () ->
        try send t key w ~event ~agent ~full_name body with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e -> Printf.eprintf "spindle webhook: %s\n%!" (Printexc.to_string e)))

let retry t ~repo body =
  let key, w = find t ~repo (get "webhook" body) in
  let delivery = get "delivery" body in
  match
    List.find_opt
      (fun (_, d) -> get "id" (decode d) = delivery)
      (deliveries_of t key)
  with
  | None -> fail S.Not_found "DeliveryNotFound" "delivery not found"
  | Some (_, d) ->
      let d = decode d in
      let payload = get "requestBody" d in
      let full_name =
        try get "full_name" (required "repository" (decode payload))
        with Invalid _ -> ""
      in
      enqueue t key w ~event:(get "event" d) ~agent:"Tangled-Hook/retry"
        ~full_name payload

(* Payloads *)

let host_of origin =
  match String.index_opt origin ':' with
  | Some i when String.length origin > i + 3 ->
      String.sub origin (i + 3) (String.length origin - i - 3)
  | _ -> origin

(* The reference spindle names a repository by its owner and lowercased
   record key. *)
let full_name (r : Catalog.repo) = r.owner ^ "/" ^ Catalog.slug r.name

let repository (r : Catalog.repo) =
  let full_name = full_name r in
  let web = "https://" ^ host_of r.knot ^ "/" ^ full_name in
  obj
    [
      ("name", str r.name);
      ("full_name", str full_name);
      ("description", str "");
      ("fork", bool false);
      ("html_url", str web);
      ("clone_url", str web);
      ("ssh_url", str ("ssh://git@" ^ host_of r.knot ^ "/" ^ full_name));
      ("created_at", str r.created);
      ("updated_at", str r.created);
      ("owner", obj [ ("did", str r.owner) ]);
    ]

let fire t (r : Catalog.repo) ~event ~agent payload =
  let body = encode payload in
  List.iter
    (fun (key, w) ->
      if
        get "repo" w = r.did
        && (match field "active" w with
          | Some (Jsont.Bool (b, _)) -> b
          | _ -> false)
        && List.mem event (strings "events" w)
      then enqueue t key w ~event ~agent ~full_name:(full_name r) body)
    (all t)

let push t r ~actor ~ref_ ~before ~after =
  let agent =
    if String.length after >= 7 then "Tangled-Hook/" ^ String.sub after 0 7
    else "Tangled-Hook/push"
  in
  fire t r ~event:"push" ~agent
    (obj
       [
         ("ref", str ref_);
         ("before", str before);
         ("after", str after);
         ("repository", repository r);
         ("pusher", obj [ ("did", str actor) ]);
       ])

let renamed t (r : Catalog.repo) ~old_name =
  fire t r ~event:"repository:renamed" ~agent:"Tangled-Hook/rename"
    (obj
       [
         ("old_name", str old_name);
         ("new_name", str r.name);
         ("repository", repository r);
         ("sender", obj [ ("did", str r.owner) ]);
       ])

let pull_request t r ~event ~sender pull =
  let action = String.sub event 13 (String.length event - 13) in
  fire t r ~event ~agent:"Tangled-Hook/pull_request"
    (obj
       [
         ("action", str action);
         ("pull_request", pull);
         ("repository", repository r);
         ("sender", obj [ ("did", str sender) ]);
       ])
