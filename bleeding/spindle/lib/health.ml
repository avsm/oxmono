(* SPDX-License-Identifier: ISC *)
open Json

type source = {
  name : string;
  mutable connected : bool;
  mutable attempts : int;
  mutable activity : float;
  mutable connected_at : float;
  mutable received : float;
  mutable event : float option;
  mutable error : string option;
  mutable persisted : float;
}

type t = {
  store : Store.t;
  enabled : bool;
  sources : (string, source) Hashtbl.t;
}

let v ~store ~enabled = { store; enabled; sources = Hashtbl.create 8 }
let optional name f = function None -> [] | Some value -> [ (name, f value) ]
let float n = Jsont.Json.number n
let bounded s = if String.length s <= 2048 then s else String.sub s 0 2048

let source_json source =
  obj
    ([
       ("source", str source.name);
       ("connected", bool source.connected);
       ("attempts", int source.attempts);
       ("lastActivityAt", float source.activity);
     ]
    @ optional "lastEventAt" float source.event
    @ optional "lastError" str source.error)

let persist t source ~now =
  Store.put t.store "observer" source.name (encode (source_json source));
  source.persisted <- now

let require t name =
  if not (Hashtbl.mem t.sources name) then
    Hashtbl.add t.sources name
      {
        name;
        connected = false;
        attempts = 0;
        activity = 0.;
        connected_at = 0.;
        received = 0.;
        event = None;
        error = None;
        persisted = 0.;
      }

let remove t name =
  Hashtbl.remove t.sources name;
  Store.delete t.store "observer" name

let starting t name ~now =
  require t name;
  let source = Hashtbl.find t.sources name in
  source.connected <- false;
  source.attempts <- source.attempts + 1;
  persist t source ~now

let connected t name ~now =
  Option.iter
    (fun source ->
      source.connected <- true;
      source.activity <- now;
      source.connected_at <- now;
      source.error <- None;
      persist t source ~now)
    (Hashtbl.find_opt t.sources name)

let activity t name ~now =
  Option.iter
    (fun source ->
      source.activity <- now;
      if now -. source.persisted >= 30. then persist t source ~now)
    (Hashtbl.find_opt t.sources name)

let event t name ~at ~now =
  Option.iter
    (fun source ->
      source.event <- Some at;
      source.received <- now)
    (Hashtbl.find_opt t.sources name)

let caught_up t ~now =
  Hashtbl.length t.sources > 0
  && Hashtbl.fold
       (fun _ source ok ->
         ok
         && ((not source.connected)
            || now -. source.connected_at >= 2.
               && (now -. source.received >= 2.
                  || Option.fold ~none:false
                       ~some:(fun at -> now -. at < 30.)
                       source.event)))
       t.sources true

let failed t name ~now exn =
  Option.iter
    (fun source ->
      source.connected <- false;
      source.error <- Some (bounded (Printexc.to_string exn));
      persist t source ~now)
    (Hashtbl.find_opt t.sources name)

let report t ~now =
  let queue name =
    let count, bytes, oldest = Store.usage t.store name in
    let age = if count = 0 then 0. else max 0. (now -. oldest) in
    ( count,
      age,
      obj
        [
          ("count", int count);
          ("bytes", int bytes);
          ("oldestAgeSeconds", float age);
        ] )
  in
  let pending, lag, inbox = queue "inbox" in
  let catalog, _, catalog_json = queue "reconcile" in
  let recovery, _, recovery_json = queue "recover" in
  let source_tasks, _, source_tasks_json = queue "recover-source" in
  let pulls, _, pulls_json = queue "recover-pulls" in
  let discovery, _, discovery_json = queue "discovery-error" in
  let sources =
    Hashtbl.fold (fun _ source xs -> source :: xs) t.sources []
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  let maintenance =
    Option.map decode (Store.get t.store "health" "maintenance")
  in
  let maintenance_ok =
    Option.fold ~none:false
      ~some:(fun value -> field "error" value = None)
      maintenance
  in
  let gaps =
    Store.list t.store "gap" |> List.map (fun (_, raw) -> decode raw)
  in
  let recovering =
    List.exists (fun value -> get "status" value = "pending") gaps
  in
  let ready =
    maintenance_ok
    && ((not t.enabled)
       || Hashtbl.mem t.sources "jetstream"
          && catalog = 0 && recovery = 0 && pulls = 0 && source_tasks = 0
          && discovery = 0 && (not recovering)
          && (pending = 0 || lag < 60.)
          && List.for_all
               (fun source -> source.connected && now -. source.activity < 120.)
               sources)
  in
  let source_json source =
    let raw = source_json source in
    obj
      (members raw
      @ optional "cursor" str (Store.get t.store "cursor" source.name)
      @ optional "eventAgeSeconds"
          (fun at -> float (max 0. (now -. at)))
          source.event)
  in
  ( ready,
    obj
      ([
         ("status", str (if ready then "ok" else "degraded"));
         ("ready", bool ready);
         ("automatic", bool t.enabled);
         ("observers", arr (List.map source_json sources));
         ("inbox", inbox);
         ("catalog", catalog_json);
         ("recovery", recovery_json);
         ("sourceRecovery", source_tasks_json);
         ("pullRecovery", pulls_json);
         ("discovery", discovery_json);
         ("replay", arr gaps);
       ]
      @ optional "maintenance" Fun.id maintenance) )
