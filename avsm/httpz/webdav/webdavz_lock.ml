(* webdavz_lock.ml — In-memory WebDAV lock manager.
   RFC 4918 Section 6–7 — locking model and lock methods. *)

type lock_info = {
  token : string;
  path : string;
  depth : [ `Zero | `Infinity ];
  scope : [ `Exclusive ];
  owner : string option;
  timeout_s : int;
  created_at : float;
}

type t = {
  locks : (string, lock_info) Hashtbl.t;  (* token -> lock_info *)
  mutable counter : int;
}

let create () =
  { locks = Hashtbl.create 32; counter = 0 }

(* Generate an opaquelocktoken URI per RFC 4918 Section 6.5 *)
let make_token t =
  t.counter <- t.counter + 1;
  let now = Unix.gettimeofday () in
  let h = Hashtbl.hash now in
  Printf.sprintf "opaquelocktoken:%08x-%04x-0000-0000-%012x"
    h (t.counter land 0xffff) t.counter

let is_expired now lock =
  now -. lock.created_at > Float.of_int lock.timeout_s

(* Remove expired locks *)
let gc t =
  let now = Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun token lock acc ->
    if is_expired now lock then token :: acc else acc)
    t.locks []
  in
  List.iter (Hashtbl.remove t.locks) expired

(* Check if path is covered by an existing lock.
   A lock on "/foo" with depth infinity covers "/foo/bar".
   A lock on "/foo" with depth zero covers only "/foo". *)
let path_is_covered_by lock ~path =
  if String.equal lock.path path then true
  else match lock.depth with
  | `Infinity ->
    let prefix = if String.equal lock.path "/" then "/" else lock.path ^ "/" in
    let plen = String.length prefix in
    String.length path >= plen && String.equal (String.sub path 0 plen) prefix
  | `Zero -> false

(* Find all active (non-expired) locks covering a path *)
let locks_covering t ~path =
  gc t;
  Hashtbl.fold (fun _token lock acc ->
    if path_is_covered_by lock ~path then lock :: acc
    else acc)
    t.locks []

(* Find all active locks rooted at a path (for lockdiscovery) *)
let active_locks t ~path =
  gc t;
  Hashtbl.fold (fun _token lock acc ->
    if String.equal lock.path path then lock :: acc
    else acc)
    t.locks []

(* Attempt to acquire a lock. Returns Error if conflicting lock exists.
   RFC 4918 Section 9.10.4 — 423 Locked if conflict. *)
let lock t ~path ~depth ~owner ~timeout_s =
  gc t;
  (* Check for conflicting locks — any exclusive lock covering this path,
     or any lock on a descendant if depth=infinity *)
  let dominated = Hashtbl.fold (fun _token lock acc ->
    (* Existing lock covers our path? *)
    if path_is_covered_by lock ~path then lock :: acc
    (* Our lock would cover existing lock's path? *)
    else if (match depth with
      | `Infinity ->
        let prefix = if String.equal path "/" then "/" else path ^ "/" in
        let plen = String.length prefix in
        String.length lock.path >= plen
          && String.equal (String.sub lock.path 0 plen) prefix
      | `Zero -> false) then lock :: acc
    else acc)
    t.locks []
  in
  match dominated with
  | _ :: _ -> Error (`Locked dominated)
  | [] ->
    let token = make_token t in
    let info = {
      token; path; depth; scope = `Exclusive;
      owner; timeout_s;
      created_at = Unix.gettimeofday ();
    } in
    Hashtbl.replace t.locks token info;
    Ok info

(* Unlock by token. Returns true if token existed and was removed. *)
let unlock t ~token =
  gc t;
  match Hashtbl.find_opt t.locks token with
  | Some _ -> Hashtbl.remove t.locks token; true
  | None -> false

(* Refresh a lock's timeout. RFC 4918 Section 9.10.2. *)
let refresh t ~token ~timeout_s =
  gc t;
  match Hashtbl.find_opt t.locks token with
  | Some lock ->
    let lock' = { lock with timeout_s; created_at = Unix.gettimeofday () } in
    Hashtbl.replace t.locks token lock';
    Some lock'
  | None -> None

(* Check if a mutating operation is allowed on path.
   If the path is locked, the caller must provide the correct token.
   Returns Ok () if allowed, Error lock if blocked.
   RFC 4918 Section 7 — write locks. *)
let check_write t ~path ~lock_token =
  let covering = locks_covering t ~path in
  match covering with
  | [] -> Ok ()
  | locks ->
    match lock_token with
    | Some tok when List.exists (fun l -> String.equal l.token tok) locks ->
      Ok ()
    | _ -> Error (List.hd locks)

(* Find a lock by token *)
let find t ~token =
  gc t;
  Hashtbl.find_opt t.locks token

(* Build lockdiscovery XML for a path's active locks.
   RFC 4918 Section 15.8 *)
let lockdiscovery_xml t ~path =
  let locks = active_locks t ~path in
  List.map (fun lock ->
    let open Webdavz_xml in
    dav_node "activelock" [
      dav_node "locktype" [dav_node "write" []];
      dav_node "lockscope" [dav_node "exclusive" []];
      dav_node "depth" [Pcdata (match lock.depth with
        | `Zero -> "0" | `Infinity -> "infinity")];
      (match lock.owner with
       | Some o -> dav_node "owner" [Pcdata o]
       | None -> dav_node "owner" []);
      dav_node "timeout" [Pcdata (Printf.sprintf "Second-%d" lock.timeout_s)];
      dav_node "locktoken" [dav_node "href" [Pcdata lock.token]];
      dav_node "lockroot" [dav_node "href" [Pcdata lock.path]];
    ])
    locks
