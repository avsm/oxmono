(* Entries live in an association list rather than a [Map], because the
   stdlib's map type does not cross portability and a cache that cannot be
   shared between domains is of no use here. A miss drops what has expired, so
   the list holds only the keys asked for within one [ttl] window and a linear
   scan stays cheap even when the keys come from the request. *)

type entry = { body : string; etag : string; expires : float }

type state = { entries : (string * entry) list; hits : int; misses : int }
type t = { ttl : float; state : state Atomic.t }

let create ~ttl =
  { ttl; state = Atomic.make { entries = []; hits = 0; misses = 0 } }

(* MD5 is a cache validator, not a security boundary, so a fast digest is the
   right one. *)
let etag_of body = Digest.to_hex (Digest.string body)

(* Every update replaces an immutable state, so the cache needs no lock. A
   losing racer retries against the state that won. *)
let rec bump t f =
  let cur = Atomic.get t.state in
  if not (Atomic.compare_and_set t.state cur (f cur)) then bump t f

let memoize t ~now ~key gen =
  let cur = Atomic.get t.state in
  match List.assoc_opt key cur.entries with
  | Some e when now < e.expires ->
      bump t (fun s -> { s with hits = s.hits + 1 });
      (e.body, `Weak e.etag)
  | _ ->
      let body = gen () in
      let etag = etag_of body in
      let e = { body; etag; expires = now +. t.ttl } in
      bump t (fun s ->
          (* A miss is the only point at which anything leaves the cache. The
             filter runs on the state this attempt read, so a losing racer
             prunes the state that won rather than an older one. *)
          let live = List.filter (fun (_, e) -> now < e.expires) s.entries in
          {
            entries = (key, e) :: List.remove_assoc key live;
            hits = s.hits;
            misses = s.misses + 1;
          });
      (body, `Weak etag)

let stats t =
  let s = Atomic.get t.state in
  (s.hits, s.misses)
