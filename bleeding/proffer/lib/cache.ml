type entry = { body : string; etag : Etag.t; expires : float; seq : int }

(* An entry's [seq] orders it by last use, so the least recently used entry is
   the one with the smallest [seq]. The stock tree keys this on [Map], but a
   stdlib [Map]'s abstract type carries no kind, so an [Atomic.t] holding one
   no longer crosses into a portable handler. An association list crosses, and
   the cache is bounded, so every operation stays within [max_entries]. *)
type state = {
  entries : (string * entry) list;
  count : int;
  next : int;
  hits : int;
  misses : int;
}

type t = { ttl : float; max_entries : int; state : state Atomic.t }

let empty = { entries = []; count = 0; next = 0; hits = 0; misses = 0 }

let create ?(max_entries = 1024) ~ttl () =
  if not (Float.is_finite ttl && ttl >= 0.) then
    invalid_arg "Proffer.Cache.create: ttl must be finite and nonnegative";
  if max_entries < 1 then
    invalid_arg "Proffer.Cache.create: max_entries must be positive";
  { ttl; max_entries; state = Atomic.make empty }

let etag_of body = Digest.to_hex (Digest.string body)

(* Every update replaces an immutable state, so the cache needs no lock. A
   losing racer retries against the state that won. *)
let rec bump t f =
  let cur = Atomic.get t.state in
  if not (Atomic.compare_and_set t.state cur (f cur)) then bump t f

let without key entries =
  List.filter (fun (k, _) -> not (String.equal k key)) entries

let touch key (e : entry) s =
  {
    s with
    entries = (key, { e with seq = s.next }) :: without key s.entries;
    next = s.next + 1;
  }

let lru = function
  | [] -> None
  | (k0, (e0 : entry)) :: tl ->
      let rec go best seq = function
        | [] -> Some best
        | (k, (e : entry)) :: tl ->
            if e.seq < seq then go k e.seq tl else go best seq tl
      in
      go k0 e0.seq tl

let rec evict max s =
  if s.count <= max then s
  else
    match lru s.entries with
    | None -> s
    | Some key ->
        evict max
          { s with entries = without key s.entries; count = s.count - 1 }

let prune now s =
  let entries =
    List.filter (fun (_, (e : entry)) -> now < e.expires) s.entries
  in
  let count = List.length entries in
  if count = s.count then s else { s with entries; count }

let store max key (e : entry) s =
  let present = List.mem_assoc key s.entries in
  evict max
    {
      s with
      entries = (key, { e with seq = s.next }) :: without key s.entries;
      count = (if present then s.count else s.count + 1);
      next = s.next + 1;
      misses = s.misses + 1;
    }

let memoize t ~now ~key gen =
  if not (Float.is_finite now) then
    invalid_arg "Proffer.Cache.memoize: now must be finite";
  let expires = now +. t.ttl in
  if not (Float.is_finite expires) then
    invalid_arg "Proffer.Cache.memoize: now + ttl must be finite";
  let cur = Atomic.get t.state in
  match List.assoc_opt key cur.entries with
  | Some e when now < e.expires ->
      bump t (fun s ->
          let s = { s with hits = s.hits + 1 } in
          (* Retry against the state that won, which may have dropped or
             replaced the entry this hit was served from. *)
          match List.assoc_opt key s.entries with
          | Some e' when now < e'.expires -> touch key e' s
          | _ -> s);
      (e.body, e.etag)
  | _ ->
      let body = gen () in
      let etag = Etag.weak (etag_of body) in
      let e = { body; etag; expires; seq = 0 } in
      bump t (fun s -> store t.max_entries key e (prune now s));
      (body, etag)

let stats t =
  let s = Atomic.get t.state in
  (s.hits, s.misses)
