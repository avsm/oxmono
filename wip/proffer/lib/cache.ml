module F64 = Stdlib_upstream_compatible.Float_u
module Map = Base.Map

type entry = { body : string; etag : Etag.t; expires : float#; seq : int }

(* [used] and [entries] contain the same keys under their current sequence. *)
type state = {
  entries : (string, entry, Base.String.comparator_witness) Map.t;
  used : (int, string, Base.Int.comparator_witness) Map.t;
  earliest_expiry : float#;
  next : int;
}

type t = {
  ttl : float#;
  max_entries : int;
  state : state Atomic.t;
  hits : int Atomic.t;
  misses : int Atomic.t;
}

let empty = {
  entries = Map.empty (module Base.String);
  used = Map.empty (module Base.Int);
  earliest_expiry = F64.of_float infinity;
  next = 0;
}

let create ?(max_entries = 1024) ~ttl () =
  if ttl < 0L then
    invalid_arg "Proffer.Cache.create: ttl must be non-negative";
  if max_entries < 1 then
    invalid_arg "Proffer.Cache.create: max_entries must be positive";
  {
    ttl = F64.of_float (Duration.to_f ttl);
    max_entries;
    state = Atomic.make empty;
    hits = Atomic.make 0;
    misses = Atomic.make 0;
  }

(* Length-prefix the key so arbitrary key/body pairs remain distinct. The
   standard-library BLAKE2b digest avoids MD5 collisions without a dependency. *)
let etag_of ~key body =
  Digest.BLAKE256.to_hex
    (Digest.BLAKE256.string
       (Printf.sprintf "%d:%s%s" (String.length key) key body))

let rec bump t f =
  let cur = Atomic.get t.state in
  if not (Atomic.compare_and_set t.state cur (f cur)) then bump t f

(* Sequence wraparound is rare, but must not change LRU ordering. *)
let prepare_seq s =
  if s.next < max_int then s
  else
    Map.fold s.used ~init:{ s with entries = empty.entries; used = empty.used; next = 0 }
      ~f:(fun ~key:_ ~data:key acc ->
        let e = Map.find_exn s.entries key in
        { acc with
          entries = Map.set acc.entries ~key ~data:{ e with seq = acc.next };
          used = Map.set acc.used ~key:acc.next ~data:key;
          next = acc.next + 1 })

let touch key (e : entry) s =
  if e.seq = s.next - 1 then s else
  let s = prepare_seq s in
  let e = Map.find_exn s.entries key in
  {
    s with
    entries = Map.set s.entries ~key ~data:{ e with seq = s.next };
    used = Map.set (Map.remove s.used e.seq) ~key:s.next ~data:key;
    next = s.next + 1;
  }

let evict max s =
  if Map.length s.entries <= max then s
  else
    let seq, key = Map.min_elt_exn s.used in
    { s with entries = Map.remove s.entries key; used = Map.remove s.used seq }

let prune now s =
  if F64.compare now s.earliest_expiry < 0 then s
  else
    Map.fold s.entries ~init:{ s with earliest_expiry = F64.of_float infinity }
      ~f:(fun ~key ~data:e acc ->
        if F64.compare now e.expires >= 0 then
          { acc with entries = Map.remove acc.entries key;
                     used = Map.remove acc.used e.seq }
        else if F64.compare e.expires acc.earliest_expiry < 0 then
          { acc with earliest_expiry = e.expires }
        else acc)

let store max key ~body ~etag ~expires s =
  let s = prepare_seq s in
  let used = match Map.find s.entries key with
    | None -> s.used
    | Some old -> Map.remove s.used old.seq
  in
  evict max
    {
      entries =
        Map.set s.entries ~key ~data:{ body; etag; expires; seq = s.next };
      used = Map.set used ~key:s.next ~data:key;
      earliest_expiry = F64.min s.earliest_expiry expires;
      next = s.next + 1;
    }

let memoize t ~now ~key gen =
  if not (Float.is_finite now) then
    invalid_arg "Proffer.Cache.memoize: now must be finite";
  let now = F64.of_float now in
  let expires = F64.add now t.ttl in
  if not (Float.is_finite (F64.to_float expires)) then
    invalid_arg "Proffer.Cache.memoize: now + ttl must be finite";
  let cur = Atomic.get t.state in
  match Map.find cur.entries key with
  | Some e when F64.compare now e.expires < 0 ->
      Atomic.incr t.hits;
        bump t (fun s ->
            (* Retry against the state that won, which may have dropped or
               replaced the entry this hit was served from. *)
            match Map.find s.entries key with
            | Some e' when F64.compare now e'.expires < 0 -> touch key e' s
            | _ -> s);
      (e.body, e.etag)
  | _ ->
      let body = gen () in
      let etag = Etag.weak (etag_of ~key body) in
      Atomic.incr t.misses;
      bump t (fun s -> store t.max_entries key ~body ~etag ~expires (prune now s));
      (body, etag)

let stats t = (Atomic.get t.hits, Atomic.get t.misses)
