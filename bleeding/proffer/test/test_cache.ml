open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let invalid f =
  match f () with _ -> false | exception Invalid_argument _ -> true

let () =
  let c = Cache.create ~ttl:10. () in
  let calls = ref 0 in
  let gen () = incr calls; "BODY" in
  let body1, etag1 = Cache.memoize c ~now:0. ~key:"/a" gen in
  check "first call renders" (body1 = "BODY" && !calls = 1);
  let body2, etag2 = Cache.memoize c ~now:5. ~key:"/a" gen in
  check "within ttl is a hit" (body2 = "BODY" && !calls = 1);
  check "etag is stable on a hit" (Etag.weak_equal etag1 etag2);
  let _b, _e = Cache.memoize c ~now:20. ~key:"/a" gen in
  check "past ttl re-renders" (!calls = 2);
  let hits, misses = Cache.stats c in
  check "stats count hits and misses" (hits = 1 && misses = 2);
  let ba, ea = Cache.memoize c ~now:20. ~key:"/a" gen in
  let bb, eb = Cache.memoize c ~now:20. ~key:"/b" (fun () -> "OTHER") in
  check "distinct keys are independent" (ba = "BODY" && bb = "OTHER");
  check "distinct bodies get distinct etags"
    (not (Etag.weak_equal ea eb));
  (* A route takes its handler at [portable], so a cache a handler captures
     reads as contended inside it. This closure has that shape, and it fails to
     compile if [Cache.t] stops crossing contention. *)
  let through_handler : (unit -> int * int) @ portable =
   fun () ->
    let _b, _e = Cache.memoize c ~now:20. ~key:"/c" (fun () -> "THIRD") in
    Cache.stats c
  in
  let hits, misses = through_handler () in
  check "reachable from a portable handler" (hits = 2 && misses = 4)

(* Rewinding [now] after a miss reveals whether that miss pruned an old key. *)
let () =
  let c = Cache.create ~ttl:10. () in
  let calls = ref 0 in
  let gen () = incr calls; "A" in
  let _b, _e = Cache.memoize c ~now:0. ~key:"/a" gen in
  let _b, _e = Cache.memoize c ~now:20. ~key:"/b" (fun () -> "B") in
  let body, _e = Cache.memoize c ~now:5. ~key:"/a" gen in
  check "a miss drops every expired entry" (!calls = 2 && body = "A");
  let _b, _e = Cache.memoize c ~now:6. ~key:"/a" gen in
  check "the entry a prune made way for is kept" (!calls = 2);
  let hits, misses = Cache.stats c in
  check "pruning leaves the counts alone" (hits = 1 && misses = 3)

(* The cache is a fixed budget, so distinct keys evict rather than accumulate,
   and eviction takes the least recently used entry rather than the oldest
   stored one. *)
let () =
  let c = Cache.create ~max_entries:2 ~ttl:1e9 () in
  let gen v () = v in
  let _b, _e = Cache.memoize c ~now:0. ~key:"a" (gen "A") in
  let _b, _e = Cache.memoize c ~now:0. ~key:"b" (gen "B") in
  let _b, _e = Cache.memoize c ~now:0. ~key:"a" (gen "A") in
  check "the second lookup of a is a hit" (fst (Cache.stats c) = 1);
  let _b, _e = Cache.memoize c ~now:0. ~key:"c" (gen "C") in
  let _b, _e = Cache.memoize c ~now:0. ~key:"a" (gen "A") in
  check "the recently used entry survived" (fst (Cache.stats c) = 2);
  let _b, _e = Cache.memoize c ~now:0. ~key:"b" (gen "B") in
  check "the least recently used entry was evicted"
    (snd (Cache.stats c) = 4)

(* Twenty thousand distinct keys against a cap of 1024: the cap is what
   bounds both memory and the cost of an insert, so this finishes at once
   rather than after seconds of list surgery. *)
let () =
  let c = Cache.create ~ttl:1e9 () in
  for i = 0 to 19_999 do
    let _b, _e =
      Cache.memoize c ~now:0. ~key:(string_of_int i) (fun () -> "x")
    in
    ()
  done;
  let hits, misses = Cache.stats c in
  check "every distinct key missed" (hits = 0 && misses = 20_000);
  let _b, _e = Cache.memoize c ~now:0. ~key:"19999" (fun () -> "x") in
  check "the last key is still cached" (fst (Cache.stats c) = 1);
  let _b, _e = Cache.memoize c ~now:0. ~key:"0" (fun () -> "x") in
  check "the first key was evicted" (fst (Cache.stats c) = 1)

(* An expired entry is pruned rather than counted against the cap. *)
let () =
  let c = Cache.create ~max_entries:2 ~ttl:10. () in
  let _b, _e = Cache.memoize c ~now:0. ~key:"a" (fun () -> "A") in
  let _b, _e = Cache.memoize c ~now:0. ~key:"b" (fun () -> "B") in
  let _b, _e = Cache.memoize c ~now:20. ~key:"c" (fun () -> "C") in
  let _b, _e = Cache.memoize c ~now:20. ~key:"c" (fun () -> "C") in
  check "a prune leaves room under the cap" (fst (Cache.stats c) = 1)

let () =
  check "a negative ttl is rejected"
    (invalid (fun () -> Cache.create ~ttl:(-1.) ()));
  check "a non-finite ttl is rejected"
    (invalid (fun () -> Cache.create ~ttl:infinity ()));
  let c = Cache.create ~ttl:1. () in
  check "a non-finite clock reading is rejected"
    (invalid (fun () -> Cache.memoize c ~now:nan ~key:"x" (fun () -> "x")));
  let huge = Cache.create ~ttl:Float.max_float () in
  check "an overflowing expiry is rejected before generation"
    (let generated = ref false in
     invalid (fun () ->
       Cache.memoize huge ~now:Float.max_float ~key:"x" (fun () ->
         generated := true;
         "x"))
     && not !generated);
  check "a zero cap is rejected"
    (invalid (fun () -> Cache.create ~max_entries:0 ~ttl:1. ()))

let () = Printf.printf "test_cache: %d checks ok\n" !checks
