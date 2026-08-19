open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  let c = Cache.create ~ttl:10. in
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

(* Pruning is observable by asking for an expired key again with a [now] before
   its own expiry. The entry is gone, so the ask is a miss, where an entry left
   in the list would have answered it. This is what keeps a cache under
   request-derived keys from growing without bound. *)
let () =
  let c = Cache.create ~ttl:10. in
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

let () = Printf.printf "test_cache: %d checks ok\n" !checks
