(* [mtime] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod times requests and builds counters
   in. Dune skips aliases under a vendored directory, so nothing else here
   reaches the patch. This test fails if a re-vendor drops it, and it pins the
   span arithmetic and the [pp] forms the log lines depend on. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* The ascriptions are the point. None of these compile unless [mtime.mli]
   still carries its [@@ portable] annotation. *)

let of_ns : (int64 -> Mtime.span) @ portable = Mtime.Span.of_uint64_ns
let to_ns : (Mtime.span -> int64) @ portable = Mtime.Span.to_uint64_ns
let of_float : (float -> Mtime.span option) @ portable = Mtime.Span.of_float_ns
let to_float : (Mtime.span -> float) @ portable = Mtime.Span.to_float_ns
let add : (Mtime.span -> Mtime.span -> Mtime.span) @ portable = Mtime.Span.add

let abs_diff : (Mtime.span -> Mtime.span -> Mtime.span) @ portable =
  Mtime.Span.abs_diff

let scale : (int -> Mtime.span -> Mtime.span) @ portable = Mtime.Span.( * )
let shorter : (Mtime.span -> than:Mtime.span -> bool) @ portable =
  Mtime.Span.is_shorter

let span_pp : (Format.formatter -> Mtime.span -> unit) @ portable =
  Mtime.Span.pp

let span_dump : (Format.formatter -> Mtime.span -> unit) @ portable =
  Mtime.Span.dump

let stamp_of_ns : (int64 -> Mtime.t) @ portable = Mtime.of_uint64_ns
let stamp_to_ns : (Mtime.t -> int64) @ portable = Mtime.to_uint64_ns
let between : (Mtime.t -> Mtime.t -> Mtime.span) @ portable = Mtime.span
let later : (Mtime.t -> than:Mtime.t -> bool) @ portable = Mtime.is_later

let add_span : (Mtime.t -> Mtime.span -> Mtime.t option) @ portable =
  Mtime.add_span

let sub_span : (Mtime.t -> Mtime.span -> Mtime.t option) @ portable =
  Mtime.sub_span

let stamp_pp : (Format.formatter -> Mtime.t -> unit) @ portable = Mtime.pp

(* And none of these compile unless [mtime_clock.mli] still carries its, or
   unless the three externals in [mtime_clock.ml] still annotate in type
   position. A signature ascription does not lift a structure-level
   external. *)

let elapsed : (unit -> Mtime.span) @ portable = Mtime_clock.elapsed
let now : (unit -> Mtime.t) @ portable = Mtime_clock.now
let period : (unit -> Mtime.span option) @ portable = Mtime_clock.period
let counter : (unit -> Mtime_clock.counter) @ portable = Mtime_clock.counter
let count : (Mtime_clock.counter -> Mtime.span) @ portable = Mtime_clock.count
let elapsed_ns : (unit -> int64) @ portable = Mtime_clock.elapsed_ns
let now_ns : (unit -> int64) @ portable = Mtime_clock.now_ns
let period_ns : (unit -> int64 option) @ portable = Mtime_clock.period_ns

(* [request_budget], [floor], [origin] and [since_start] do not compile unless
   [Mtime.span], [Mtime.t] and [Mtime_clock.counter] still have the
   [immutable_data] kind. A portable closure reads a module-level value only
   if its type crosses portability and contention, and each of these is read
   from one. Passing a span as an argument would prove nothing, since a type
   used only as a parameter or a result need not cross anything. *)

let request_budget = Mtime.Span.(30 * s)
let floor = Mtime.Span.ms
let earliest = Mtime.min_stamp
let latest = Mtime.max_stamp
let origin = Mtime_clock.counter ()

let budget_ns : (unit -> int64) @ portable = fun () -> to_ns request_budget
let over_floor : (Mtime.span -> bool) @ portable =
 fun s -> not (shorter s ~than:floor)
let full_range : (unit -> Mtime.span) @ portable =
 fun () -> between earliest latest
let since_start : (unit -> Mtime.span) @ portable = fun () -> count origin

let render ns = Format.asprintf "%a" span_pp (of_ns ns)

let () =
  (* The durations are the unit of every timeout in the tree, so their scale
     is part of the interface. *)
  check "ns is 1" (Int64.equal (to_ns Mtime.Span.ns) 1L);
  check "us is 1e3 ns" (Int64.equal (to_ns Mtime.Span.us) 1_000L);
  check "ms is 1e6 ns" (Int64.equal (to_ns Mtime.Span.ms) 1_000_000L);
  check "s is 1e9 ns" (Int64.equal (to_ns Mtime.Span.s) 1_000_000_000L);
  check "min is 60 s" (Int64.equal (to_ns Mtime.Span.min) 60_000_000_000L);
  check "hour is 3600 s"
    (Int64.equal (to_ns Mtime.Span.hour) 3_600_000_000_000L);
  check "day is 86400 s"
    (Int64.equal (to_ns Mtime.Span.day) 86_400_000_000_000L);
  check "year is a Julian year"
    (Int64.equal (to_ns Mtime.Span.year) 31_557_600_000_000_000L);
  check "zero is 0" (Int64.equal (to_ns Mtime.Span.zero) 0L);
  check "one is 1" (Int64.equal (to_ns Mtime.Span.one) 1L);

  (* A span is unsigned, so max_span is -1L read as unsigned and every
     comparison has to be the unsigned one. A signed compare would make
     max_span the shortest span there is. *)
  check "max_span is 2^64-1"
    (Int64.equal (to_ns Mtime.Span.max_span) (-1L));
  check "max_span is the longest span"
    (Mtime.Span.is_longer Mtime.Span.max_span ~than:Mtime.Span.year);
  check "min_span is zero"
    (Mtime.Span.equal Mtime.Span.min_span Mtime.Span.zero);

  check "spans add" (Int64.equal (to_ns (add Mtime.Span.s Mtime.Span.ms))
                       1_001_000_000L);
  check "scaling multiplies"
    (Int64.equal (to_ns (scale 3 Mtime.Span.s)) 3_000_000_000L);
  check "abs_diff is symmetric"
    (Int64.equal (to_ns (abs_diff Mtime.Span.s Mtime.Span.ms))
       (to_ns (abs_diff Mtime.Span.ms Mtime.Span.s)));

  check "the module level budget is read from a portable closure"
    (Int64.equal (budget_ns ()) 30_000_000_000L);
  check "the module level floor is read from a portable closure"
    (over_floor Mtime.Span.s && not (over_floor Mtime.Span.us));
  check "the module level stamps span the whole range"
    (Int64.equal (to_ns (full_range ())) (-1L));

  check "a float converts" (of_float 1e3 = Some (of_ns 1_000L));
  check "a negative float is rejected" (Option.is_none (of_float (-1.)));
  check "2^53 ns is out of range"
    (Option.is_none (of_float 9007199254740992.));
  check "a span converts back to a float" (to_float (of_ns 1_000L) = 1e3);

  (* [pp] rounds towards positive infinity, so no duration ever prints
     shorter than it is. 12'345ns is 12.345us and prints as 12.4us. *)
  check "under a microsecond prints nanoseconds"
    (String.equal (render 999L) "999ns");
  check "microseconds use the greek mu"
    (String.equal (render 1_000L) "1\xce\xbcs");
  check "one fractional digit is kept"
    (String.equal (render 1_500L) "1.5\xce\xbcs");
  check "rounding is towards positive infinity"
    (String.equal (render 12_345L) "12.4\xce\xbcs");
  check "three digits drop the fraction"
    (String.equal (render 150_000L) "150\xce\xbcs");
  check "milliseconds print" (String.equal (render 1_500_000L) "1.5ms");
  check "seconds print" (String.equal (render 1_500_000_000L) "1.5s");
  check "minutes carry seconds"
    (String.equal (render 90_000_000_000L) "1min30s");
  check "hours carry minutes"
    (String.equal (render 5_400_000_000_000L) "1h30min");
  check "days carry hours"
    (String.equal (render 90_000_000_000_000L) "1d1h");
  check "years carry days"
    (String.equal (render 34_560_000_000_000_000L) "1a35d");
  check "an exact year has no remainder"
    (String.equal (render 31_557_600_000_000_000L) "1a");
  check "the longest span prints"
    (String.equal (render (-1L)) "584a198d");
  check "dump is the unsigned integer"
    (String.equal (Format.asprintf "%a" span_dump Mtime.Span.max_span)
       "18446744073709551615");

  check "a stamp prints as nanoseconds"
    (String.equal (Format.asprintf "%a" stamp_pp (stamp_of_ns 42L)) "42ns");
  check "min_stamp is 0" (Int64.equal (stamp_to_ns Mtime.min_stamp) 0L);
  check "max_stamp is 2^64-1"
    (Int64.equal (stamp_to_ns Mtime.max_stamp) (-1L));
  check "max_stamp is later than min_stamp"
    (later Mtime.max_stamp ~than:Mtime.min_stamp);
  check "a span added to a stamp comes back"
    (add_span (stamp_of_ns 10L) (of_ns 5L) = Some (stamp_of_ns 15L));
  check "an overflowing addition is None"
    (Option.is_none (add_span Mtime.max_stamp Mtime.Span.one));
  check "an underflowing subtraction is None"
    (Option.is_none (sub_span Mtime.min_stamp Mtime.Span.one));
  check "span is order independent"
    (Int64.equal (to_ns (between (stamp_of_ns 10L) (stamp_of_ns 4L)))
       (to_ns (between (stamp_of_ns 4L) (stamp_of_ns 10L))));

  (* The clock is monotonic, so the only thing worth asserting is that it
     does not go backwards and that the counter counts from where it was
     made. Both reads go through the portable ascriptions above. *)
  let t0 = now () in
  let e0 = elapsed () in
  let raw0 = elapsed_ns () in
  ignore (now_ns ());
  ignore (period ());
  ignore (period_ns ());
  let c = counter () in
  for _ = 1 to 100_000 do ignore (Sys.opaque_identity 0) done;
  check "the clock does not go backwards" (later (now ()) ~than:t0);
  check "elapsed does not go backwards"
    (not (Mtime.Span.is_shorter (elapsed ()) ~than:e0));
  check "the raw counter does not go backwards"
    (Int64.unsigned_compare (elapsed_ns ()) raw0 >= 0);
  check "a fresh counter counts a nonnegative span"
    (Int64.unsigned_compare (to_ns (count c)) 0L >= 0);
  check "the module level counter is read from a portable closure"
    (Int64.unsigned_compare (to_ns (since_start ())) 0L >= 0);

  Printf.printf "test_mtime: %d checks ok\n" !checks
