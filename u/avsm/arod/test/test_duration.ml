(* [duration] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod builds timeouts and retry delays
   in. Dune skips aliases under a vendored directory, so nothing else here
   reaches the patch. This test fails if a re-vendor drops it, and it pins the
   conversions and the parser the serving path depends on. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let raises name f =
  incr checks;
  match f () with
  | _ ->
    prerr_endline ("FAIL: " ^ name ^ " did not raise");
    exit 1
  | exception Invalid_argument _ -> ()

(* The ascriptions are the point. None of these compile unless [duration.mli]
   still carries its [@@ portable] annotation. *)

let sec : (int -> Duration.t) @ portable = Duration.of_sec
let ms : (int -> Duration.t) @ portable = Duration.of_ms
let us : (int -> Duration.t) @ portable = Duration.of_us
let min : (int -> Duration.t) @ portable = Duration.of_min
let hour : (int -> Duration.t) @ portable = Duration.of_hour
let day : (int -> Duration.t) @ portable = Duration.of_day
let year : (int -> Duration.t) @ portable = Duration.of_year
let of_float : (float -> Duration.t) @ portable = Duration.of_f
let to_float : (Duration.t -> float) @ portable = Duration.to_f
let to_ms : (Duration.t -> int) @ portable = Duration.to_ms
let to_sec : (Duration.t -> int) @ portable = Duration.to_sec
let to_min : (Duration.t -> int) @ portable = Duration.to_min
let sec_64 : (int64 -> Duration.t) @ portable = Duration.of_sec_64
let to_sec_64 : (Duration.t -> int64) @ portable = Duration.to_sec_64
let show : (Format.formatter -> Duration.t -> unit) @ portable = Duration.pp

let parse : (string -> (Duration.t, [ `Msg of string ]) result) @ portable =
 fun s -> Duration.of_string s

let parse_exn : (string -> Duration.t) @ portable = Duration.of_string_exn

(* [request_timeout] and [read_deadline] do not compile unless [Duration.t]
   still crosses portability and contention. A portable closure reads a
   module-level value only if its type crosses both, and both of these are
   read from one. Passing a duration as an argument would prove nothing,
   since a type used only as a parameter or a result need not cross
   anything. *)

let request_timeout = Duration.of_sec 30
let read_deadline = Duration.of_ms 250

let timeout_ms : (unit -> int) @ portable = fun () -> to_ms request_timeout
let deadline_us : (unit -> int) @ portable =
 fun () -> Duration.to_us read_deadline

let render d = Format.asprintf "%a" show d

let () =
  (* A nanosecond count is what every consumer stores, so the scale of each
     constructor is part of the interface, not an implementation detail. *)
  check "a microsecond is 1e3 ns" (Int64.equal (us 1) 1_000L);
  check "a millisecond is 1e6 ns" (Int64.equal (ms 1) 1_000_000L);
  check "a second is 1e9 ns" (Int64.equal (sec 1) 1_000_000_000L);
  check "a minute is 60 s" (Int64.equal (min 1) 60_000_000_000L);
  check "an hour is 3600 s" (Int64.equal (hour 1) 3_600_000_000_000L);
  check "a day is 24 h" (Int64.equal (day 1) 86_400_000_000_000L);
  check "a year is 8766 h" (Int64.equal (year 1) 31_557_600_000_000_000L);
  check "the 64 bit second constructor agrees"
    (Int64.equal (sec_64 90L) (sec 90));

  check "seconds round trip" (to_sec (sec 90) = 90);
  check "a second is 1000 ms" (to_ms (sec 1) = 1000);
  check "truncation is toward zero" (to_min (sec 119) = 1);
  check "the 64 bit second projection agrees"
    (Int64.equal (to_sec_64 (sec 90)) 90L);

  check "a fractional second converts"
    (Int64.equal (of_float 1.5) 1_500_000_000L);
  check "a duration converts back to seconds" (to_float (sec 2) = 2.0);

  check "the module level timeout is read from a portable closure"
    (timeout_ms () = 30_000);
  check "the module level deadline is read from a portable closure"
    (deadline_us () = 250_000);

  check "pp uses minutes and seconds under an hour"
    (String.equal (render (sec 90)) "1m30s");
  check "pp uses hours and minutes under a day"
    (String.equal (render (min 150)) "2h30m");
  check "pp uses days and hours under a year"
    (String.equal (render (hour 50)) "2d02h");
  check "pp uses years and days above a year"
    (String.equal (render (day 400)) "1a34d");
  check "pp uses fractional seconds under a minute"
    (String.equal (render (ms 1500)) "1.500s");
  check "pp uses milliseconds under a second"
    (String.equal (render (us 1500)) "1.500ms");
  check "pp uses microseconds under a millisecond"
    (String.equal (render 1_500L) "1.500\xce\xbcs");

  check "a single metric parses"
    (parse "30s" = Ok (sec 30));
  check "metrics accumulate"
    (parse "1m30s" = Ok (sec 90));
  check "a year and a day combine"
    (parse "1y1d" = Ok (Int64.add (year 1) (day 1)));
  check "the subsecond metrics parse"
    (parse "1ms1us1ns" = Ok (Int64.add (ms 1) (Int64.add (us 1) 1L)));
  check "the greek mu is accepted for microseconds"
    (parse "1\xce\xbcs" = Ok (us 1));
  check "the micro sign is accepted for microseconds"
    (parse "1\xc2\xb5s" = Ok (us 1));
  check "a repeated metric is rejected" (Result.is_error (parse "1d1d"));
  check "a bare number is rejected" (Result.is_error (parse "30"));
  check "an unknown metric is rejected" (Result.is_error (parse "30x"));
  check "the exception raising parser agrees"
    (Int64.equal (parse_exn "1m30s") (sec 90));

  (* Every constructor checks its input, which is why consumers may pass a
     value straight through from configuration. *)
  raises "a negative second count" (fun () -> sec (-1));
  raises "a negative float" (fun () -> of_float (-1.));
  raises "an out of range year" (fun () -> year 1_000_000);

  Printf.printf "test_duration: %d checks ok\n" !checks
