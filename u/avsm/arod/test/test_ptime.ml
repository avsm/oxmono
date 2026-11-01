(* [ptime] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod renders pages and builds feeds in.
   Dune skips aliases under a vendored directory, so nothing else here reaches
   the patch. This test fails if a re-vendor drops it, and it pins the
   conversions the render path depends on. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* [get] is used from the portable closures below, so it must not close over
   anything nonportable. [exit] is nonportable, hence the exception. *)
let get = function None -> invalid_arg "no timestamp" | Some v -> v

(* The ascriptions are the point. None of these compile unless [ptime.mli]
   still carries its [@@ portable] annotation. *)

let parse : (string -> Ptime.t option) @ portable =
 fun s -> match Ptime.of_rfc3339 s with Ok (t, _, _) -> Some t | Error _ -> None

let render : (Ptime.t -> string) @ portable =
 fun t -> Ptime.to_rfc3339 ~tz_offset_s:0 t
let day : (Ptime.t -> Ptime.date) @ portable = fun t -> Ptime.to_date t
let show : (Format.formatter -> Ptime.t -> unit) @ portable = Ptime.pp

let clock_now : (unit -> Ptime.t) @ portable = Ptime_clock.now

(* [epoch_date] and [after_epoch] do not compile unless [Ptime.t] and
   [Ptime.span] still have the [immutable_data] kind. A portable closure reads
   a module-level value only if its type crosses portability and contention,
   and both of these are read from one. Passing a timestamp as an argument
   would prove nothing, since a type used only as a parameter or a result need
   not cross anything. *)

let hour = Ptime.Span.of_int_s 3600

let epoch_date : (unit -> Ptime.date) @ portable = fun () -> Ptime.to_date Ptime.epoch
let after_epoch : (unit -> string) @ portable =
 fun () -> render (get (Ptime.add_span Ptime.epoch hour))

let latest : (unit -> string) @ portable = fun () -> render Ptime.max
let earliest : (unit -> string) @ portable = fun () -> render Ptime.min

let () =
  check "the epoch is 1970-01-01" (epoch_date () = (1970, 01, 01));
  check "a module-level span adds to the epoch"
    (String.equal (after_epoch ()) "1970-01-01T01:00:00Z");
  check "min renders" (String.equal (earliest ()) "0000-01-01T00:00:00Z");
  check "max renders" (String.equal (latest ()) "9999-12-31T23:59:59Z");
  check "an RFC 3339 stamp round trips"
    (String.equal (render (get (parse "2026-08-19T09:41:07Z"))) "2026-08-19T09:41:07Z");
  check "a zone offset is resolved to UTC"
    (String.equal (render (get (parse "2026-08-19T09:41:07+02:00")))
       "2026-08-19T07:41:07Z");
  check "a leap second maps to the next second"
    (String.equal (render (get (parse "1998-12-31T23:59:60Z")))
       "1999-01-01T00:00:00Z");
  check "a malformed stamp is an error" (Option.is_none (parse "2026-08-19"));
  check "a leap day converts back to a date"
    (day (get (parse "2024-02-29T12:00:00Z")) = (2024, 02, 29));
  check "a day that is not in the year is rejected"
    (Option.is_none (parse "2026-02-29T00:00:00Z"));
  check "pp is the human form"
    (String.equal (Format.asprintf "%a" show Ptime.epoch)
       "1970-01-01 00:00:00 +00:00");
  check "the clock reads after the epoch"
    (Ptime.is_later (clock_now ()) ~than:Ptime.epoch);
  Printf.printf "test_ptime: %d checks ok\n" !checks
