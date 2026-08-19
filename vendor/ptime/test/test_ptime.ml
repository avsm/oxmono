(* Pins the three module-level tables this copy replaced, exhaustively where
   the domain is small enough to enumerate. Dune skips aliases under a
   vendored directory, so this must be run by name:

     dune exec vendor/ptime/test/test_ptime.exe

   The smaller, runtest-visible copy of the portability guarantees lives in
   avsm/arod/test/test_ptime.ml. *)

let checks = ref 0
let fail name = prerr_endline ("FAIL: " ^ name); exit 1
let check name b = incr checks; if not b then fail name
let get = function None -> invalid_arg "no timestamp" | Some v -> v

let is_leap y = (y mod 4 = 0) && (y mod 100 <> 0 || y mod 400 = 0)

(* [Ptime.of_date] answers [None] on a day past the end of its month, so
   accepting exactly the days a month has pins the month-length match that
   replaced upstream's array. *)

let () =
  for y = 0 to 9999 do
    for m = 1 to 12 do
      let len = match m with
      | 2 -> if is_leap y then 29 else 28
      | 4 | 6 | 9 | 11 -> 30
      | _ -> 31
      in
      for d = 1 to 32 do
        let ok = Ptime.of_date (y, m, d) <> None in
        if ok <> (d <= len) then
          fail (Printf.sprintf "%04d-%02d-%02d accepted=%b" y m d ok)
      done
    done
  done;
  incr checks

(* Every day from 1970 through 1972 walks the weekday match that replaced
   upstream's array, and 1970-01-01 is known to have been a thursday. *)

let () =
  let names = [| "sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat" |] in
  let name = function
  | `Sun -> "sun" | `Mon -> "mon" | `Tue -> "tue" | `Wed -> "wed"
  | `Thu -> "thu" | `Fri -> "fri" | `Sat -> "sat"
  in
  for d = 0 to 1095 do
    let t = get (Ptime.of_span (Ptime.Span.unsafe_of_d_ps (d, 0L))) in
    let n = Ptime.weekday_num t in
    if n <> (d + 4) mod 7 then fail (Printf.sprintf "day %d numbered %d" d n);
    if not (String.equal (name (Ptime.weekday t)) names.(n)) then
      fail (Printf.sprintf "day %d named %s" d (name (Ptime.weekday t)))
  done;
  incr checks

(* Each fractional digit count renders one more digit, which pins the divisor
   match that replaced upstream's array. Out of range counts keep raising what
   [Array.get] raised, since [Ptime.truncate] does not clip its argument. *)

let () =
  let t = get (Ptime.of_rfc3339 "2026-08-19T09:41:07.123456789012Z"
               |> function Ok (t, _, _) -> Some t | Error _ -> None)
  in
  let expect =
    [| "2026-08-19T09:41:07Z"; "2026-08-19T09:41:07.1Z";
       "2026-08-19T09:41:07.12Z"; "2026-08-19T09:41:07.123Z";
       "2026-08-19T09:41:07.1234Z"; "2026-08-19T09:41:07.12345Z";
       "2026-08-19T09:41:07.123456Z"; "2026-08-19T09:41:07.1234567Z";
       "2026-08-19T09:41:07.12345678Z"; "2026-08-19T09:41:07.123456789Z";
       "2026-08-19T09:41:07.1234567890Z"; "2026-08-19T09:41:07.12345678901Z";
       "2026-08-19T09:41:07.123456789012Z" |]
  in
  for frac = 0 to 12 do
    check (Printf.sprintf "frac_s %d renders" frac)
      (String.equal (Ptime.to_rfc3339 ~frac_s:frac ~tz_offset_s:0 t) expect.(frac))
  done;
  check "an out of range frac_s still raises in truncate"
    (match Ptime.truncate ~frac_s:13 t with
     | _ -> false
     | exception Invalid_argument m -> String.equal m "index out of bounds");
  check "a negative frac_s still raises in truncate"
    (match Ptime.truncate ~frac_s:(-1) t with
     | _ -> false
     | exception Invalid_argument m -> String.equal m "index out of bounds");
  check "round clips instead of raising"
    (Ptime.Span.equal
       (Ptime.Span.round ~frac_s:13 (Ptime.to_span t))
       (Ptime.Span.round ~frac_s:12 (Ptime.to_span t)))

let () = Printf.printf "test_ptime: %d checks ok\n" !checks
