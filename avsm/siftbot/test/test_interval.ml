module I = Siftbot.Interval

let time s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | _ -> failwith "bad fixture"

let check now interval expected =
  let actual = I.since ~now:(time now) (I.parse interval) in
  if not (Ptime.equal actual (time expected)) then
    failwith ("Wrong interval: " ^ interval)

let refused text =
  match I.since ~now:(time "2026-10-25T12:34:56Z") (I.parse text) with
  | _ -> failwith ("Accepted invalid interval: " ^ text)
  | exception Invalid_argument _ -> ()

let () =
  check "2026-10-25T12:34:56Z" "6m" "2026-04-25T12:34:56Z";
  check "2026-08-31T12:34:56Z" "6m" "2026-02-28T12:34:56Z";
  check "2024-08-31T12:34:56Z" "6m" "2024-02-29T12:34:56Z";
  check "2024-02-29T12:34:56Z" "1y" "2023-02-28T12:34:56Z";
  check "2026-02-28T12:34:56Z" "6m" "2025-08-28T12:34:56Z";
  check "2026-10-25T12:34:56Z" "2w" "2026-10-11T12:34:56Z";
  check "2026-10-25T12:34:56Z" "1d" "2026-10-24T12:34:56Z";
  check "2026-10-25T12:34:56Z" "2026-04-25T13:34:56+01:00"
    "2026-04-25T12:34:56Z";
  List.iter refused
    [
      "";
      "6";
      "0m";
      "-1m";
      "6M";
      "0x10d";
      "1.5m";
      "six months";
      "9999999999999999999999d";
      "999999999999999999d";
      "999999m";
      "2026-10-25T12:34:56Z";
      "2027-01-01T00:00:00Z";
    ];
  print_endline "Sift import intervals, month boundaries and validation passed"
