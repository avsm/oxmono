type t = Seconds of int | Months of int | Timestamp of Ptime.t

let invalid () =
  invalid_arg "Use --since Nd, Nw, Nm, Ny or an RFC3339 timestamp before now"

let multiply n factor =
  if n > max_int / factor then invalid ();
  n * factor

let parse text =
  let length = String.length text in
  let relative () =
    if length < 2 then invalid ();
    let number = String.sub text 0 (length - 1) in
    if not (String.for_all (fun c -> c >= '0' && c <= '9') number) then
      invalid ();
    let n =
      match int_of_string_opt number with
      | Some n when n > 0 -> n
      | _ -> invalid ()
    in
    match text.[length - 1] with
    | 'd' -> Seconds (multiply n 86400)
    | 'w' -> Seconds (multiply n (7 * 86400))
    | 'm' -> Months n
    | 'y' -> Months (multiply n 12)
    | _ -> invalid ()
  in
  match Ptime.of_rfc3339 text with
  | Ok (t, _, consumed) when consumed = length -> Timestamp t
  | _ -> relative ()

let since ~now interval =
  let start =
    match interval with
    | Timestamp t -> t
    | Seconds seconds -> (
        match Ptime.sub_span now (Ptime.Span.of_int_s seconds) with
        | Some t -> t
        | None -> invalid ())
    | Months n ->
        let (year, month, day), time = Ptime.to_date_time now in
        let index = (year * 12) + month - 1 in
        if n > index then invalid ();
        let index = index - n in
        let rec clamp day =
          if day < 1 then invalid ();
          match
            Ptime.of_date_time ((index / 12, (index mod 12) + 1, day), time)
          with
          | Some t -> t
          | None -> clamp (day - 1)
        in
        clamp day
  in
  if Ptime.compare start now >= 0 then invalid ();
  start
