open! Core

let days_in_month = 30.

type direction =
  | Elapsed
  | Future

type stage =
  | Years
  | A_year
  | Months
  | A_month
  | Days
  | More_than_a_day
  | A_day
  | Hours
  | An_hour
  | Minutes
  | A_minute
  | Now
[@@deriving enumerate]

let lower_time_boundary_of stage =
  let open Time_ns.Span in
  match stage with
  | Years -> of_day (days_in_month *. 12.)
  | A_year -> of_day (days_in_month *. 10.)
  | Months -> of_day (days_in_month *. 2.)
  | A_month -> of_day 25.
  | Days -> of_hr 44.
  | More_than_a_day -> of_hr 28.
  | A_day -> of_hr 18.
  | Hours -> of_min 90.
  | An_hour -> of_min 45.
  | Minutes -> of_sec 90.
  | A_minute -> of_sec 30.
  | Now -> of_sec 0.
;;

let stage_of_span span direction =
  let open Time_ns.Span in
  let span = abs span in
  let comp =
    match direction with
    | Elapsed -> ( >= )
    | Future -> ( > )
  in
  List.find_exn all_of_stage ~f:(fun stage -> comp span (lower_time_boundary_of stage))
;;

let get_count span stage =
  let open Time_ns.Span in
  let span = abs span in
  match stage with
  | Years -> to_day span /. days_in_month /. 12. |> Float.iround_down_exn
  | A_year -> 1
  | Months -> to_day span /. days_in_month |> Float.iround_down_exn
  | A_month -> 1
  | Days -> to_day span |> Float.iround_down_exn
  | More_than_a_day -> 1
  | A_day -> 1
  | Hours -> to_hr span |> Float.iround_down_exn
  | An_hour -> 1
  | Minutes -> to_min span |> Float.iround_down_exn
  | A_minute -> 1
  | Now -> 1
;;

let to_string ?(now = Time_ns.now ()) reference =
  let span = Time_ns.diff now reference in
  let direction =
    match Time_ns.Span.sign span with
    | Sign.Pos | Zero -> Elapsed
    | Neg -> Future
  in
  let span = Time_ns.Span.abs span in
  let stage = stage_of_span span direction in
  let count = get_count span stage in
  let direction_string =
    match direction with
    | Elapsed -> "ago"
    | Future -> "from now"
  in
  match stage, count with
  | Years, 1 -> sprintf !"%d year %s" count direction_string
  | Years, _ -> sprintf !"%d years %s" count direction_string
  | A_year, _ -> sprintf !"about a year %s" direction_string
  | Months, 1 -> sprintf !"%d month %s" count direction_string
  | Months, _ -> sprintf !"%d months %s" count direction_string
  | A_month, _ -> sprintf !"about a month %s" direction_string
  | Days, 1 -> sprintf !"%d day %s" count direction_string
  | Days, _ -> sprintf !"%d days %s" count direction_string
  | More_than_a_day, _ -> sprintf !"more than a day %s" direction_string
  | A_day, _ -> sprintf !"about a day %s" direction_string
  | Hours, 1 -> sprintf !"%d hour %s" count direction_string
  | Hours, _ -> sprintf !"%d hours %s" count direction_string
  | An_hour, _ -> sprintf !"about an hour %s" direction_string
  | Minutes, 1 -> sprintf !"%d minute %s" count direction_string
  | Minutes, _ -> sprintf !"%d minutes %s" count direction_string
  | A_minute, _ -> sprintf !"about a minute %s" direction_string
  | Now, _ ->
    (match direction with
     | Elapsed -> "just now"
     | Future -> "in a moment")
;;
