open Core
open Async_kernel
open Incr_dom
open Js_of_ocaml

module Format = struct
  type t =
    | Time_ago
    | Short_string_ago
    | Short_string_until
    | Custom of (now:Time_ns.t -> reference:Time_ns.t -> string)

  let print t ~now reference =
    match t with
    | Time_ago -> Time_ago.to_string ~now reference
    | Short_string_ago -> Time_ns.Span.to_short_string (Time_ns.diff now reference)
    | Short_string_until -> Time_ns.Span.to_short_string (Time_ns.diff reference now)
    | Custom f -> f ~now ~reference
  ;;
end

module State = struct
  type t =
    { format : Format.t
    ; reference : Time_ns.t
    ; tick_event : (unit, unit) Clock_ns.Event.t option ref
    }
end

let id =
  Type_equal.Id.create
    ~name:"Time ago widget"
    (fun (_ : State.t * Dom_html.element Js.t) -> Sexp.Atom "time-ago-widget")
;;

let time_to_next_change format now reference =
  let curr_string = Format.print format ~now reference in
  let rec search low high =
    if Time_ns.Span.(high - low < of_sec 1.)
    then high
    else (
      let mid = Time_ns.Span.((low + high) / 2.) in
      let now = Time_ns.add now mid in
      if String.equal curr_string (Format.print format ~now reference)
      then search mid high
      else search low mid)
  in
  (* 131072 is the first power of 2 over the number of seconds in a day *)
  Time_ns.Span.(search (of_sec 0.) (of_sec 131072.))
;;

let rec tick_and_schedule_next
  ({ format; reference; tick_event } as t : State.t)
  (dom_node : Dom_html.element Js.t)
  =
  let now = Time_ns.now () in
  let time_string = Format.print format ~now reference in
  dom_node##.textContent := Js.Opt.return (Js.string time_string);
  let event =
    let delay = time_to_next_change format now reference in
    let callback () = tick_and_schedule_next t dom_node in
    Async_kernel.Clock_ns.Event.run_after delay callback ()
  in
  tick_event := Some event
;;

let init ~format ~reference =
  let dom_node = Dom_html.document##createElement (Js.string "span") in
  let tick_event = ref None in
  let t = { State.format; reference; tick_event } in
  tick_and_schedule_next t dom_node;
  t, dom_node
;;

let stop_updates tick_event =
  match !tick_event with
  | None -> ()
  | Some event ->
    ignore (Clock_ns.Event.abort event () : (unit, unit) Time_source.Event.Abort_result.t);
    tick_event := None
;;

let update ~reference (prev_state : State.t) (dom_node : Dom_html.element Js.t) =
  if Time_ns.equal reference prev_state.reference
  then prev_state, dom_node
  else (
    stop_updates prev_state.tick_event;
    let t = { prev_state with reference } in
    tick_and_schedule_next t dom_node;
    t, dom_node)
;;

let view ?(format = Format.Time_ago) reference =
  Vdom.Node.widget
    ~id
    ~update:(fun prev_state dom_node -> update ~reference prev_state dom_node)
    ~init:(fun () -> init ~format ~reference)
    ~destroy:(fun (prev_state : State.t) (_ : #Dom_html.element Js.t) ->
      stop_updates prev_state.tick_event)
    ()
;;

module%test _ = struct
  let time_init = Time_ns.epoch

  let print_time_to_next_change format span =
    let now = Time_ns.add time_init span in
    time_to_next_change format now time_init |> printf !"%{Time_ns.Span}\n"
  ;;

  let%expect_test "next change - elapsed: Time_ago" =
    Time_ns.Span.
      [ of_sec 29.
      ; of_sec 29. + of_ms 500.
      ; of_min 2.
      ; of_min 44. + of_sec 12.
      ; of_min 45. + of_sec 30.
      ; of_min 89. + of_sec 30.
      ; of_min 90.
      ; of_hr 17. + of_min 20.
      ; of_hr 18.
      ; of_hr 27. + of_min 40.
      ; of_hr 28. + of_min 30.
      ; of_hr 43. + of_min 11.
      ; of_hr 44. + of_min 30.
      ; of_day 3. + of_hr 12.
      ; of_day 299.
      ; of_day 359.
      ]
    |> List.iter ~f:(print_time_to_next_change Time_ago);
    [%expect
      {|
      1s
      500ms
      1m
      48s
      44m30s
      30s
      30m
      40m
      10h
      20m
      15h30m
      49m
      3h30m
      12h
      1d
      1d
      |}]
  ;;

  let%expect_test "next change - elapsed: Short_string_ago" =
    Time_ns.Span.
      [ of_sec 29.
      ; of_sec 29. + of_ms 500.
      ; of_min 2.
      ; of_min 44. + of_sec 12.
      ; of_min 45. + of_sec 30.
      ; of_min 89. + of_sec 30.
      ; of_min 90.
      ; of_hr 17. + of_min 20.
      ; of_hr 18.
      ; of_hr 27. + of_min 40.
      ; of_hr 28. + of_min 30.
      ; of_hr 43. + of_min 11.
      ; of_hr 44. + of_min 30.
      ; of_day 3. + of_hr 12.
      ; of_day 299.
      ; of_day 359.
      ]
    |> List.iter ~f:(print_time_to_next_change Short_string_ago);
    [%expect
      {|
      1s
      500ms
      6s
      48s
      30s
      30s
      6m
      40m
      1h
      1h20m
      30m
      49m
      1h30m
      3h
      1d
      1d
      |}]
  ;;

  let%expect_test "next change out of search range - elapsed" =
    Time_ns.Span.[ of_day 30. + of_hr 12.; of_day 300.; of_day 700. ]
    |> List.iter ~f:(print_time_to_next_change Time_ago);
    [%expect
      {|
      1d12h24m32s
      1d12h24m32s
      1d12h24m32s
      |}]
  ;;

  let%expect_test "next change - future" =
    Time_ns.Span.
      [ of_sec 31.
      ; of_sec 90.
      ; of_sec 90. + of_ms 500.
      ; of_min 45. - of_ms 1.
      ; of_min 45. + of_sec 12.
      ; of_min 90.
      ; of_min 90. + of_sec 32.
      ; of_hr 18. - of_ms 1.
      ; of_hr 18. + of_min 20.
      ; of_hr 28. - of_ms 1.
      ; of_hr 28. + of_min 40.
      ; of_hr 44. - of_ms 1.
      ; of_hr 44. + of_min 11.
      ; of_day 3. - of_ms 1.
      ; of_day 26. - of_ms 1.
      ; of_day 300.
      ]
    |> List.map ~f:Time_ns.Span.neg
    |> List.iter ~f:(print_time_to_next_change Time_ago);
    [%expect
      {|
      1s
      1m
      500ms
      1m
      12s
      45m
      32s
      1h
      20m
      10h
      40m
      16h
      11m
      1d
      1d
      500ms
      |}]
  ;;

  let%expect_test "next change out of search range - future" =
    Time_ns.Span.[ of_day 32.; of_day 299. ]
    |> List.map ~f:Time_ns.Span.neg
    |> List.iter ~f:(print_time_to_next_change Time_ago);
    [%expect
      {|
      1d12h24m32s
      1d12h24m32s
      |}]
  ;;
end
