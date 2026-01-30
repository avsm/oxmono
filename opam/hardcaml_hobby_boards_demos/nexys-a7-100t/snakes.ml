open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  include Nexys_a7_100t
end

module Snake_states = struct
  type t =
    | Right_top
    | Down_top
    | Left_mid1
    | Down_left
    | Right_bottom
    | Up_bottom
    | Left_mid2
    | Up_mid
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let snake_machine scope spec rep =
  let%hw.Always.State_machine snake_machine =
    Always.State_machine.create (module Snake_states) spec
  in
  let limit = mux rep ([ 7; 3; 1; 0 ] |> List.map ~f:(of_unsigned_int ~width:3)) in
  let horz_count = Always.Variable.reg spec ~width:3 in
  let horz_one_hot = binary_to_onehot horz_count.value in
  let horz_one_hot =
    mux
      rep
      [ horz_one_hot
      ; repeat horz_one_hot.:[3, 0] ~count:2
      ; repeat horz_one_hot.:[1, 0] ~count:4
      ; repeat horz_one_hot.:[0, 0] ~count:8
      ]
  in
  let sets =
    Array.init 8 ~f:(fun _ -> Always.Variable.reg spec ~clear_to:(ones 8) ~width:8)
  in
  let set ~f = Always.proc (List.mapi (Array.to_list sets) ~f) in
  let code = Always.Variable.wire ~default:(zero 8) () in
  let scan_right code' next =
    Always.
      [ code <--. 1 lsl code'
      ; horz_count <-- horz_count.value +:. 1
      ; when_
          (horz_count.value ==: limit)
          [ horz_count <-- limit; snake_machine.set_next next ]
      ]
  in
  let scan_left code' next =
    Always.
      [ code <--. 1 lsl code'
      ; horz_count <-- horz_count.value -:. 1
      ; when_ (horz_count.value ==:. 0) [ horz_count <--. 0; snake_machine.set_next next ]
      ]
  in
  let set_code code' next =
    Always.[ code <--. 1 lsl code'; snake_machine.set_next next ]
  in
  let snake_machine =
    Always.(
      proc
        [ set ~f:(fun _ s -> s <--. 0)
        ; set ~f:(fun i s -> when_ horz_one_hot.:(i) [ s <-- code.value ])
        ; snake_machine.switch
            [ Right_top, scan_right 0 Down_top
            ; Down_top, set_code 1 Left_mid1
            ; Left_mid1, scan_left 6 Down_left
            ; Down_left, set_code 4 Right_bottom
            ; Right_bottom, scan_right 3 Up_bottom
            ; Up_bottom, set_code 2 Left_mid2
            ; Left_mid2, scan_left 6 Up_mid
            ; Up_mid, set_code 5 Right_top
            ]
        ])
  in
  let sets = Array.map sets ~f:(fun s -> s.value) |> Array.to_list in
  sets, snake_machine
;;

let create_snakes ~scope ~spec ~switches ~scan_update ~snake_update =
  let set_n = Always.Variable.reg spec ~clear_to:(ones 8) ~width:8 in
  let select_n = Always.Variable.reg spec ~clear_to:(ones 8) ~width:8 in
  let%hw_var digit_count = Always.Variable.reg spec ~width:3 in
  let%hw_var scan_count = Always.Variable.reg spec ~width:17 in
  let%hw_var snake_update_count = Always.Variable.reg spec ~width:10 in
  let sets, snake_machine =
    snake_machine
      scope
      spec
      (* gray code ordering of switches which is a little easier to cycle through *)
      (mux switches.:[1, 0] ([ 0; 1; 3; 2 ] |> List.map ~f:(of_unsigned_int ~width:2)))
  in
  let%hw_list sets in
  let%hw_var_list sets1 = List.init 8 ~f:(fun _ -> Always.Variable.reg spec ~width:8) in
  let%hw_var_list sets2 = List.init 8 ~f:(fun _ -> Always.Variable.reg spec ~width:8) in
  let set =
    List.transpose_exn
      [ sets
      ; sets1 |> List.map ~f:(fun d -> d.value)
      ; sets2 |> List.map ~f:(fun d -> d.value)
      ]
    |> List.map ~f:(fun d -> reduce ~f:( |: ) d)
    |> mux digit_count.value
  in
  Always.(
    compile
      [ scan_count <-- scan_count.value +:. 1
      ; set_n <-- ~:set
      ; select_n <-- ~:(binary_to_onehot digit_count.value)
      ; when_
          (scan_count.value ==:. scan_update - 1)
          [ scan_count <--. 0
          ; digit_count <-- digit_count.value +:. 1
          ; snake_update_count <-- snake_update_count.value +:. 1
          ; when_
              (snake_update_count.value ==:. snake_update - 1)
              [ snake_update_count <--. 0
              ; snake_machine
              ; proc (List.map2_exn sets1 sets ~f:(fun a b -> a <-- b))
              ; proc (List.map2_exn sets2 sets1 ~f:(fun a b -> a <-- b.value))
              ]
          ]
      ]);
  set_n.value, reverse select_n.value
;;

let create () =
  let board = Board.create () in
  let scope = Scope.create () in
  let clocking = Clock_and_reset.create board in
  let spec = Utils.sync_reg_spec clocking in
  let switches = Switches.create board in
  let snake_set_n, snake_select_n =
    create_snakes ~scope ~spec ~switches ~scan_update:100_000 ~snake_update:32
  in
  let set_n = snake_set_n in
  let select_n = snake_select_n in
  Seven_segment_display.complete board { Seven_segment_display.O.set_n; select_n };
  Leds.complete
    board
    (Cylon.cylon_eye_top ~scope ~enable_rate:3_333_333 ~clock:clocking.clock_100
     |> concat_lsb);
  board
;;
