open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  include Nexys_a7_100t
end

(* The following is copied from the hardcaml documentation. *)

let cylon_eye ~clock ~enable =
  let spec = Reg_spec.create ~clock () in
  let eye_pos = Always.Variable.reg spec ~enable ~width:4 in
  let eye_dir = Always.Variable.reg spec ~enable ~width:1 in
  let eye_bar = List.init 16 ~f:(fun _ -> Always.Variable.reg spec ~enable ~width:3) in
  Always.(
    compile
      [ (* decrese intensity *)
        proc
          (List.map eye_bar ~f:(fun eye ->
             proc [ eye <-- eye.value -:. 1; when_ (eye.value ==:. 0) [ eye <--. 0 ] ]))
      ; (* set current eye to max intensity *)
        proc
          (List.mapi eye_bar ~f:(fun idx eye ->
             when_ (eye_pos.value ==:. idx) [ eye <--. 7 ]))
      ; (* scan left and right *)
        if_
          eye_dir.value
          [ eye_pos <-- eye_pos.value -:. 1
          ; when_ (eye_pos.value ==:. 1) [ eye_dir <-- gnd ]
          ]
          [ eye_pos <-- eye_pos.value +:. 1
          ; when_ (eye_pos.value ==:. 14) [ eye_dir <-- vdd ]
          ]
      ]);
  List.map eye_bar ~f:(fun eye -> eye.value)
;;

let cylon_eye_top ~scope ~enable_rate ~clock =
  let spec = Reg_spec.create ~clock () in
  let%hw_var enable = Always.Variable.reg spec ~width:1 in
  let eyes = cylon_eye ~clock ~enable:enable.value in
  let%hw_var count = Always.Variable.reg spec ~width:22 in
  let leds =
    List.init (List.length eyes) ~f:(fun _ -> Always.Variable.reg spec ~width:1)
  in
  Always.(
    compile
      [ (* Control the enable. *)
        enable <-- gnd
      ; count <-- count.value +:. 1
      ; when_ (count.value ==:. enable_rate - 1) [ count <--. 0; enable <-- vdd ]
      ; (* PWM for each LED *)
        proc
          (List.map2_exn leds eyes ~f:(fun led eye ->
             led <-- (eye >=: count.value.:[2, 0])))
      ]);
  List.map leds ~f:(fun led -> led.value)
;;

let create () =
  let scope = Scope.create () in
  let board = Board.create () in
  let clocking = Clock_and_reset.create board in
  Leds.complete
    board
    (cylon_eye_top ~scope ~enable_rate:3_333_333 ~clock:clocking.clock_100 |> concat_lsb);
  board
;;
