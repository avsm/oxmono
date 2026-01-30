open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  include Nexys_a7_100t
end

module Make (Digits : sig
    val num_digits : int
  end) =
struct
  open Digits

  (* Bits required to represent [999...999] *)
  let binary_bits = num_bits_to_represent (Int.pow 10 num_digits - 1)

  module State = struct
    type t =
      | Start
      | Double
      | Dabble
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ~clock ~clear ~start ~binary_in =
    let spec = Reg_spec.create ~clock ~clear () in
    (* Registers to latch the input binary value and count through it's bits. *)
    let binary = Always.Variable.reg spec ~width:binary_bits in
    let bit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 binary_bits) in
    (* Register to count through digits while dabbling. *)
    let digit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 num_digits) in
    (* One digit count to use as a register write enable for the bcd digits *)
    let digit_count_one_hot = binary_to_onehot digit_count.value in
    (* Registers for the BCD digit *)
    let bcd = Array.init num_digits ~f:(fun _ -> Always.Variable.reg spec ~width:4) in
    (* Dabbling logic - look up the current bcd digit and perform dabble operation if
       greater than 4 *)
    let bcd_dabbled =
      let digit =
        mux digit_count.value (List.map (Array.to_list bcd) ~f:(fun bcd -> bcd.value))
      in
      mux2 (digit >:. 4) (digit +:. 3) digit
    in
    (* Statemachine *)
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ (* Wait for start *)
                  bit_count <--. 0
                ; binary <-- binary_in
                ; when_
                    start
                    [ proc (List.init num_digits ~f:(fun digit -> bcd.(digit) <--. 0))
                    ; sm.set_next Double
                    ]
                ] )
            ; ( Double
              , [ (* Shift in the next binary bit through all the BCD registers. *)
                  binary <-- sll binary.value ~by:1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       bcd.(digit)
                       <-- lsbs bcd.(digit).value
                           @:
                           if digit = 0
                           then msb binary.value
                           else msb bcd.(digit - 1).value))
                ; digit_count <--. 0
                ; bit_count <-- bit_count.value +:. 1
                ; (* Count through all the input binary bits *)
                  if_
                    (bit_count.value ==:. binary_bits - 1)
                    [ sm.set_next Start ]
                    [ sm.set_next Dabble ]
                ] )
            ; ( Dabble
              , [ (* Iterate through each digit and perform the dabble operation. *)
                  digit_count <-- digit_count.value +:. 1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       when_ digit_count_one_hot.:(digit) [ bcd.(digit) <-- bcd_dabbled ]))
                ; when_ (digit_count.value ==:. num_digits - 1) [ sm.set_next Double ]
                ] )
            ]
        ]);
    sm.is Start, Array.map bcd ~f:(fun bcd -> bcd.value)
  ;;
end

module BCD = Make (struct
    let num_digits = 5
  end)

let to_ssd_code v =
  mux
    v
    (Hardcaml_hobby_boards_kernel.Seven_segment_display.numeric_codes
     |> Array.to_list
     |> List.map ~f:Hardcaml_hobby_boards_kernel.Seven_segment_display.to_signal)
;;

let decimal_display ~spec ~switches =
  let start = Always.Variable.reg spec ~width:1 in
  let _, bcd =
    BCD.create
      ~clock:(Reg_spec.clock spec)
      ~clear:(Reg_spec.clear_exn spec)
      ~start:start.value
      ~binary_in:(gnd @: switches)
  in
  let set_n = Always.Variable.reg spec ~clear_to:(ones 8) ~width:8 in
  let select_n = Always.Variable.reg spec ~clear_to:(ones 8) ~width:8 in
  let digit = Always.Variable.reg spec ~width:3 in
  let enable_count = Always.Variable.reg spec ~width:17 in
  Always.(
    compile
      [ enable_count <-- enable_count.value +:. 1
      ; start <-- gnd
      ; when_ (enable_count.value ==:. 50_000) [ start <-- vdd ]
      ; when_
          (enable_count.value ==:. 99_999)
          [ enable_count <--. 0
          ; digit <-- digit.value +:. 1
          ; when_ (digit.value ==:. 4) [ digit <--. 0 ]
          ; set_n <-- ~:(mux digit.value (Array.to_list bcd) |> to_ssd_code)
          ; select_n <-- ~:(binary_to_onehot digit.value)
          ]
      ]);
  set_n.value, select_n.value
;;

let create () =
  let board = Board.create () in
  let clocking = Clock_and_reset.create board in
  let spec = Utils.sync_reg_spec clocking in
  let switches = Switches.create board in
  let set_n, select_n = decimal_display ~spec ~switches in
  Seven_segment_display.complete board { Seven_segment_display.O.set_n; select_n };
  Leds.complete board switches;
  board
;;
