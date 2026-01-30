open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  include Arty_a7
end

let create () =
  let board = Board.create () in
  let spec = Utils.sync_reg_spec (Clock_and_reset.create board) in
  let counter = reg_fb spec ~width:27 ~f:(fun d -> d +:. 1) in
  Leds.complete board (sel_top counter ~width:4);
  board
;;
