(** A framebuffer expander that reads input_width * input_height 12-bit rgb pixel data
    from memory, scales it to output_width * output_height and outputs the VGA signals. *)

open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_kernel

module type Config = sig
  val input_width : int
  val input_height : int
  val output_width : int
  val output_height : int
  val start_address : int
  val vga_spec : Vga.Spec.t
  val vga_clock_div : int
end

module Make (Config : Config) (Memory : Memory_bus_intf.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_request : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { red : 'a
      ; green : 'a
      ; blue : 'a
      ; hsync : 'a
      ; vsync : 'a
      ; memory_request : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
