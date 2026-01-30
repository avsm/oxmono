open! Core
open Hardcaml_axi
module Make (Axi : Stream.S) : Prepend_address_intf.M(Axi).S
