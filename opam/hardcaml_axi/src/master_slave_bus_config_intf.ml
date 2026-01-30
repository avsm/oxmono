open Base
open! Hardcaml

module type S = sig
  val data_bits : int
  val addr_bits : int
end

module type Master_slave_bus_config = sig
  module type S = S
end
