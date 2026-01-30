open! Core

module V1 :
  Bidirectional_map_interfaces_stable.S
  with type ('l, 'lc, 'r, 'rc) t = ('l, 'lc, 'r, 'rc) Bidirectional_multimap.t
