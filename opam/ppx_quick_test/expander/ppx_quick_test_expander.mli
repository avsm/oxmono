open Ppxlib

val expand : loc:location -> value_binding -> rec_flag -> expression -> expression
val enclose_impl : location -> structure * structure
