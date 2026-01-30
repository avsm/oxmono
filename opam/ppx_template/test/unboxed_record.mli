type%template ('a : k) t =
  { x : int
  ; a : 'a
  }
[@@kind k = base]

[@@@expand_inline:
  val%template unbox : ('a : k). ('a t[@kind k]) -> ('a t#[@kind k]) [@@kind k = base]]

[@@@ocaml.text "/*"]

val unbox__bits64 : ('a : bits64). 'a t__bits64 -> 'a t__bits64#
val unbox__bits32 : ('a : bits32). 'a t__bits32 -> 'a t__bits32#
val unbox__word : ('a : word). 'a t__word -> 'a t__word#
val unbox__float64 : ('a : float64). 'a t__float64 -> 'a t__float64#
val unbox__float32 : ('a : float32). 'a t__float32 -> 'a t__float32#

[@@@ocaml.text "/*"]

val unbox : ('a : value). 'a t -> 'a t#

[@@@end]
