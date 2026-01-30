type%template ('a : k) t =
  { x : int
  ; a : 'a
  }
[@@kind k = base]

[@@@expand_inline
  let%template unbox (type a : k) ({ x; a } : (a t[@kind k])) : (a t#[@kind k]) =
    #{ x; a }
  [@@kind k = base]
  ;;]

let unbox__bits64 (type a : bits64) ({ x; a } : a t__bits64) : a t__bits64# = #{ x; a }
and unbox__bits32 (type a : bits32) ({ x; a } : a t__bits32) : a t__bits32# = #{ x; a }
and unbox__word (type a : word) ({ x; a } : a t__word) : a t__word# = #{ x; a }

and unbox__float64 (type a : float64) ({ x; a } : a t__float64) : a t__float64# =
  #{ x; a }

and unbox__float32 (type a : float32) ({ x; a } : a t__float32) : a t__float32# =
  #{ x; a }

and unbox (type a : value) ({ x; a } : a t) : a t# = #{ x; a }

[@@@end]
