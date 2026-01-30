(* Basic coercions *)

[@@@expand_inline
  let%template u = () [@@kind k = (value, bits64)]
  let%template _use_u = (u [@kind k or value]) [@@kind k = (value, immediate64, bits64)]
  let%template u = () [@@kind k = (value_or_null mod separable, bits64)]

  let%template _use_u = (u [@kind k or (value_or_null mod separable)])
  [@@kind k = (mutable_data, bits64)]
  ;;]

let u = ()
and u__bits64 = ()

let _use_u = u
and _use_u__immediate64 = u
and _use_u__bits64 = u__bits64

let u__'value_or_null_mod_separable' = ()
and u__bits64 = ()

let _use_u__mutable_data = u__'value_or_null_mod_separable'
and _use_u__bits64 = u__bits64

[@@@end]

(* Coerce a kind set

   base_with_imm contains
   - bits64
   - bits32
   - word
   - float64
   - float32
   - value
   - immediate
   - immediate64

   immediate and immedate64 are subkinds of value so they don't appear
*)
[@@@expand_inline let%template _u = () [@@kind k = (base_with_imm or value)]]

let _u__bits64 = ()
and _u__bits32 = ()
and _u__word = ()
and _u__float64 = ()
and _u__float32 = ()
and _u = ()

[@@@end]

(* Coerce by a kind set

   same as previous test. base is the same as base_with_imm but does not contain immediate
   or immediate64
*)
[@@@expand_inline let%template _u = () [@@kind k = (base_with_imm or base)]]

let _u__bits64 = ()
and _u__bits32 = ()
and _u__word = ()
and _u__float64 = ()
and _u__float32 = ()
and _u = ()

[@@@end]

(* Coerce in a product *)

let%template u = () [@@kind k = (base_or_null & base_or_null)]

let%template _use_u = (u [@kind (k1 or value_or_null) & (k2 or value_or_null)])
[@@kind k1 = base_or_null, k2 = base_or_null]
;;

(* Coerce to the more specific of several comparable options *)

[@@@expand_inline
  [%%template
  [@@@kind_set.define ks = (value_or_null, value_or_null mod external64)]

  let u = () [@@kind ks & value]
  let _use_u = (u [@kind (k or ks) & value]) [@@kind k = (value, immediate64)]]]

let u__'value_or_null_value' = ()
and u__''value_or_null_mod_external64'_value' = ()

let _use_u = u__'value_or_null_value'
and _use_u__immediate64 = u__''value_or_null_mod_external64'_value'

[@@@end]
