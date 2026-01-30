open! Ppx_template_test_common

[@@@disable_unused_warnings]

[@@@expand_inline
  [%%template
  [@@@kind_set.define or_pair = (value, value & value)]
  [@@@kind_set ks = (bits64, or_pair)]
  [@@@kind.default_if_multiple k = ks]

  let id (x : (_ : k)) = x]]

include struct
  include struct
    let id (x : (_ : bits64)) = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    let id (x : (_ : value)) = x
  end [@@ocaml.doc " @inline "]

  include struct
    let id__'value_value' (x : (_ : value & value)) = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]
