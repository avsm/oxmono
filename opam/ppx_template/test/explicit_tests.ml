(** Test that the explicit attributes work. *)

[@@@disable_unused_warnings]

(* Define values so they can be referred to by tests below *)

let x__global = ()
let x__local = ()
let x__heap = ()
let x__value = ()
let x__value__global = ()

(* mono tests *)

[@@@expand_inline
  (* basic mono *)
  let f x = x [@mode.explicit global]

  (* multiple attributes *)
  let f x = x [@mode.explicit global] [@kind value]
  let f x = x [@mode.explicit global] [@kind.explicit value]

  (* mono refers to parent variable, but is still explicit *)
  let%template[@mode l = global] f x = x [@mode.explicit l]

  (* mono refers to explicit parent variable, but is not explicit *)
  let%template[@mode.explicit l = global] f x = x [@mode l]

  (* mono refers to default parent variable, but is still explicit *)
  [%%template
  [@@@mode.default l = global]

  let f x = x [@mode.explicit l]]

  (* mono refers to explicit default parent variable, but is not explicit *)
  [%%template
  [@@@mode.default.explicit l = global]

  let f x = x [@mode l]]

  (* slightly more complicated version of the above *)
  [%%template
  [@@@mode.default l = (global, local)]

  let f x = x [@mode.explicit l] [@@mode l = (l, global)]]]

let f x = x__global
let f x = x__global
let f x = x__value__global
let f x = x__global
let f__global x = x

include struct
  let f x = x__global
end [@@ocaml.doc " @inline "]

include struct
  let f__global x = x
end [@@ocaml.doc " @inline "]

include struct
  let f x = x__global
end [@@ocaml.doc " @inline "]

include struct
  let f__local x = x__local
  and f x = x__global
end [@@ocaml.doc " @inline "]

[@@@end]

(* poly tests *)

[@@@expand_inline
  (* basic poly *)
  let%template f x = x [@@mode.explicit l = (global, local)]

  (* multiple attributes *)
  let%template f x = x [@@mode.explicit l = (global, local)] [@@kind value]

  (* multiple explicit attributes *)
  let%template f x = x [@@mode.explicit l = (global, local)] [@@kind.explicit value]

  (* poly refers to parent variable, but is still explicit *)
  [%%template
  [@@@mode.default l = (global, local)]

  let f x = x [@@mode.explicit l = (l, global)]]

  (* mono refers to explicit default parent variable, but is not explicit *)
  [%%template
  [@@@mode.default.explicit l = (global, local)]

  let f x = x [@@mode l = (l, global)]]]

let f__global x = x
and f__local x = x

let f__global x = x
and f__local x = x

let f__value__global x = x
and f__value__local x = x

include struct
  let f__global x = x
end [@@ocaml.doc " @inline "]

include struct
  let f__local x = x
  and f__global x = x
end [@@ocaml.doc " @inline "]

include struct
  let f x = x
end [@@ocaml.doc " @inline "]

include struct
  let f__local x = x
  and f x = x
end [@@ocaml.doc " @inline "]

[@@@end]

(* floating-poly tests *)

[@@@expand_inline
  (* basic floating poly *)
  [%%template
  [@@@mode.default.explicit l = (global, local)]

  let f x = x]

  (* multiple floating polys *)
  [%%template
  [@@@mode l1 = (global, local)]
  [@@@mode l2 = (global, local)]
  [@@@mode.default.explicit l1 l2]

  let f x = x]

  (* with non-floating poly *)
  [%%template
  [@@@mode l1 = (global, local)]
  [@@@mode l2 = (global, local)]

  let f x = x [@@mode.explicit l1 l2]]

  (* with non-floating poly but don't use explicit from parent *)
  [%%template
  [@@@mode l1 = (global, local)]
  [@@@mode l2 = (global, local)]
  [@@@mode.default.explicit l1 l2]

  let f x = x [@@mode l1 l2]]]

include struct
  let f__global x = x
end [@@ocaml.doc " @inline "]

include struct
  let f__local x = x
end [@@ocaml.doc " @inline "]

include struct
  include struct
    include struct
      let f__global__global x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let f__global__local x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    include struct
      let f__local__global x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let f__local__local x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    let f__global__global x = x
  end [@@ocaml.doc " @inline "]

  include struct
    let f__global__local x = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    let f__local__global x = x
  end [@@ocaml.doc " @inline "]

  include struct
    let f__local__local x = x
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    include struct
      let f x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let f__global__local x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  include struct
    include struct
      let f__local__global x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    include struct
      let f__local__local x = x
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]

(* other axes *)

[@@@expand_inline
  let f x = x [@modality.explicit global]
  let f x = x [@kind.explicit value]
  let f x = x [@alloc.explicit heap]]

let f x = x__global
let f x = x__value
let f x = x__heap

[@@@end]

(* check that we stably sort by sub-axis *)

[@@@expand_inline
  [%%template
  [@@@mode.default.explicit c = (contended, uncontended), p = (portable, nonportable)]

  let f x = x
  let g = (f [@mode.explicit c p])
  let h = (g [@mode.explicit p c])]]

include struct
  let f__portable__contended x = x
  let g__portable__contended = f__portable__contended
  let h__portable__contended = g__portable__contended
end [@@ocaml.doc " @inline "]

include struct
  let f__nonportable__contended x = x
  let g__nonportable__contended = f__nonportable__contended
  let h__nonportable__contended = g__nonportable__contended
end [@@ocaml.doc " @inline "]

include struct
  let f__portable__uncontended x = x
  let g__portable__uncontended = f__portable__uncontended
  let h__portable__uncontended = g__portable__uncontended
end [@@ocaml.doc " @inline "]

include struct
  let f__nonportable__uncontended x = x
  let g__nonportable__uncontended = f__nonportable__uncontended
  let h__nonportable__uncontended = g__nonportable__uncontended
end [@@ocaml.doc " @inline "]

[@@@end]

(* Check we have the same mangling for sets regardless of explicitness. *)

[@@@expand_inline
  module%template _ = struct
    module type S = sig end [@@kind_set _ = (value, bits64)]
  end

  module%template _ = struct
    module type S = sig end [@@kind_set.explicit _ = (value, bits64)]
  end]

module _ = struct
  module type S = sig end
  module type S__''bits64'' = sig end
end

module _ = struct
  module type S__''value'' = sig end
  module type S__''bits64'' = sig end
end

[@@@end]
