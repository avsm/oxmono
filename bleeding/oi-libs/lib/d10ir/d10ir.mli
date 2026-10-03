(** d10ir: a build IR that produces d10 layers.

    A {!Plan.t} is a graph of nodes, each describing how to build one
    {!D10.Layer.t}. The {!Direct} executor consumes a plan and a {!Config.t},
    dispatches a fiber per node, and writes layers into a d10 store.

    Producers (like [oi]) emit plans by combining solver output with
    pre-resolved source archives. Consumers can run a plan directly, or
    serialise it to JSON and replay it elsewhere. *)

module Layer_hash = Layer_hash
module Archive = Archive
module Plan = Plan
module Config = Config
module Direct = Direct
module Registry = Registry
