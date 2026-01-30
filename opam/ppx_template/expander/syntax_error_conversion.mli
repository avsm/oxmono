open! Stdppx
open! Import

(** [to_extension_node ?also_drop ctx node err] produces an error extension node for
    context [ctx], handling marking the [node] as dropped for ppxlib. If sibling nodes to
    [node] are also going to be dropped, they can be passed via [~also_drop]. *)
val to_extension_node
  : 'a.
  ?also_drop:'a list -> 'a Attributes.Context.any -> 'a -> Syntax_error.t -> 'a

val to_extension_node_floating
  : 'a.
  'a Attributes.Floating.Context.poly -> loc:location -> Syntax_error.t -> 'a
