open! Stdppx
open! Import

(** [convert mty sig_] returns the module type corresponding to
    {[
      mty [@with: sig_]
    ]}
    where [sig_] has already been monomorphized by ppx_template.

    Only some constructs have meaning in [sig_]
    - [include sig end] and [[%%template:]] are inlined
    - doc comments are preserved and attached to a best-effort relevant node within the
      final module type
    - type, module, and module type bindings (e.g. [type t = t] or [module type S := S])
      are converted to with constraints on [mty]
    - [module M : sig ... end] is used to construct contsraints with multi-part identifier
      paths, e.g.
      {[
        module M : sig
          type t := t
        end
      ]}
      produces a [with type M.t := t] constraint *)
val convert : module_type -> signature -> (module_type, Syntax_error.t) result
