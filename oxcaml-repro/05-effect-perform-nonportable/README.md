# Effect.perform is nonportable, blocking effect-based schedulers

`Stdlib.Effect` carries no mode annotations in `5.2.0+ox`, so any
function that performs an effect cannot be `portable`:

```
Error: The value "Effect.perform" is "nonportable"
       but is expected to be "portable"
```

Performing an effect is handled by the current domain's own handler
stack and shares nothing across domains, so a portable `perform` is
sound. The oxcaml development branch has the real design
(`Effect.Safe.perform` with a `Handler.t @ local` capability token and
the yielding axis); until that ships, annotating an effect-based
library requires re-declaring the primitive, as `workaround.ml` shows
and as the vendored Eio does in `lib_eio/core/peff.ml`.
