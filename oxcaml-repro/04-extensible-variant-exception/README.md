# Exceptions with extensible payloads cannot be matched in portable code

Extension constructors require every payload type to cross portability,
and an extensible type can never cross, so an exception like Eio's
`exception Io of err * context` with `type err = ..` cannot be matched
or constructed in any portable function:

```
Error: This value is "nonportable" but is expected to be "portable".
  Hint: All arguments of the constructor "Io"
  must cross this axis to use it in this position.
```

`control.ml` shows a string-payload exception matching fine, and
`workaround.ml` the only current escape: define the matcher at legacy
mode and assert it with `Obj.magic_portable`. This may be by design,
but it makes the common error-context idiom (wrap, match, re-raise with
context) unannotatable without unsafe casts. Every error-decorating
function in Eio needed one.
