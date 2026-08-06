# [@@unsafe_allow_any_mode_crossing] is inert without an explicit kind

Three pieces of evidence, and the fix:

- `repro.ml`: a declaration carrying only the attribute does not match
  an abstract `type t : value mod portable contended` in its interface.
- `use_sites.ml`: the attribute alone does not let a value of the type
  be used from a portable function either. The attribute has no
  observable effect in either direction.
- `kind_only.ml`: the kind annotation alone is rejected, showing the
  attribute is what authorises the otherwise unprovable kind.
- `fix.ml`: attribute plus explicit kind works.

```
Error: The implementation "repro.ml" does not match the interface "repro.cmi":
       Type declarations do not match:
         type t : value non_float = { g : (int -> int) array; }
       [@@unsafe_allow_any_mode_crossing]
       is not included in
         type t : value mod portable contended
```

Either the attribute should imply the crossing (and the docs say which
kind), or an attribute that has no effect should be an error. Silence
here cost real debugging time.
