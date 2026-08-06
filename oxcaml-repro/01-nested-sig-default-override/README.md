# Nested-signature items cannot override a modality default

`repro.mli` sets a file-level `@@ portable` default and tries to exempt
one item of a nested module with `@@ nonportable`. The override parses
but is ignored: the compiler still demands a portable `M.pp`.

```
Error: The implementation "repro.ml" does not match the interface "repro.cmi":
       In module "M":
       Values do not match:
         val pp : Format.formatter -> int -> unit (* in a structure at nonportable *)
       is not included in
         val pp : Format.formatter -> int -> unit @@ portable
```

Note the expected declaration is printed as `@@ portable` although the
source says `@@ nonportable`. Giving the nested signature its own
`sig @@ ...` default does not help either; the outer default always
wins. `control.mli` shows the same override working at the scope of the
default itself.

Practical impact: an mli using a file default cannot exempt printers or
registries inside nested modules, and must fall back to item-by-item
annotation (this hit `Eio.Net`, `Eio.Time` and `Eio.Exn`).
