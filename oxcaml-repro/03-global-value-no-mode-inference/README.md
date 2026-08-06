# Constants of abstract types are unusable from portable code

`lib.mli` exports an abstract `type t` (an `int` underneath), a
constant `v` and a portable `get`. A portable function may call
`Lib.get` but not touch `Lib.v`:

```
Error: This value is "contended"
         because it is used inside the function at file "consumer.ml" ...
         which is expected to be "portable".
       However, the highlighted expression is expected to be "uncontended".
```

`fixed.mli` declares `type t : immediate` and everything works, but only
the defining library can do that. There is no consumer-side way to
declare or assert a kind for an abstract type from another library, so
constants such as `Mtime.Span.zero`, `Hmap.empty` or registered
`Runtime_events.User.t` descriptors wall off portable code until each
upstream library is edited. A consumer-side kind assertion (or a
`Basement.Portability_hacks`-style proof for contention) would unblock
annotation work on the ecosystem.
