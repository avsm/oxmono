## Release v0.17.0
- Stack allocate the `~default` argument to `Lru_cache.S.find_or_add` by annotating it with
  `local_`.
  This makes the interface consistent with that of `Hashtbl`.
