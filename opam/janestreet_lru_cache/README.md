Lru_cache
=========

`Lru_cache` provides caching with least-recently-used eviction policy.

`Lru_cache` has seen many years of production use in Iron.  We intend
to make it the preferred LRU cache implementation.

Related functionality (as of 2019-05):

- `Core.Memo.lru`: Its interface is written for transparent
  memoization of functions and thus exposes no access to the cache
  itself.

- `Memo_cache`: Uses a re-implementation of `Hash_queue`. Cannot
  change cache capacity after creation.  Its README suggests using
  `Core.Memo` instead.

