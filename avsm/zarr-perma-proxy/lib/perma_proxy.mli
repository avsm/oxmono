(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP proxy for immutable Zarr stores and other public objects. *)
module Manifest = Manifest
module Cache = Cache
module Coverage = Coverage
module Shard = Shard

type mapping
type t
val mapping : prefix:string -> upstream:string -> mapping
(** [mapping ~prefix ~upstream] maps a local absolute path to an HTTP store
    root. Neither root may have a trailing slash. A prefix of ["/"] is allowed.
    Invalid mappings raise [Invalid_argument]. *)
val create : cache:Cache.t -> mapping list -> t
(** [create ~cache mappings] creates a proxy. The longest matching prefix
    wins. At least one mapping is required. *)
val site : t Proffer.Site.t
(** [site] serves GET, HEAD and CORS OPTIONS. A single explicit, open-ended
    or suffix byte range is supported. Multi-range requests return 400.
    Successful cache hits require no upstream request, including HEAD.

    Cached array or consolidated metadata identifies indexed Zarr shards.
    Missing ranges read and validate their index, then cache complete encoded
    inner chunks overlapping the request. Unsupported layouts retain ordinary
    byte-range caching. Whole reads fetch gaps and assemble the original shard
    without rewriting its index. Full objects stream from disk.

    Hits are permanent. Use immutable dataset URLs and stop the server before
    clearing its cache to refresh a changed publication. No upstream
    credentials, cookies or client authorization headers are forwarded. *)
