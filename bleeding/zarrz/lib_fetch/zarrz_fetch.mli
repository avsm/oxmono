(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Read only HTTP store for Zarr hierarchies.

    A {!Zarrz.Store.t} whose objects are fetched over HTTP through a
    {!Fetch.t} capability. The store reads. [set], [erase] and [list]
    are [None], so an attempt to write through it raises.

    Two failure modes reach a caller, and they are distinct on purpose.
    Transport, protocol and policy failures propagate exactly as
    {!Fetch} raises them, as [Eio.Io (Fetch.E _, _)], so a caller that
    already handles [fetch] errors keeps handling them and loses no
    context. An HTTP exchange that completes but answers something the
    store cannot use, such as a 500 or a 416, raises
    {!Zarrz.Error.E} with a {!Zarrz.Error.Store} payload naming the
    method, the URL and the status. *)

val store : ?ranged:bool -> base_url:string -> _ Fetch.t -> Zarrz.Store.t
(** [store ~base_url client] is a store serving the key [k] from the URL
    [base_url ^ "/" ^ k]. Store keys are already restricted to the
    characters a path segment admits, so no escaping is applied.

    [ranged] (default [true]) is the store's {!Zarrz.Store.ranged}
    field, which tells the core that a ranged read is cheaper than a
    whole one. Pass [false] for an origin that ignores [Range], which
    makes the core fetch whole chunks instead of paying for a range
    request that the origin answers with the whole object anyway.

    The operations behave as follows.

    - [get] issues [GET]. 200 buffers the body, 404 and 410 are [None].
    - [get_range] issues [GET] with a [Range] header, one byte range.
      206 returns the body as sent. 200 means the origin ignored the
      range, so the whole body is buffered and sliced locally with the
      truncation {!Zarrz.Byte_range.resolve} applies. 404 and 410 are
      [None]. A range of zero bytes is answered from nothing, with no
      request at all.
    - [get_ranges] runs its ranges as concurrent fibers, at most six in
      flight. Any range answering 404 or 410 means the object went away
      mid read, so the whole call is [None]. An empty range list is
      [Some []] and makes no request.
    - [size] issues [HEAD] and reports [Content-Length]. It is [None]
      when the header is absent as well as when the object is, which is
      the ambiguity {!Zarrz.Store.size} documents.

    A response body is read straight into the result buffer through
    [Cstruct] views, so no intermediate string is built. When the
    response declares a [Content-Length] the buffer is allocated once at
    that size and a body that ends early is an error. Otherwise the
    buffer grows by doubling.

    @raise Invalid_argument if [base_url] is empty or ends in ["/"].
    @raise Zarrz.Error.E [(Store _)] on any other status, including 416
      from a range the origin declares unsatisfiable, and on a body that
      is shorter than the [Content-Length] it declared. *)
