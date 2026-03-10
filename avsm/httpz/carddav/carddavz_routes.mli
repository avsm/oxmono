(** CardDAV httpz route integration.

    Generates httpz routes for a CardDAV address book server
    with file-backed storage.

    {{:https://datatracker.ietf.org/doc/html/rfc6352}RFC 6352} *)

val routes : store:Carddavz_store.t -> locks:Webdavz.Lock.t -> Httpz_server.Route.route list
(** [routes ~store] generates CardDAV routes including:
    - PROPFIND, PROPPATCH (via webdavz)
    - GET, PUT, DELETE for vCards
    - REPORT (addressbook-query, addressbook-multiget)
    - OPTIONS (advertises [DAV: 1, addressbook]) *)
