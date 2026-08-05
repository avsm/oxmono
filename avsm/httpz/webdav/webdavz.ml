(** WebDAV protocol library for httpz.

    Implements the core WebDAV data model and request/response handling
    from {{:https://datatracker.ietf.org/doc/html/rfc4918}RFC 4918}.

    {2 Module Structure}

    - {!Xml} — XML tree types and xmlm-based codec
    - {!Prop} — Well-known DAV: property names and accessors
    - {!Request} — PROPFIND / PROPPATCH request body parsing
    - {!Response} — 207 Multi-Status response generation
    - {!Handler} — Generic [STORE]-based WebDAV route generation

    {2 Quick Start}

    {[
      (* 1. Implement STORE for your backend *)
      module My_store : Webdavz.STORE = struct ... end

      (* 2. Generate routes *)
      let routes = Webdavz.Handler.routes (module My_store) my_store

      (* 3. Serve with httpz_eio *)
      let route_table = Httpz_server.Route.of_list routes
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc4918> RFC 4918 — WebDAV *)

module Xml = Webdavz_xml
module Prop = Webdavz_prop
module Request = Webdavz_request
module Response = Webdavz_response
module Handler = Webdavz_handler
module Lock = Webdavz_lock

module type RO_STORE = Webdavz_handler.RO_STORE
module type STORE = Webdavz_handler.STORE
