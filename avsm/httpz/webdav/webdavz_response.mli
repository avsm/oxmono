(** WebDAV 207 Multi-Status response generation.

    The 207 Multi-Status response conveys per-resource status information
    for operations that affect multiple resources (PROPFIND, PROPPATCH,
    COPY, MOVE, DELETE on collections).

    {2 Wire Format}

    A multistatus response contains [<response>] elements, each with
    an [<href>] and one or more [<propstat>] elements grouping properties
    by their HTTP status code:

    {v
      <multistatus xmlns="DAV:">
        <response>
          <href>/collection/file.txt</href>
          <propstat>
            <prop>
              <displayname>file.txt</displayname>
              <getcontentlength>1234</getcontentlength>
            </prop>
            <status>HTTP/1.1 200 OK</status>
          </propstat>
          <propstat>
            <prop><getetag/></prop>
            <status>HTTP/1.1 404 Not Found</status>
          </propstat>
        </response>
      </multistatus>
    v}

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-13> RFC 4918 Section 13 — multi-status response
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14.16> RFC 4918 Section 14.16 — multistatus XML element *)

(** {1 Types} *)

type propstat = {
  status : Httpz.Res.status;
  props : Webdavz_xml.tree list;
}
(** A property-status grouping: all [props] share the same HTTP [status].

    Typically a PROPFIND response has one 200 propstat for found properties
    and one 404 propstat for requested-but-missing properties.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14.22> RFC 4918 Section 14.22 — propstat *)

type response = {
  href : string;
  propstats : propstat list;
}
(** A per-resource response within a multistatus.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14.24> RFC 4918 Section 14.24 — response *)

(** {1 Generation} *)

val multistatus : response list -> string
(** [multistatus responses] serializes a complete 207 Multi-Status XML body.

    The result is ready to be sent as the response body with
    [Content-Type: application/xml; charset=utf-8].

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14.16> RFC 4918 Section 14.16 *)

(** {1 Construction Helpers} *)

val propstat_ok : Webdavz_xml.tree list -> propstat
(** [propstat_ok props] groups [props] under HTTP 200 OK. *)

val propstat_not_found : Webdavz_xml.tree list -> propstat
(** [propstat_not_found props] groups [props] under HTTP 404 Not Found.
    Used when a client requests properties that don't exist on the resource. *)

val prop_node : Webdavz_xml.fqname -> Webdavz_xml.tree list -> Webdavz_xml.tree
(** [prop_node (ns, name) values] wraps property values in their element.

    {[prop_node ("DAV:", "displayname") [pcdata "My Folder"]]}
    produces [<displayname>My Folder</displayname>]. *)

val empty_prop_node : Webdavz_xml.fqname -> Webdavz_xml.tree
(** [empty_prop_node (ns, name)] creates an empty property element.

    Used in {!Propname} responses (listing names without values) and
    in 404 propstats (indicating which properties were not found). *)
