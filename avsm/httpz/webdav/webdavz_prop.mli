(** WebDAV property names and accessors.

    WebDAV resources have properties identified by XML qualified names
    (namespace URI + local name). This module defines the well-known
    property names from
    {{:https://datatracker.ietf.org/doc/html/rfc4918#section-15}RFC 4918 Section 15}
    and provides accessors for property sets.

    {2 Dead vs Live Properties}

    - {b Live properties}: maintained by the server (e.g., {!getcontentlength},
      {!getetag}). The server computes these from the resource state.
    - {b Dead properties}: stored and returned verbatim. Clients can set
      arbitrary dead properties via PROPPATCH.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15> RFC 4918 Section 15 — DAV properties *)

(** {1 Types} *)

type t = (Webdavz_xml.fqname * Webdavz_xml.tree list) list
(** A property set: association list of [(qualified_name, value_elements)].

    The value is a list of child XML trees — an empty list represents
    an element with no content (e.g., [<resourcetype/>]). *)

(** {1 Well-known DAV: Properties}

    These are the standard property names defined in RFC 4918 Section 15.
    All are in the ["DAV:"] namespace. *)

val resourcetype : Webdavz_xml.fqname
(** [("DAV:", "resourcetype")] — specifies the nature of the resource.

    Collections contain a [<DAV:collection/>] child element.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.9> RFC 4918 Section 15.9 *)

val displayname : Webdavz_xml.fqname
(** [("DAV:", "displayname")] — human-readable name for the resource.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.2> RFC 4918 Section 15.2 *)

val getcontenttype : Webdavz_xml.fqname
(** [("DAV:", "getcontenttype")] — MIME type of the resource content.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.5> RFC 4918 Section 15.5 *)

val getcontentlength : Webdavz_xml.fqname
(** [("DAV:", "getcontentlength")] — content length in bytes.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.4> RFC 4918 Section 15.4 *)

val getetag : Webdavz_xml.fqname
(** [("DAV:", "getetag")] — entity tag for the resource.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.6> RFC 4918 Section 15.6 *)

val getlastmodified : Webdavz_xml.fqname
(** [("DAV:", "getlastmodified")] — last modification date (RFC 1123 format).
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.7> RFC 4918 Section 15.7 *)

val creationdate : Webdavz_xml.fqname
(** [("DAV:", "creationdate")] — creation date (ISO 8601 format).
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.1> RFC 4918 Section 15.1 *)

val supportedlock : Webdavz_xml.fqname
(** [("DAV:", "supportedlock")] — lock capabilities.

    An empty element signals no lock support (class 1 compliance).
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.10> RFC 4918 Section 15.10 *)

val lockdiscovery : Webdavz_xml.fqname
(** [("DAV:", "lockdiscovery")] — active locks on the resource.

    An empty element signals no active locks.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.8> RFC 4918 Section 15.8 *)

(** {1 Accessors} *)

val find : Webdavz_xml.fqname -> t -> Webdavz_xml.tree list option
(** [find name props] returns the value trees for [name], or [None]
    if the property is not present. *)

val is_collection : t -> bool
(** [is_collection props] returns [true] if the {!resourcetype} property
    contains a [<DAV:collection/>] child element.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.9> RFC 4918 Section 15.9 *)
