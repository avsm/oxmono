(** WebDAV XML tree types and xmlm codec.

    Provides a simple XML tree representation used throughout the WebDAV
    protocol for property values, request bodies, and multistatus responses.

    The tree type models the subset of XML used by WebDAV:
    elements with namespace-qualified names, attributes, and text content.
    No support for processing instructions, comments, or mixed content —
    these are not used in the WebDAV wire format.

    {2 Wire Format}

    WebDAV uses XML for request bodies (PROPFIND, PROPPATCH, LOCK, REPORT)
    and response bodies (207 Multi-Status). All elements are namespace-qualified;
    the primary namespace is ["DAV:"] defined in
    {{:https://datatracker.ietf.org/doc/html/rfc4918#section-14}RFC 4918 Section 14}.

    {2 Example}

    {[
      let body = Webdavz_xml.dav_node "propfind"
        [dav_node "prop" [dav_node "displayname" []; dav_node "resourcetype" []]]
      in
      Webdavz_xml.serialize body
      (* produces: <propfind xmlns="DAV:"><prop><displayname/><resourcetype/></prop></propfind> *)
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc4918> RFC 4918 — HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)
*)

(** {1 Types} *)

type fqname = string * string
(** Fully qualified XML name: [(namespace_uri, local_name)].

    Standard WebDAV properties use the ["DAV:"] namespace:
    {[("DAV:", "displayname")]}

    CardDAV properties use:
    {[("urn:ietf:params:xml:ns:carddav", "address-data")]}

    @see <https://datatracker.ietf.org/doc/html/rfc4918#appendix-A> RFC 4918 Appendix A — notes on XML namespaces *)

type attribute = fqname * string
(** XML attribute: [(qualified_name, value)]. *)

type tree =
  | Pcdata of string
      (** Character data (text content). *)
  | Node of string * string * attribute list * tree list
      (** [Node (namespace, local_name, attributes, children)].

          An XML element. Empty elements have [children = \[\]].
          Self-closing tags (e.g., [<displayname/>]) and explicitly
          closed empty tags are equivalent. *)
(** XML tree.

    Recursive representation of an XML document fragment.
    Sufficient for all WebDAV/CardDAV wire format needs.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14> RFC 4918 Section 14 — XML element definitions *)

(** {1 Namespaces} *)

val dav_ns : string
(** ["DAV:"] — the WebDAV namespace URI.

    All standard WebDAV elements and properties are in this namespace.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-18> RFC 4918 Section 18 — DAV compliance classes *)

val carddav_ns : string
(** ["urn:ietf:params:xml:ns:carddav"] — the CardDAV namespace URI.

    @see <https://datatracker.ietf.org/doc/html/rfc6352#section-11> RFC 6352 Section 11 — XML namespace *)

(** {1 Parsing and Serialization} *)

val parse : string -> tree option
(** [parse xml_string] parses an XML document into a tree.

    Returns the root element, or [None] on malformed input.
    Uses {{:https://erratique.ch/software/xmlm}xmlm} for standards-compliant
    XML 1.0 parsing with full namespace resolution.

    {b Note}: The entire document must be well-formed. Partial documents
    or documents with unresolved namespace prefixes will return [None]. *)

val serialize : tree -> string
(** [serialize tree] serializes a tree to an XML document string.

    Includes the XML declaration ([<?xml version="1.0" encoding="UTF-8"?>])
    as recommended by RFC 4918.

    {[
      serialize (dav_node "prop" [dav_node "displayname" [pcdata "My Files"]])
    ]} *)

val serialize_compact : tree -> string
(** [serialize_compact tree] serializes without XML declaration.
    Suitable for embedding or tests. *)

(** {1 Date Formatting} *)

val http_date_of_unix_time : float -> string
(** [http_date_of_unix_time t] formats a Unix timestamp as an HTTP-date
    (IMF-fixdate per RFC 7231 Section 7.1.1.1):
    ["Thu, 01 Jan 2015 00:00:00 GMT"].

    Used for the {!Webdavz_prop.getlastmodified} property.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.7> RFC 4918 Section 15.7 *)

val iso8601_of_unix_time : float -> string
(** [iso8601_of_unix_time t] formats a Unix timestamp as ISO 8601 / RFC 3339:
    ["2015-01-01T00:00:00Z"].

    Used for the {!Webdavz_prop.creationdate} property.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.1> RFC 4918 Section 15.1 *)

(** {1 Construction Helpers} *)

val error_xml : string -> tree
(** [error_xml element_name] creates a DAV:error precondition/postcondition
    XML body: [<D:error><D:element_name/></D:error>].

    Standard error element names include:
    - ["resource-must-be-null"] — MKCOL on existing resource
    - ["propfind-finite-depth"] — PROPFIND with Depth: infinity
    - ["cannot-modify-protected-property"] — PROPPATCH on live property

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-16> RFC 4918 Section 16 *)

val dav_node : string -> tree list -> tree
(** [dav_node local_name children] creates an element in the ["DAV:"] namespace.

    Equivalent to [Node (dav_ns, local_name, \[\], children)].

    {[
      dav_node "href" [pcdata "/calendars/user/"]
      (* = Node ("DAV:", "href", [], [Pcdata "/calendars/user/"]) *)
    ]} *)

val pcdata : string -> tree
(** [pcdata s] creates a text node. Equivalent to [Pcdata s]. *)

(** {1 Query Helpers} *)

val find_children : string -> string -> tree list -> tree list
(** [find_children ns local_name children] filters [children] to those
    matching the given namespace and local name.

    Returns only {!Node} elements; {!Pcdata} nodes are excluded. *)

val node_name : tree -> fqname option
(** [node_name tree] extracts the qualified name from a {!Node},
    or returns [None] for {!Pcdata}. *)
