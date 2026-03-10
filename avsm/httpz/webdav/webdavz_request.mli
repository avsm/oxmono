(** WebDAV request body parsing.

    Parses PROPFIND and PROPPATCH XML request bodies as defined in
    {{:https://datatracker.ietf.org/doc/html/rfc4918}RFC 4918}.

    {2 PROPFIND Request Bodies}

    A PROPFIND body specifies which properties to retrieve
    ({{:https://datatracker.ietf.org/doc/html/rfc4918#section-9.1}Section 9.1}):

    - Empty body or [<allprop/>]: retrieve all properties
    - [<propname/>]: retrieve just property names (no values)
    - [<prop>] with child elements: retrieve specific properties

    {2 PROPPATCH Request Bodies}

    A PROPPATCH body contains [<set>] and [<remove>] instructions
    ({{:https://datatracker.ietf.org/doc/html/rfc4918#section-9.2}Section 9.2}).

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9> RFC 4918 Section 9 — HTTP methods for distributed authoring *)

(** {1 PROPFIND}

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.1> RFC 4918 Section 9.1 *)

type propfind =
  | Allprop of Webdavz_xml.fqname list
      (** Retrieve all properties. The name list contains additional
          properties requested via [<include>]
          ({{:https://datatracker.ietf.org/doc/html/rfc4918#section-14.2}Section 14.2}). *)
  | Propname
      (** Retrieve property names only (no values). *)
  | Props of Webdavz_xml.fqname list
      (** Retrieve the listed properties. Properties not found on a
          resource appear in a 404 propstat. *)
(** PROPFIND request variants. *)

val parse_propfind : string -> propfind option
(** [parse_propfind xml] parses a PROPFIND request body.
    Returns [None] on malformed XML. *)

val propfind_of_body : string option -> propfind
(** [propfind_of_body body_opt] parses a PROPFIND body, defaulting to
    [Allprop \[\]] for [None] or empty string per
    {{:https://datatracker.ietf.org/doc/html/rfc4918#section-9.1}Section 9.1}:
    "A client may choose not to submit a request body. An empty PROPFIND
    request body MUST be treated as if it were an 'allprop' request." *)

(** {1 PROPPATCH}

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.2> RFC 4918 Section 9.2 *)

type propupdate =
  | Set of Webdavz_xml.fqname * Webdavz_xml.tree list
      (** Set a property to the given value. *)
  | Remove of Webdavz_xml.fqname
      (** Remove a property. *)
(** A single property update instruction. *)

val parse_proppatch : string -> propupdate list option
(** [parse_proppatch xml] parses a PROPPATCH [<propertyupdate>] body.
    Returns a list of update instructions in document order, or [None]
    on malformed XML.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-14.19> RFC 4918 Section 14.19 — propertyupdate *)

(** {1 Depth Header}

    The Depth header controls recursion for collection operations.
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-10.2> RFC 4918 Section 10.2 *)

type depth =
  | Zero
      (** Apply to the resource only. *)
  | One
      (** Apply to the resource and its immediate children. *)
  | Infinity
      (** Apply to the resource and all descendants. *)
(** PROPFIND/COPY/MOVE depth.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-10.2> RFC 4918 Section 10.2 — Depth header *)

val parse_depth : string option -> (depth, [> `Bad_request]) result
(** [parse_depth header_value] parses a Depth header value.

    - [None] defaults to {!Infinity} per
      {{:https://datatracker.ietf.org/doc/html/rfc4918#section-9.1}Section 9.1}
    - ["0"] maps to {!Zero}
    - ["1"] maps to {!One}
    - ["infinity"] maps to {!Infinity}
    - Any other value returns [Error `Bad_request] *)
