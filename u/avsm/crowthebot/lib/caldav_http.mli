(** Transport policy for Crow's calendar mirror. *)

val read_only : url:string -> _ Fetch.t -> Fetch.plain
(** [read_only ~url fetch] restricts [fetch] to the configured HTTPS origin. It
    permits body-free GET, PROPFIND at depth zero or one, and the bounded
    DAV:sync-collection report used by the mirror. Depth-one calendar-query
    reports must request VEVENT expansion for the same bounded interval of at
    most 31 days in their filter and calendar-data. Other methods, reports,
    request bodies, headers and URL queries are denied before transport. XML
    requests are limited to 64 KiB. The result has no backend-specific
    capability. This function accepts no credentials. *)
