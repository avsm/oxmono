(** CardDAV server library for httpz.

    Provides vCard 4.0 parsing, file-backed address book storage,
    CardDAV REPORT handling, and httpz route integration.

    {{:https://datatracker.ietf.org/doc/html/rfc6352}RFC 6352}
    {{:https://datatracker.ietf.org/doc/html/rfc6350}RFC 6350} *)

module Vcard = Carddavz_vcard
module Store = Carddavz_store
module Report = Carddavz_report
module Routes = Carddavz_routes
