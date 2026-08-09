(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Migration from schema V1 to V2.

    The migration is pure so that it can be tested against a copy of a live
    store without touching it. It fails rather than accepting a value it
    does not recognise, such as an unknown service kind, so a store is
    never silently misread. *)

val v1_to_v2 :
  Sortal_schema_contact_v1.t -> (Sortal_schema_contact_v2.t, string) result
(** [v1_to_v2 c] is [c] in the V2 schema, or [Error why] naming the first
    value [v1_to_v2] does not recognise, or the first organization carrying
    a field V2 has nowhere to put.

    A [services] entry becomes an account. Its handle is taken from the
    entry, or derived from the URL tail, or is the URL itself when the URL
    is a bare handle. A [urls] entry whose host names a platform becomes an
    account with a handle derived from the URL, and every other entry
    becomes a link. The [orcid] field and the [atproto] block become
    accounts, [organizations] becomes affiliations, and [icon] and
    [thumbnail] collapse into [photo], [thumbnail] taking priority. An
    account that duplicates the platform and handle of one already produced
    is dropped, keeping the first.

    V2 has no field for some data V1 carries. None of it identifies an
    account or a person, so [v1_to_v2] drops it rather than erroring: a
    [service]'s [url], [label], [primary] and [range]; a promoted [urls]
    entry's [label] and [range], or a link's [range]; an [email]'s
    [type_], [range] and [note]; an [atproto_service]'s [atp_url]; and an
    [organization]'s [range], once its bounds move to the affiliation's
    [from] and [until]. The one exception is an [organization]'s [email]:
    V2 affiliations have no field for it, and unlike the fields above it
    identifies a person, so [v1_to_v2] refuses to drop it and returns
    [Error] instead, naming the contact and the organization. *)

val platform_of_host : string -> Sortal_schema_platform.id option
(** [platform_of_host h] is the platform served at host [h], or [None] if
    [h] is an ordinary web host. A [github.io] host is a personal site and
    is not GitHub. Any [scholar.google.<tld>] host is Google Scholar: a
    Scholar profile id is global, not tied to the domain it was found
    under. *)

val classify_url :
  string -> [ `Account of Sortal_schema_account.t | `Link ]
(** [classify_url u] is how [u] migrates: an account when its host names a
    platform and a handle can be derived from it, and a link otherwise. This
    is the same decision {!v1_to_v2} makes for a [urls] entry, exposed so a
    caller can report on it without reimplementing it. *)
