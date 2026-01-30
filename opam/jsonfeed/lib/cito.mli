(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Citation Typing Ontology (CiTO) intent annotations.

    CiTO provides a structured vocabulary for describing the nature of
    citations. This module implements support for CiTO annotations as used in
    the references extension.

    @see <https://purl.archive.org/spar/cito> Citation Typing Ontology
    @see <https://sparontologies.github.io/cito/current/cito.html>
      CiTO Specification *)

type t =
  [ `Cites  (** The base citation property *)
  | (* Factual citation intents *)
    `CitesAsAuthority
    (** Cites as authoritative source *)
  | `CitesAsDataSource  (** Cites as origin of data *)
  | `CitesAsEvidence  (** Cites for factual evidence *)
  | `CitesForInformation  (** Cites as information source *)
  | `UsesDataFrom  (** Uses data from cited work *)
  | `UsesMethodIn  (** Uses methodology from cited work *)
  | `UsesConclusionsFrom  (** Applies conclusions from cited work *)
  | (* Agreement/disagreement *)
    `AgreesWith
    (** Concurs with cited statements *)
  | `DisagreesWith  (** Rejects cited statements *)
  | `Confirms  (** Validates facts in cited work *)
  | `Refutes  (** Disproves cited statements *)
  | `Disputes  (** Contests without definitive refutation *)
  | (* Critical engagement *)
    `Critiques
    (** Analyzes and finds fault *)
  | `Qualifies  (** Places conditions on statements *)
  | `Corrects  (** Fixes errors in cited work *)
  | `Updates  (** Advances understanding beyond cited work *)
  | `Extends  (** Builds upon cited facts *)
  | (* Rhetorical/stylistic *)
    `Parodies
    (** Imitates for comic effect *)
  | `Plagiarizes  (** Uses without acknowledgment *)
  | `Derides  (** Expresses contempt *)
  | `Ridicules  (** Mocks cited work *)
  | (* Document relationships *)
    `Describes
    (** Characterizes cited entity *)
  | `Documents  (** Records information about source *)
  | `CitesAsSourceDocument  (** Cites as foundational source *)
  | `CitesAsMetadataDocument  (** Cites containing metadata *)
  | `Compiles  (** Uses to create new work *)
  | `Reviews  (** Examines cited statements *)
  | `Retracts  (** Formally withdraws *)
  | (* Support/context *)
    `Supports
    (** Provides intellectual backing *)
  | `GivesSupportTo  (** Provides support to citing entity *)
  | `ObtainsSupportFrom  (** Obtains backing from cited work *)
  | `GivesBackgroundTo  (** Provides context *)
  | `ObtainsBackgroundFrom  (** Obtains context from cited work *)
  | (* Exploratory *)
    `SpeculatesOn
    (** Theorizes without firm evidence *)
  | `CitesAsPotentialSolution  (** Offers possible resolution *)
  | `CitesAsRecommendedReading  (** Suggests as further reading *)
  | `CitesAsRelated  (** Identifies as thematically connected *)
  | (* Quotation/excerpting *)
    `IncludesQuotationFrom
    (** Incorporates direct quotes *)
  | `IncludesExcerptFrom  (** Uses non-quoted passages *)
  | (* Dialogue *)
    `RepliesTo
    (** Responds to cited statements *)
  | `HasReplyFrom  (** Evokes response *)
  | (* Linking *)
    `LinksTo
    (** Provides URL hyperlink *)
  | (* Shared attribution *)
    `SharesAuthorWith
    (** Common authorship *)
  | `SharesJournalWith  (** Published in same journal *)
  | `SharesPublicationVenueWith  (** Published in same venue *)
  | `SharesFundingAgencyWith  (** Funded by same agency *)
  | `SharesAuthorInstitutionWith  (** Authors share affiliation *)
  | (* Extensibility *)
    `Other of string
    (** Custom or future CiTO term *) ]
(** CiTO citation intent annotation.

    Represents the intent or nature of a citation using the Citation Typing
    Ontology. Each variant corresponds to a specific CiTO property. The [`Other]
    variant allows for custom or future CiTO terms not yet included in this
    library.

    {b Categories:}
    - Factual: Citing for data, methods, evidence, or information
    - Critical: Agreement, disagreement, correction, or qualification
    - Rhetorical: Style-based citations (parody, ridicule, etc.)
    - Relational: Document relationships and compilations
    - Support: Providing or obtaining backing and context
    - Exploratory: Speculation and recommendations
    - Quotation: Direct quotes and excerpts
    - Dialogue: Replies and responses
    - Sharing: Common attributes between works *)

(** {1 Conversion} *)

val of_string : string -> t
(** [of_string s] converts a CiTO term string to its variant representation.

    Recognized CiTO terms are converted to their corresponding variants.
    Unrecognized terms are wrapped in [`Other].

    The comparison is case-insensitive for standard CiTO terms but preserves the
    original case in [`Other] variants.

    {b Examples:}
    {[
      of_string "cites" (* returns `Cites *) of_string "usesMethodIn"
        (* returns `UsesMethodIn *) of_string
        "citesAsRecommendedReading" (* returns `CitesAsRecommendedReading *)
        of_string "customTerm" (* returns `Other "customTerm" *)
    ]} *)

val to_string : t -> string
(** [to_string t] converts a CiTO variant to its canonical string
    representation.

    Standard CiTO terms use their official CiTO local names (camelCase).
    [`Other] variants return the wrapped string unchanged.

    {b Examples:}
    {[
      to_string `Cites (* returns "cites" *) to_string `UsesMethodIn
        (* returns "usesMethodIn" *) to_string (`Other "customTerm")
      (* returns "customTerm" *)
    ]} *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two CiTO annotations.

    Two annotations are equal if they represent the same CiTO term. For [`Other]
    variants, string comparison is case-sensitive. *)

(** {1 Jsont Type} *)

val jsont : t Jsont.t
(** Declarative JSON type for CiTO annotations.

    Maps CiTO intent strings to the corresponding variants. Unknown intents are
    mapped to [`Other s]. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty prints a CiTO annotation to the formatter.

    {b Example output:}
    {v citesAsRecommendedReading v} *)
