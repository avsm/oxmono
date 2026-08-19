type t =
  [ `Cites
  | `CitesAsAuthority
  | `CitesAsDataSource
  | `CitesAsEvidence
  | `CitesForInformation
  | `UsesDataFrom
  | `UsesMethodIn
  | `UsesConclusionsFrom
  | `AgreesWith
  | `DisagreesWith
  | `Confirms
  | `Refutes
  | `Disputes
  | `Critiques
  | `Qualifies
  | `Corrects
  | `Updates
  | `Extends
  | `Parodies
  | `Plagiarizes
  | `Derides
  | `Ridicules
  | `Describes
  | `Documents
  | `CitesAsSourceDocument
  | `CitesAsMetadataDocument
  | `Compiles
  | `Reviews
  | `Retracts
  | `Supports
  | `GivesSupportTo
  | `ObtainsSupportFrom
  | `GivesBackgroundTo
  | `ObtainsBackgroundFrom
  | `SpeculatesOn
  | `CitesAsPotentialSolution
  | `CitesAsRecommendedReading
  | `CitesAsRelated
  | `IncludesQuotationFrom
  | `IncludesExcerptFrom
  | `RepliesTo
  | `HasReplyFrom
  | `LinksTo
  | `SharesAuthorWith
  | `SharesJournalWith
  | `SharesPublicationVenueWith
  | `SharesFundingAgencyWith
  | `SharesAuthorInstitutionWith
  | `Other of string ]

let to_string = function
  | `Cites -> "cites"
  | `CitesAsAuthority -> "citesAsAuthority"
  | `CitesAsDataSource -> "citesAsDataSource"
  | `CitesAsEvidence -> "citesAsEvidence"
  | `CitesForInformation -> "citesForInformation"
  | `UsesDataFrom -> "usesDataFrom"
  | `UsesMethodIn -> "usesMethodIn"
  | `UsesConclusionsFrom -> "usesConclusionsFrom"
  | `AgreesWith -> "agreesWith"
  | `DisagreesWith -> "disagreesWith"
  | `Confirms -> "confirms"
  | `Refutes -> "refutes"
  | `Disputes -> "disputes"
  | `Critiques -> "critiques"
  | `Qualifies -> "qualifies"
  | `Corrects -> "corrects"
  | `Updates -> "updates"
  | `Extends -> "extends"
  | `Parodies -> "parodies"
  | `Plagiarizes -> "plagiarizes"
  | `Derides -> "derides"
  | `Ridicules -> "ridicules"
  | `Describes -> "describes"
  | `Documents -> "documents"
  | `CitesAsSourceDocument -> "citesAsSourceDocument"
  | `CitesAsMetadataDocument -> "citesAsMetadataDocument"
  | `Compiles -> "compiles"
  | `Reviews -> "reviews"
  | `Retracts -> "retracts"
  | `Supports -> "supports"
  | `GivesSupportTo -> "givesSupportTo"
  | `ObtainsSupportFrom -> "obtainsSupportFrom"
  | `GivesBackgroundTo -> "givesBackgroundTo"
  | `ObtainsBackgroundFrom -> "obtainsBackgroundFrom"
  | `SpeculatesOn -> "speculatesOn"
  | `CitesAsPotentialSolution -> "citesAsPotentialSolution"
  | `CitesAsRecommendedReading -> "citesAsRecommendedReading"
  | `CitesAsRelated -> "citesAsRelated"
  | `IncludesQuotationFrom -> "includesQuotationFrom"
  | `IncludesExcerptFrom -> "includesExcerptFrom"
  | `RepliesTo -> "repliesTo"
  | `HasReplyFrom -> "hasReplyFrom"
  | `LinksTo -> "linksTo"
  | `SharesAuthorWith -> "sharesAuthorWith"
  | `SharesJournalWith -> "sharesJournalWith"
  | `SharesPublicationVenueWith -> "sharesPublicationVenueWith"
  | `SharesFundingAgencyWith -> "sharesFundingAgencyWith"
  | `SharesAuthorInstitutionWith -> "sharesAuthorInstitutionWith"
  | `Other s -> s

let of_string s =
  match String.lowercase_ascii s with
  | "cites" -> `Cites
  | "citesasauthority" -> `CitesAsAuthority
  | "citesasdatasource" -> `CitesAsDataSource
  | "citesasevidence" -> `CitesAsEvidence
  | "citesforinformation" -> `CitesForInformation
  | "usesdatafrom" -> `UsesDataFrom
  | "usesmethodin" -> `UsesMethodIn
  | "usesconclusionsfrom" -> `UsesConclusionsFrom
  | "agreeswith" -> `AgreesWith
  | "disagreeswith" -> `DisagreesWith
  | "confirms" -> `Confirms
  | "refutes" -> `Refutes
  | "disputes" -> `Disputes
  | "critiques" -> `Critiques
  | "qualifies" -> `Qualifies
  | "corrects" -> `Corrects
  | "updates" -> `Updates
  | "extends" -> `Extends
  | "parodies" -> `Parodies
  | "plagiarizes" -> `Plagiarizes
  | "derides" -> `Derides
  | "ridicules" -> `Ridicules
  | "describes" -> `Describes
  | "documents" -> `Documents
  | "citesassourcedocument" -> `CitesAsSourceDocument
  | "citesasmetadatadocument" -> `CitesAsMetadataDocument
  | "compiles" -> `Compiles
  | "reviews" -> `Reviews
  | "retracts" -> `Retracts
  | "supports" -> `Supports
  | "givessupportto" -> `GivesSupportTo
  | "obtainssupportfrom" -> `ObtainsSupportFrom
  | "givesbackgroundto" -> `GivesBackgroundTo
  | "obtainsbackgroundfrom" -> `ObtainsBackgroundFrom
  | "speculateson" -> `SpeculatesOn
  | "citesaspotentialsolution" -> `CitesAsPotentialSolution
  | "citesasrecommendedreading" -> `CitesAsRecommendedReading
  | "citesasrelated" -> `CitesAsRelated
  | "includesquotationfrom" -> `IncludesQuotationFrom
  | "includesexcerptfrom" -> `IncludesExcerptFrom
  | "repliesto" -> `RepliesTo
  | "hasreplyfrom" -> `HasReplyFrom
  | "linksto" -> `LinksTo
  | "sharesauthorwith" -> `SharesAuthorWith
  | "sharesjournalwith" -> `SharesJournalWith
  | "sharespublicationvenuewith" -> `SharesPublicationVenueWith
  | "sharesfundingagencywith" -> `SharesFundingAgencyWith
  | "sharesauthorinstitutionwith" -> `SharesAuthorInstitutionWith
  | _ -> `Other s

let equal a b = match (a, b) with `Other sa, `Other sb -> sa = sb | _ -> a = b
let pp ppf t = Format.fprintf ppf "%s" (to_string t)

let jsont =
  let kind = "CiTO intent" in
  let doc = "A Citation Typing Ontology intent annotation" in
  Jsont.map ~kind ~doc ~dec:of_string ~enc:to_string Jsont.string
