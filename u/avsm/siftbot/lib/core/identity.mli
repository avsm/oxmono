(** Read-only Sortal resolution with historical affiliations. *)

type person = { uid : string; contact : Sortal.Contact.t }
type match_ = { uid : string; name : string; affiliations : string list }

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> root:string -> person list

val resolve :
  person list ->
  aliases:(string * string) list ->
  address:string ->
  date:Ptime.date ->
  match_ list
(** [resolve people ~aliases ~address ~date] returns all exact candidates.
    Reviewed address-to-UID aliases take precedence. An empty result means
    unknown. Multiple candidates retain ambiguity. Affiliations use inclusive
    start and exclusive end dates. *)

val duplicates :
  Siftbot_model.message list ->
  (Siftbot_model.source * Siftbot_model.source) list
(** [duplicates messages] proposes relationships from Message-ID and identical
    sender/body evidence. It never merges or removes the original sources. *)
