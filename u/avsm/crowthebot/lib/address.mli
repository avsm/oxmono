(** Matrix addressing without display-name authority. *)

val mentions : self:string -> Jsont.json -> bool
(** [mentions ~self content] checks [m.mentions.user_ids] for [self]. *)

val body : reply:bool -> string -> string
(** [body ~reply text] removes a legacy reply fallback when [reply] is true. *)

val command :
  self:string -> mentioned:bool -> direct:bool -> string -> string option
(** [command ~self ~mentioned ~direct text] recognizes [!crow], exact account
    mentions and nonempty direct messages. A bare address requests help. *)

val direct_peer :
  ?admin:string ->
  self:string ->
  marked:bool ->
  complete:bool ->
  string list ->
  string option
(** [direct_peer ?admin ~self ~marked ~complete members] returns the other
    account when complete membership contains exactly two users, including
    [self], and the room is marked as a DM or the peer is [admin]. Include
    invited users in [members]. *)
