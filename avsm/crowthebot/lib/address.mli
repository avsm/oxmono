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
  self:string -> marked:bool -> complete:bool -> string list -> string option
(** [direct_peer ~self ~marked ~complete members] returns the other account only
    for a marked DM with complete membership containing exactly two users,
    including [self]. Include invited users in [members]. *)
