(** ES256K service authentication against an explicitly configured PLC. *)

exception Rejected
(** [Rejected] means the token or resolved verification key is invalid. *)

val authenticate :
  read:(string -> string) -> plc:string -> actor:string -> audience:string ->
  meth:string -> now:float -> string -> string
(** [authenticate ~read ~plc ~actor ~audience ~meth ~now token] is [actor]
    after checking the JWT signature, issuer, audience, method and lifetime.
    [read] must bound response size and time and restrict requests to [plc],
    the trusted directory origin. It is called for each verification so key
    rotation takes effect without restarting. Transport errors propagate.
    Tokens may expire at most one hour after [now]. *)
