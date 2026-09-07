type precondition = Proceed | Revalidated | Failed

val[@zero_alloc] reject_conditional_write :
  has_now:bool -> float# -> Req.t @ local -> bool @@ portable
val[@zero_alloc] evaluate :
  has_now:bool -> float# -> Req.t @ local -> Resp.description @ local -> precondition
  @@ portable
