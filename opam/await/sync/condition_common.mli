@@ portable

open Await_kernel

type 'k t : value mod contended portable

val create : ?padded:bool @ local -> unit -> 'k t

val wait
  :  acquire:(Await.t @ local -> 'lock @ local -> unit) @ once portable
  -> release:('lock @ local -> unit) @ once portable
  -> Await.t @ local
  -> 'k t @ local
  -> lock:'lock @ local
  -> 'k Capsule.Expert.Key.t @ unique
  -> 'k Capsule.Expert.Key.t @ unique

val signal : 'k t @ local -> unit
val broadcast : 'k t @ local -> unit
