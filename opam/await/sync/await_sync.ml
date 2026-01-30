(** Basic synchronization primitives using [Await]. *)

include Await_sync_intf
module Atom = Atom
module Awaitable = Awaitable
module Barrier = Barrier
module Countdown_latch = Countdown_latch
module Ivar = Ivar
module Mutex = Mutex
module Mvar = Mvar
module Rwlock = Rwlock
module Scope = Scope
module Semaphore = Semaphore
module Stack = Stack
