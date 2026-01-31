module List = Stdlib.ListLabels

module Poly = struct
  let equal = ( = )
  let compare = compare
end

module Phys_equal = struct
  let equal = ( == )
end

let ( = ) = Int.equal
let ( == ) = [ `Use_phys_equal ]
let ( < ) x y = Int.compare x y = -1
let ( > ) x y = Int.compare x y = 1
let min = Int.min
let max = Int.max
let compare = Int.compare

module Int = struct
  let[@warning "-32"] hash (x : int) = Hashtbl.hash x

  include Stdlib.Int
end

external runtime5 : unit -> bool @@ portable = "%runtime5"

module type Mutex = sig @@ portable
  type t : value mod portable contended
  val create : unit -> t
  val lock : t -> unit
  val try_lock : t -> bool
  val unlock : t -> unit
  val protect : t @ local -> (unit -> 'a) -> 'a
end

module Mutex : Mutex = struct
  include (
    val (
      if runtime5 ()
      then (module Mutex : Mutex)
      else (module struct
        (* Define a fake version of Mutex for runtime4; we're not synchronizing access for
           the sake of systhreads, only multiple domains, and Mutex raises on runtime4
           unless you include the threads library (which we can't do since we need to work
           on JSOO) *)
        type t = unit
        let create () = ()
        let lock () = ()
        let try_lock () = true
        let unlock () = ()
        let protect () f = f ()
      end : Mutex)))
end

include Iarray.O
