type (!'a : value_or_null) t : immutable_data with 'a @@ contended portable

external make
  : ('a : value_or_null).
  'a @ contended once portable unique -> ('a t[@local_opt]) @ unique
  @@ portable
  = "%makemutable"

external get
  : ('a : value_or_null).
  'a t @ local unique -> 'a @ contended once portable unique
  @@ portable
  = "%atomic_load"

external set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended once portable unique -> unit
  @@ portable
  = "%atomic_set"

external exchange
  : ('a : value_or_null).
  'a t @ local
  -> 'a @ contended once portable unique
  -> 'a @ contended once portable unique
  @@ portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value_or_null).
  'a t @ local
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended once portable unique
  -> Basement.Compare_failed_or_set_here.t
  @@ portable
  = "%atomic_cas"
