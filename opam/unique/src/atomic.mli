@@ portable

(** Atomic references to unique values. Thread-safe version of {!Ref.t}. *)
type (!'a : value_or_null) t : immutable_data with 'a @@ contended portable

(** [make a] creates a new reference containing the given value [a]. *)
external make
  : ('a : value_or_null).
  'a @ contended once portable unique -> ('a t[@local_opt]) @ unique
  = "%makemutable"

(** [get t] destroys [t] to extract the value inside. *)
external get
  : ('a : value_or_null).
  'a t @ local unique -> 'a @ contended once portable unique
  = "%atomic_load"

(** [set t a] overrides the stored value inside [t] with [a]. *)
external set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended once portable unique -> unit
  = "%atomic_set"

(** [exchange t a] extracts the value inside [t], replacing it with [a]. *)
external exchange
  : ('a : value_or_null).
  'a t @ local
  -> 'a @ contended once portable unique
  -> 'a @ contended once portable unique
  = "%atomic_exchange"

(** [compare_and_set r ~if_phys_equal_to ~replace_with] sets the new value of [r] to
    [replace_with] {i only} if its current value is physically equal to [if_phys_equal_to]
    -- the comparison and the set occur atomically. Returns [Set_here] if the value was
    set to [replace_with] by this call to [compare_and_set], or [Compare_failed] if the
    current value was not physically equal to [if_phys_equal_to] and hence the atomic
    reference was left unchanged. *)
external compare_and_set
  : ('a : value_or_null).
  'a t @ local
  -> if_phys_equal_to:'a @ contended
  -> replace_with:'a @ contended once portable unique
  -> Basement.Compare_failed_or_set_here.t
  = "%atomic_cas"
