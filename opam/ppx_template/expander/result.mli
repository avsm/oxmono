open! Stdppx

include module type of struct
  include Result
end

module Let_syntax : sig
  val ( let* )
    :  ('ok1, 'err) result
    -> ('ok1 -> ('ok2, 'err) result)
    -> ('ok2, 'err) result

  val ( >>= )
    :  ('ok1, 'err) result
    -> ('ok1 -> ('ok2, 'err) result)
    -> ('ok2, 'err) result

  val ( let+ ) : ('ok1, 'err) result -> ('ok1 -> 'ok2) -> ('ok2, 'err) result
  val ( >>| ) : ('ok1, 'err) result -> ('ok1 -> 'ok2) -> ('ok2, 'err) result
end

val map_error : ('a, 'b) result -> f:('b -> 'c) -> ('a, 'c) result
val all : ('a, 'err) result list -> ('a list, 'err) result
val all_unit : (unit, 'err) result list -> (unit, 'err) result
val collect_errors : ('a, Syntax_error.t) result list -> ('a list, Syntax_error.t) result
val combine_errors : ('ok, 'err) result list -> ('ok list, 'err list) result
