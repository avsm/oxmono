type 'a t

module Let_syntax : sig
  val return : 'a -> 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val map2 : 'a -> 'b -> f:('a -> 'b -> 'c) -> 'c
    val map3 : 'a -> 'b -> 'c -> f:('a -> 'b -> 'c -> 'd) -> 'd
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig
      val return : 'a -> 'a t
    end
  end
end
