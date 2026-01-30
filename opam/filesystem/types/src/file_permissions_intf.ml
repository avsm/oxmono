open! Core

(** Infix aliases for functions in [S]. *)
module type Operators = sig @@ portable
  type t

  (** Alias for [intersection]. *)
  val ( land ) : t -> t -> t

  (** Alias for [union]. *)
  val ( lor ) : t -> t -> t

  (** Alias for [symmetric_diff]. *)
  val ( lxor ) : t -> t -> t
end

module type S = sig @@ portable
  (** Represents unix file permissions. *)
  type t : immediate
  [@@deriving
    compare ~portable ~localize
    , equal ~portable ~localize
    , hash ~portable
    , quickcheck ~portable
    , sexp_of ~portable]

  (** {2 Constants} *)

  (** No permissions. *)
  val empty : t

  (** Individual file permission bits. *)

  val u_r : t
  val u_w : t
  val u_x : t
  val g_r : t
  val g_w : t
  val g_x : t
  val o_r : t
  val o_w : t
  val o_x : t

  (** All file permission bits, i.e. [0o0777]. *)

  val ugo_rwx : t

  (** Combining permissions with [rw] (read/write), [rx] (executables), and [rwx] (e.g.
      directories or symlinks). *)

  val u_rw : t
  val g_rw : t
  val o_rw : t
  val u_rx : t
  val g_rx : t
  val o_rx : t
  val u_rwx : t
  val g_rwx : t
  val o_rwx : t

  (** Combining permissions with [ug] (user and group) and [ugo] (everyone). *)

  val ug_r : t
  val ug_w : t
  val ug_x : t
  val ug_rw : t
  val ug_rx : t
  val ug_rwx : t
  val ugo_r : t
  val ugo_w : t
  val ugo_x : t
  val ugo_rw : t
  val ugo_rx : t

  (** Commonly used combinations. *)

  (** [u_rw lor ugo_r], i.e. [of_int_exn 0o0644] *)
  val t_0644 : t

  (** [ug_rw lor ugo_r], i.e. [of_int_exn 0o0664] *)
  val t_0664 : t

  (** [u_rwx lor ugo_rx], i.e. [of_int_exn 0o0755] *)
  val t_0755 : t

  (** [ug_rwx lor ugo_rx], i.e. [of_int_exn 0o0775] *)
  val t_0775 : t

  (** Special mode bits, used for properties other than actual file permissions. *)

  val setuid : t
  val setgid : t
  val sticky : t

  (** Equivalent to [setuid lor setgid lor sticky], i.e. [0o7000]. *)
  val special_mode_bits : t

  (** Equivalent to [ugo_rwx lor special_mode_bits], i.e. [0o7777]. *)
  val all_including_special_mode_bits : t

  (** {2 Predicates} *)

  (** Test if the input is [empty]. *)
  val is_empty : t -> bool

  (** Test if two permissions contain at least one set bit in common. *)
  val do_intersect : t -> t -> bool

  (** Test if [t] contains only permission bits also set in [of_]. *)
  val is_subset : t -> of_:t -> bool

  (** {2 Constructors} *)

  (** Produce only the permissions contained in both inputs. *)
  val intersection : t -> t -> t

  (** Produce all permissions contained in either or both inputs. *)
  val union : t -> t -> t

  (** Produce permissions contained in either input but not both. *)
  val symmetric_diff : t -> t -> t

  (** {2 Operators} *)

  module Operators : Operators with type t := t
  include Operators with type t := t

  (** {2 Integer conversion}

      Permissions are represented as unsigned 12-bit integers. The definitions of
      individual permission bits above proceed from most significant bit to least
      significant bit in the integer representation. *)

  val of_int_exn : int -> t
  val to_int : t -> int
end

module type File_permissions = sig
  module type S = S

  include S
end
