open! Core

module type S = sig @@ portable
  module File_kind : T
  module File_permissions : T

  (** File statistics returned by [stat]. *)
  type t =
    { host_device : int
    ; inode : int
    ; kind : File_kind.t
    ; permissions : File_permissions.t
    ; hard_links : int
    ; user_id : int
    ; group_id : int
    ; file_device : int
    ; size : Int63.t
    ; access_time : Time_ns.t
    ; modify_time : Time_ns.t
    ; status_time : Time_ns.t
    }
  [@@deriving equal ~portable ~localize, quickcheck ~portable, sexp_of ~portable]

  (** Conversions *)

  val of_unix_stats : Core_unix.stats -> t
  val to_unix_stats : t -> Core_unix.stats
end

module type File_stats = sig
  module type S = S

  include
    S with module File_kind := File_kind and module File_permissions := File_permissions
end
