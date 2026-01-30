open! Core
include File_stats_intf

module Or_testing = struct
  (** Hide nondeterministic or host-specific values during expect tests. *)
  type 'a t = 'a [@@deriving equal ~portable ~localize, quickcheck ~portable]

  let sexp_of_t sexp_of_a a =
    if am_running_test then Sexp.Atom "<hidden>" else sexp_of_a a
  ;;
end

type t =
  { host_device : int Or_testing.t
  ; inode : int Or_testing.t
  ; kind : File_kind.t
  ; permissions : File_permissions.t
  ; hard_links : int
  ; user_id : int Or_testing.t
  ; group_id : int Or_testing.t
  ; file_device : int Or_testing.t
  ; size : Int63.t
  ; access_time : Time_ns_unix.t Or_testing.t
  ; modify_time : Time_ns_unix.t Or_testing.t
  ; status_time : Time_ns_unix.t Or_testing.t
  }
[@@deriving equal ~portable ~localize, quickcheck ~portable, sexp_of ~portable]

let to_unix_stats
  { host_device
  ; inode
  ; kind
  ; permissions
  ; hard_links
  ; user_id
  ; group_id
  ; file_device
  ; size
  ; access_time
  ; modify_time
  ; status_time
  }
  : Core_unix.stats
  =
  { st_dev = host_device
  ; st_ino = inode
  ; st_kind = kind |> File_kind.to_unix_file_kind
  ; st_perm = permissions |> File_permissions.to_int
  ; st_nlink = hard_links
  ; st_uid = user_id
  ; st_gid = group_id
  ; st_rdev = file_device
  ; st_size = size |> Int63.to_int64
  ; st_atime = access_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  ; st_mtime = modify_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  ; st_ctime = status_time |> Time_ns.to_span_since_epoch |> Time_ns.Span.to_sec
  }
;;

let of_unix_stats
  ({ st_dev
   ; st_ino
   ; st_kind
   ; st_perm
   ; st_nlink
   ; st_uid
   ; st_gid
   ; st_rdev
   ; st_size
   ; st_atime
   ; st_mtime
   ; st_ctime
   } :
    Core_unix.stats)
  =
  { host_device = st_dev
  ; inode = st_ino
  ; kind = st_kind |> File_kind.of_unix_file_kind
  ; permissions = st_perm |> File_permissions.of_int_exn
  ; hard_links = st_nlink
  ; user_id = st_uid
  ; group_id = st_gid
  ; file_device = st_rdev
  ; size = st_size |> Int63.of_int64_exn
  ; access_time = st_atime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  ; modify_time = st_mtime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  ; status_time = st_ctime |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
  }
;;
