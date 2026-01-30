open! Core
open Expect_test_helpers_core
open Filesystem_types
open File_permissions.Operators

type t = File_permissions.t
[@@deriving compare ~localize, equal ~localize, hash, quickcheck, sexp_of]

open struct
  (** As we re-export defined constants, record them for use as examples. *)

  let examples = Queue.create ()

  let example t =
    Queue.enqueue examples t;
    t
  ;;
end

let empty = example File_permissions.empty
let setuid = example File_permissions.setuid
let setgid = example File_permissions.setgid
let sticky = example File_permissions.sticky
let u_r = example File_permissions.u_r
let u_w = example File_permissions.u_w
let u_x = example File_permissions.u_x
let g_r = example File_permissions.g_r
let g_w = example File_permissions.g_w
let g_x = example File_permissions.g_x
let o_r = example File_permissions.o_r
let o_w = example File_permissions.o_w
let o_x = example File_permissions.o_x
let u_rw = example File_permissions.u_rw
let g_rw = example File_permissions.g_rw
let o_rw = example File_permissions.o_rw
let u_rx = example File_permissions.u_rx
let g_rx = example File_permissions.g_rx
let o_rx = example File_permissions.o_rx
let u_rwx = example File_permissions.u_rwx
let g_rwx = example File_permissions.g_rwx
let o_rwx = example File_permissions.o_rwx
let ug_r = example File_permissions.ug_r
let ug_w = example File_permissions.ug_w
let ug_x = example File_permissions.ug_x
let ug_rw = example File_permissions.ug_rw
let ug_rx = example File_permissions.ug_rx
let ug_rwx = example File_permissions.ug_rwx
let ugo_r = example File_permissions.ugo_r
let ugo_w = example File_permissions.ugo_w
let ugo_x = example File_permissions.ugo_x
let ugo_rw = example File_permissions.ugo_rw
let ugo_rx = example File_permissions.ugo_rx
let ugo_rwx = example File_permissions.ugo_rwx
let t_0644 = example File_permissions.t_0644
let t_0664 = example File_permissions.t_0664
let t_0755 = example File_permissions.t_0755
let t_0775 = example File_permissions.t_0775
let special_mode_bits = example File_permissions.special_mode_bits

let all_including_special_mode_bits =
  example File_permissions.all_including_special_mode_bits
;;

open struct
  (** Freeze the list of examples. *)
  let examples = Queue.to_list examples
end

let%expect_test "constants" =
  List.iter examples ~f:(fun t -> print_s (sexp_of_t t));
  [%expect
    {|
    ----------
    ---S------
    ------S---
    ---------T
    -r--------
    --w-------
    ---x------
    ----r-----
    -----w----
    ------x---
    -------r--
    --------w-
    ---------x
    -rw-------
    ----rw----
    -------rw-
    -r-x------
    ----r-x---
    -------r-x
    -rwx------
    ----rwx---
    -------rwx
    -r--r-----
    --w--w----
    ---x--x---
    -rw-rw----
    -r-xr-x---
    -rwxrwx---
    -r--r--r--
    --w--w--w-
    ---x--x--x
    -rw-rw-rw-
    -r-xr-xr-x
    -rwxrwxrwx
    -rw-r--r--
    -rw-rw-r--
    -rwxr-xr-x
    -rwxrwxr-x
    ---S--S--T
    -rwsrwsrwt
    |}]
;;

let to_int = File_permissions.to_int
let of_int_exn = File_permissions.of_int_exn

let%expect_test "[to_int] / [of_int_exn]" =
  let test t ~verbose =
    let int = to_int t in
    if verbose then print_s [%sexp (t : t), "=", (sprintf "0o%04o" int : string)];
    let round_trip = of_int_exn int in
    require_equal (module File_permissions) t round_trip
  in
  List.iter examples ~f:(test ~verbose:true);
  quickcheck_m (module File_permissions) ~f:(test ~verbose:false);
  [%expect
    {|
    (---------- = 0o0000)
    (---S------ = 0o4000)
    (------S--- = 0o2000)
    (---------T = 0o1000)
    (-r-------- = 0o0400)
    (--w------- = 0o0200)
    (---x------ = 0o0100)
    (----r----- = 0o0040)
    (-----w---- = 0o0020)
    (------x--- = 0o0010)
    (-------r-- = 0o0004)
    (--------w- = 0o0002)
    (---------x = 0o0001)
    (-rw------- = 0o0600)
    (----rw---- = 0o0060)
    (-------rw- = 0o0006)
    (-r-x------ = 0o0500)
    (----r-x--- = 0o0050)
    (-------r-x = 0o0005)
    (-rwx------ = 0o0700)
    (----rwx--- = 0o0070)
    (-------rwx = 0o0007)
    (-r--r----- = 0o0440)
    (--w--w---- = 0o0220)
    (---x--x--- = 0o0110)
    (-rw-rw---- = 0o0660)
    (-r-xr-x--- = 0o0550)
    (-rwxrwx--- = 0o0770)
    (-r--r--r-- = 0o0444)
    (--w--w--w- = 0o0222)
    (---x--x--x = 0o0111)
    (-rw-rw-rw- = 0o0666)
    (-r-xr-xr-x = 0o0555)
    (-rwxrwxrwx = 0o0777)
    (-rw-r--r-- = 0o0644)
    (-rw-rw-r-- = 0o0664)
    (-rwxr-xr-x = 0o0755)
    (-rwxrwxr-x = 0o0775)
    (---S--S--T = 0o7000)
    (-rwsrwsrwt = 0o7777)
    |}]
;;

let is_empty = File_permissions.is_empty

let%expect_test "[is_empty]" =
  quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
    require (Bool.equal (is_empty t) (to_int t = 0)));
  [%expect {| |}]
;;

module Operators = struct
  let ( lxor ) = File_permissions.( lxor )

  let%expect_test "[lxor]" =
    quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
      require_equal (module File_permissions) (t lxor t) empty;
      require_equal (module File_permissions) (t lxor empty) t;
      require_equal
        (module File_permissions)
        (t lor (all_including_special_mode_bits lxor t))
        all_including_special_mode_bits;
      require_equal
        (module File_permissions)
        (t land (all_including_special_mode_bits lxor t))
        empty);
    [%expect {| |}];
    quickcheck_m
      (module struct
        type t = File_permissions.t * File_permissions.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) -> require_equal (module File_permissions) (a lxor b) (b lxor a));
    [%expect {| |}]
  ;;

  let ( land ) = File_permissions.( land )

  let%expect_test "[land]" =
    quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
      require_equal (module File_permissions) (t land t) t;
      require_equal (module File_permissions) (t land empty) empty;
      require_equal (module File_permissions) (t land all_including_special_mode_bits) t);
    [%expect {| |}];
    quickcheck_m
      (module struct
        type t = File_permissions.t * File_permissions.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) -> require_equal (module File_permissions) (a land b) (b land a));
    [%expect {| |}]
  ;;

  let ( lor ) = File_permissions.( lor )

  let%expect_test "[lor]" =
    quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
      require_equal (module File_permissions) (t lor t) t;
      require_equal (module File_permissions) (t lor empty) t;
      require_equal
        (module File_permissions)
        (t lor all_including_special_mode_bits)
        all_including_special_mode_bits);
    [%expect {| |}];
    quickcheck_m
      (module struct
        type t = File_permissions.t * File_permissions.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (a, b) -> require_equal (module File_permissions) (a lor b) (b lor a));
    [%expect {| |}]
  ;;
end

include Operators

let intersection = File_permissions.intersection
let union = File_permissions.union
let symmetric_diff = File_permissions.symmetric_diff

let%expect_test "[intersection] and [union] and [symmetric_diff]" =
  require (phys_equal intersection ( land ));
  require (phys_equal union ( lor ));
  require (phys_equal symmetric_diff ( lxor ));
  [%expect {| |}]
;;

let do_intersect = File_permissions.do_intersect

let%expect_test "[do_intersect]" =
  quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
    require (not (do_intersect t empty));
    require_equal (module Bool) (do_intersect t t) (not (is_empty t));
    require_equal
      (module Bool)
      (do_intersect t all_including_special_mode_bits)
      (not (is_empty t)));
  [%expect {| |}];
  quickcheck_m
    (module struct
      type t = File_permissions.t * File_permissions.t [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (a, b) ->
      require_equal (module Bool) (do_intersect a b) (do_intersect b a);
      require_equal (module Bool) (do_intersect a (a lor b)) (not (is_empty a));
      require_equal (module Bool) (do_intersect b (a lor b)) (not (is_empty b)));
  [%expect {| |}]
;;

let is_subset = File_permissions.is_subset

let%expect_test "[is_subset]" =
  quickcheck_m (module File_permissions) ~examples ~f:(fun t ->
    require (is_subset t ~of_:t);
    require (Bool.equal (is_empty t) (is_subset t ~of_:empty));
    require (is_subset t ~of_:all_including_special_mode_bits));
  [%expect {| |}];
  quickcheck_m
    (module struct
      type t = File_permissions.t * File_permissions.t [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (a, b) ->
      require (Bool.equal (equal a b) (is_subset a ~of_:b && is_subset b ~of_:a));
      require (is_subset a ~of_:(a lor b));
      require (is_subset b ~of_:(a lor b));
      require (is_subset (a land b) ~of_:a);
      require (is_subset (a land b) ~of_:b));
  [%expect {| |}]
;;
