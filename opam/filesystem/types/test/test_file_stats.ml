open! Core
open Expect_test_helpers_core
open Filesystem_types

include struct
  type t = File_stats.t [@@deriving equal ~localize, quickcheck, sexp_of]
end

type t = File_stats.t =
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

let%expect_test "[sexp_of_t]" =
  print_s
    (sexp_of_t
       { host_device = 100
       ; inode = 200
       ; kind = Symlink
       ; permissions = File_permissions.ugo_r
       ; hard_links = 300
       ; user_id = 400
       ; group_id = 500
       ; file_device = 600
       ; size = Int63.of_int 700
       ; access_time = Time_ns_unix.epoch
       ; modify_time = Time_ns_unix.epoch
       ; status_time = Time_ns_unix.epoch
       });
  [%expect
    {|
    ((host_device <hidden>)
     (inode       <hidden>)
     (kind        Symlink)
     (permissions -r--r--r--)
     (hard_links  300)
     (user_id     <hidden>)
     (group_id    <hidden>)
     (file_device <hidden>)
     (size        700)
     (access_time <hidden>)
     (modify_time <hidden>)
     (status_time <hidden>))
    |}]
;;

open struct
  let round_time time_ns =
    Time_ns.prev_multiple
      ~can_equal_before:true
      ~base:Time_ns.epoch
      ~interval:Time_ns.Span.second
      ~before:time_ns
      ()
  ;;

  let round_times t =
    { t with
      access_time = round_time t.access_time
    ; modify_time = round_time t.modify_time
    ; status_time = round_time t.status_time
    }
  ;;
end

let of_unix_stats = File_stats.of_unix_stats
let to_unix_stats = File_stats.to_unix_stats

let%expect_test "[to_unix_stats] / [of_unix_stats]" =
  quickcheck_m
    (module struct
      type nonrec t = t [@@deriving quickcheck, sexp_of]

      (* Avoid rounding error during float conversions. *)
      let quickcheck_generator =
        Quickcheck.Generator.map quickcheck_generator ~f:round_times
      ;;
    end)
    ~f:(fun t ->
      let unix_stats = to_unix_stats t in
      let round_trip = of_unix_stats unix_stats in
      match equal t round_trip with
      | true -> ()
      | false ->
        print_cr
          [%message
            "round trip failed" (t : t) (unix_stats : Core_unix.stats) (round_trip : t)];
        Expect_test_patdiff.print_patdiff_s [%sexp (t : t)] [%sexp (round_trip : t)]);
  [%expect {| |}]
;;
