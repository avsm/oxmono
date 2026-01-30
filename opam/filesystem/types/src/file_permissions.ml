open! Core
include File_permissions_intf

type t = { perm : int }
[@@unboxed]
[@@deriving compare ~portable ~localize, equal ~portable ~localize, hash ~portable]

let mask = 0o7777
let is_ok int = int land mask = int

let[@cold] raise_not_ok int ~name =
  raise_s (Atom (sprintf "%s: invalid file permissions 0o%o" name int))
;;

let of_int_exn int =
  if is_ok int
  then { perm = int }
  else raise_not_ok int ~name:"File_permissions.of_int_exn"
;;

let to_int { perm } = perm
let empty = { perm = 0o0000 }
let setuid = { perm = 0o4000 }
let setgid = { perm = 0o2000 }
let sticky = { perm = 0o1000 }
let u_r = { perm = 0o0400 }
let u_w = { perm = 0o0200 }
let u_x = { perm = 0o0100 }
let g_r = { perm = 0o0040 }
let g_w = { perm = 0o0020 }
let g_x = { perm = 0o0010 }
let o_r = { perm = 0o0004 }
let o_w = { perm = 0o0002 }
let o_x = { perm = 0o0001 }

module Operators = struct
  let ( land ) a b = { perm = a.perm land b.perm }
  let ( lor ) a b = { perm = a.perm lor b.perm }
  let ( lxor ) a b = { perm = a.perm lxor b.perm }
end

include Operators

let u_rw = u_r lor u_w
let g_rw = g_r lor g_w
let o_rw = o_r lor o_w
let u_rx = u_r lor u_x
let g_rx = g_r lor g_x
let o_rx = o_r lor o_x
let u_rwx = u_r lor u_w lor u_x
let g_rwx = g_r lor g_w lor g_x
let o_rwx = o_r lor o_w lor o_x
let ug_r = u_r lor g_r
let ug_w = u_w lor g_w
let ug_x = u_x lor g_x
let ug_rw = u_rw lor g_rw
let ug_rx = u_rx lor g_rx
let ug_rwx = u_rwx lor g_rwx
let ugo_r = u_r lor g_r lor o_r
let ugo_w = u_w lor g_w lor o_w
let ugo_x = u_x lor g_x lor o_x
let ugo_rw = ugo_r lor ugo_w
let ugo_rx = ugo_r lor ugo_x
let ugo_rwx = u_rwx lor g_rwx lor o_rwx
let t_0644 = { perm = 0o644 }
let t_0664 = { perm = 0o664 }
let t_0755 = { perm = 0o755 }
let t_0775 = { perm = 0o775 }
let special_mode_bits = setuid lor setgid lor sticky
let all_including_special_mode_bits = ugo_rwx lor special_mode_bits
let intersection = ( land )
let union = ( lor )
let symmetric_diff = ( lxor )
let is_empty t = equal t empty
let do_intersect a b = not (is_empty (a land b))
let is_subset t ~of_ = equal t (t land of_)

let to_string t =
  (* Prints permissions the way [ls] does, with the file kind set to '-'. *)
  sprintf
    "-%c%c%c%c%c%c%c%c%c"
    (if do_intersect t u_r then 'r' else '-')
    (if do_intersect t u_w then 'w' else '-')
    (if do_intersect t setuid
     then if do_intersect t u_x then 's' else 'S'
     else if do_intersect t u_x
     then 'x'
     else '-')
    (if do_intersect t g_r then 'r' else '-')
    (if do_intersect t g_w then 'w' else '-')
    (if do_intersect t setgid
     then if do_intersect t g_x then 's' else 'S'
     else if do_intersect t g_x
     then 'x'
     else '-')
    (if do_intersect t o_r then 'r' else '-')
    (if do_intersect t o_w then 'w' else '-')
    (if do_intersect t sticky
     then if do_intersect t o_x then 't' else 'T'
     else if do_intersect t o_x
     then 'x'
     else '-')
;;

let sexp_of_t t = Sexp.Atom (to_string t)

let quickcheck_generator =
  Quickcheck.Generator.(map [@mode portable]) (Int.gen_incl 0 mask) ~f:of_int_exn
;;

let quickcheck_observer =
  Quickcheck.Observer.(unmap [@mode portable]) Int.quickcheck_observer ~f:to_int
;;

let quickcheck_shrinker =
  let singletons = List.init (Int.popcount mask) ~f:(fun i -> of_int_exn (1 lsl i)) in
  Quickcheck.Shrinker.(create [@mode portable]) (fun t ->
    List.filter_map singletons ~f:(fun bit ->
      if is_subset bit ~of_:t then Some (t lxor bit) else None)
    |> Sequence.of_list)
;;
