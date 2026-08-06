(* The kind annotation without the attribute is rejected, which shows
   the attribute is what authorises the unprovable kind. Together with
   repro.ml and use_sites.ml: the attribute has no effect at all unless
   the declaration also repeats the kind. *)
type t : value mod portable contended = { g : (int -> int) array }
