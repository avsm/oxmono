val pages :
  ?per_page:int -> (page:string -> per_page:string -> 'a list) -> 'a Seq.t
(** [pages ?per_page f] is the items of every page of [f], in order.
    [f ~page ~per_page] fetches one page. Pages are numbered from 1. The
    sequence is lazy and stops after a page with fewer than [per_page] items.
    [per_page] defaults to 100. The arguments are strings because the
    generated operations take them as strings. *)
