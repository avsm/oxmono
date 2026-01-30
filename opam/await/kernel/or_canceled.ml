open Base

type%template ('a : k) t =
  | Canceled
  | Completed of 'a
[@@kind
  k
  = ( value_or_null
    , void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null )]
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]

let%template[@inline] completed_exn : (_ t[@kind k]) -> _ = function
  | Completed a -> a
  | Canceled ->
    (match failwith "[Await.Or_canceled.completed_exn]: Canceled" with
     | (_ : Nothing.t) -> .)
[@@kind
  k
  = ( value_or_null
    , void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null )]
;;

include Monad.Make [@mode local] [@modality portable] (struct
    type nonrec 'a t = 'a t

    let[@inline] return a = Completed a
    let map = `Define_using_bind

    let[@inline] bind t ~f =
      match t with
      | Canceled -> Canceled
      | Completed a -> f a
    ;;
  end)

let never_completed = function
  | Canceled -> ()
  | Completed (_ : Nothing.t) -> .
;;

module Exn = struct
  exception Canceled

  let%template[@inline] catch (f @ local once) : (_ t[@kind k]) @ u =
    match f () with
    | value -> Completed value
    | exception Canceled -> Canceled
  [@@kind
    k
    = ( value_or_null
      , void
      , value_or_null & void
      , value_or_null & value_or_null
      , (value_or_null & value_or_null) & value_or_null )]
  [@@mode u = (aliased, unique)]
  ;;
end
