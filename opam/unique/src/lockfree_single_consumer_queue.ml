open Base

(* SAFETY: This data structure must not expose operations that would duplicate elems. *)

type 'a t =
  { mutable head : 'a list Modes.Portended.t
  ; tail : 'a Lockfree_stack.t
  }

let create ?padded () =
  { head = { portended = [] }; tail = Lockfree_stack.create ?padded () }
;;

let is_empty t = phys_equal [] t.head.portended && Lockfree_stack.is_empty t.tail

let rec rev_to (ys @ contended once portable unique) : _ @ contended once portable unique
  = function
  | [] -> ys
  | x :: xs -> rev_to (x :: ys) xs
;;

let dequeue_or_null t =
  match t.head.portended with
  | x :: xs ->
    t.head <- { portended = xs };
    This ((Obj.magic_unique [@mode contended portable]) x)
  | [] ->
    if Lockfree_stack.is_empty t.tail
    then Null
    else (
      match rev_to [] (Lockfree_stack.pop_all t.tail) with
      | x :: xs ->
        let xs = (Obj.magic_many [@mode contended portable]) xs in
        if not (phys_equal [] xs) then t.head <- { portended = xs };
        This x
      | [] -> Null)
;;

let[@tail_mod_cons] rec ( @ )
  (xs @ contended once portable unique)
  (ys @ contended once portable unique)
  =
  match xs with
  | [] -> ys
  | x :: xs -> x :: (xs @ ys)
;;

let dequeue_all t =
  let head = t.head.portended in
  let tail = Lockfree_stack.pop_all t.tail in
  if not (phys_equal [] head) then t.head <- { portended = [] };
  (Obj.magic_unique [@mode contended portable]) head @ rev_to [] tail
;;

let[@inline] enqueue t x = Lockfree_stack.push t.tail x
