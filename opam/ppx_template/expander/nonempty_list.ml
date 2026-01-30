open! Stdppx
open Import

type 'a t = ( :: ) of 'a * 'a list

let to_list (hd :: tl) : _ list = hd :: tl
let map (hd :: tl) ~f = f hd :: List.map tl ~f

let all_nonempty (r :: rs) =
  let open Result.Let_syntax in
  let* r = r in
  let+ rs = Result.all rs in
  r :: rs
;;

let map_result l ~f = map l ~f |> all_nonempty

let compare (x :: xs) (y :: ys) ~cmp =
  match cmp x y with
  | 0 -> List.compare ~cmp xs ys
  | r -> r
;;

let sort_uniq (hd :: tl) ~cmp =
  match List.sort_uniq ~cmp (hd :: tl) with
  | hd :: tl -> hd :: tl
  | [] -> assert false (* invariant *)
;;

let concat ((x :: xs) :: ys) = x :: List.concat (xs :: List.map ys ~f:to_list)

let rec product = function
  | hd :: [] -> map hd ~f:(fun x -> [ x ])
  | hd :: tl :: tls ->
    let ys = product (tl :: tls) in
    map hd ~f:(fun x -> map ys ~f:(fun y -> x :: to_list y)) |> concat
;;
