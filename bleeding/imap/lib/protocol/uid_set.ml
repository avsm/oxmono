type t = (Uid.t * Uid.t) list

let empty = []
let is_empty t = t = []
let singleton u = [u,u]
let intervals t = t

let lt a b = Uid.compare a b < 0
let umin a b = if lt b a then b else a
let umax a b = if lt a b then b else a

let adjacent last next =
  match Uid.succ last with Some n -> Uid.equal n next | None -> false

let normalize xs =
  let xs = List.map (fun (a,b) -> if lt b a then b,a else a,b) xs in
  let xs = List.sort (fun (a,_) (b,_) -> Uid.compare a b) xs in
  List.rev (List.fold_left (fun acc (a,b) ->
    match acc with
    | (c,d)::rest when not (lt d a) || adjacent d a -> (c, umax b d)::rest
    | _ -> (a,b)::acc) [] xs)

let of_intervals = normalize
let of_list l = normalize (List.map (fun u -> u,u) l)
let union a b = normalize (a @ b)
let add u t = union [u,u] t

let star = match Uid.of_int64 4_294_967_295L with
  | Ok u -> u
  | Error e -> invalid_arg e

let of_wire ?(allow_star=false) s =
  let endpoint x =
    if allow_star && x="*" then Ok star
    else if x="" || x.[0]='0' ||
            not (String.for_all (fun c -> c >= '0' && c <= '9') x)
    then Error ("invalid UID set endpoint " ^ x)
    else
      match Option.map Uid.of_int64 (Int64.of_string_opt x) with
      | Some (Ok n) -> Ok n
      | _ -> Error ("UID set endpoint " ^ x ^ " outside range") in
  if s="" then Error "empty UID set"
  else
    let rec parse acc = function
      | [] -> Ok (normalize (List.rev acc))
      | part::rest ->
          (match String.split_on_char ':' part with
           | [one] ->
               (match endpoint one with
                | Error _ as e -> e | Ok n -> parse ((n,n)::acc) rest)
           | [first;last] ->
               (match endpoint first,endpoint last with
                | Ok a,Ok b -> parse ((a,b)::acc) rest
                | Error e,_ | _,Error e -> Error e)
           | _ -> Error ("invalid UID range " ^ part)) in
    parse [] (String.split_on_char ',' s)

let to_wire t =
  if t = [] then invalid_arg "Uid_set.to_wire: empty set";
  String.concat "," (List.map (fun (a,b) ->
    if Uid.equal a b then Uid.to_string a
    else Uid.to_string a ^ ":" ^ Uid.to_string b) t)

let cardinality t = List.fold_left (fun acc (a,b) ->
  Int64.add acc (Int64.succ (Int64.sub (Uid.to_int64 b) (Uid.to_int64 a))))
  0L t

let mem x t = List.exists (fun (a,b) -> not (lt x a) && not (lt b x)) t

let rec inter a b = match a,b with
  | [],_ | _,[] -> []
  | (a1,a2)::ra,(b1,b2)::rb ->
      let lo = umax a1 b1 and hi = umin a2 b2 in
      let rest = if lt a2 b2 then inter ra b else inter a rb in
      if lt hi lo then rest else (lo,hi)::rest

let rec diff a b = match a,b with
  | [],_ -> []
  | a,[] -> a
  | (a1,a2)::ra,(b1,b2)::rb ->
      if lt b2 a1 then diff a rb
      else if lt a2 b1 then (a1,a2)::diff ra b
      else
        let left = match Uid.pred b1 with
          | Some p when lt a1 b1 -> [a1,p]
          | _ -> [] in
        let rest = match Uid.succ b2 with
          | Some n when lt b2 a2 -> diff ((n,a2)::ra) rb
          | _ -> diff ra b in
        left @ rest

let fold f t acc =
  let rec span u last acc =
    let acc = f u acc in
    if Uid.equal u last then acc
    else match Uid.succ u with
      | Some n -> span n last acc
      | None -> acc in
  List.fold_left (fun acc (a,b) -> span a b acc) acc t

let iter f t = fold (fun u () -> f u) t ()
let to_list t = List.rev (fold List.cons t [])

let equal = List.equal (fun (a,b) (c,d) -> Uid.equal a c && Uid.equal b d)
let compare = List.compare (fun (a,b) (c,d) ->
  match Uid.compare a c with 0 -> Uid.compare b d | n -> n)
let pp ppf t =
  Format.pp_print_string ppf (if t=[] then "(empty)" else to_wire t)
