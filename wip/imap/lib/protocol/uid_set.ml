module A = Stdlib_stable.Iarray

(* Interval [i] runs from [t.:(2i)] to [t.:(2i+1)]. Intervals are sorted,
   disjoint and non-adjacent, so equal sets have equal arrays and [mem]
   can bisect. *)
type t = Uid.t iarray

let empty : t = [: :]
let is_empty t = A.length t = 0
let singleton u : t = [: u; u :]
let count t = A.length t / 2
let lo t i = A.get t (2 * i)
let hi t i = A.get t (2 * i + 1)
let intervals t = List.init (count t) (fun i -> lo t i, hi t i)

let lt a b = Uid.compare a b < 0

(* A builder appends intervals in ascending order of their lower bound and
   merges each with the last one when they overlap or touch. *)
type builder = { mutable bounds : Uid.t array; mutable len : int }

let builder n u = { bounds = Array.make (max 2 (2 * n)) u; len = 0 }

let push b first last =
  let n = b.len in
  if n > 0 &&
     (let top = b.bounds.(n - 1) in
      not (lt top first) ||
      match Uid.succ top with Some s -> Uid.equal s first | None -> false)
  then (if lt b.bounds.(n - 1) last then b.bounds.(n - 1) <- last)
  else begin
    if n = Array.length b.bounds then begin
      let grown = Array.make (2 * n) first in
      Array.blit b.bounds 0 grown 0 n;
      b.bounds <- grown
    end;
    b.bounds.(n) <- first;
    b.bounds.(n + 1) <- last;
    b.len <- n + 2
  end

let build b : t = A.init b.len (fun i -> b.bounds.(i))

let normalize = function
  | [] -> empty
  | ((u, _) :: _) as xs ->
      let xs = List.map (fun (a, b) -> if lt b a then b, a else a, b) xs in
      let xs = List.sort (fun (a, _) (b, _) -> Uid.compare a b) xs in
      let b = builder (List.length xs) u in
      List.iter (fun (first, last) -> push b first last) xs;
      build b

let of_intervals = normalize
let of_list l = normalize (List.map (fun u -> u, u) l)

let union a b =
  let na = count a and nb = count b in
  if na = 0 then b else if nb = 0 then a else
  let out = builder (na + nb) (lo a 0) in
  let rec merge i j =
    if i = na && j = nb then ()
    else if j = nb || (i < na && not (lt (lo b j) (lo a i))) then
      (push out (lo a i) (hi a i); merge (i + 1) j)
    else (push out (lo b j) (hi b j); merge i (j + 1)) in
  merge 0 0;
  build out

let add u t = union (singleton u) t

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
  if is_empty t then invalid_arg "Uid_set.to_wire: empty set";
  let buf = Buffer.create (count t * 16) in
  for i = 0 to count t - 1 do
    if i > 0 then Buffer.add_char buf ',';
    let a = lo t i and b = hi t i in
    Buffer.add_string buf (Uid.to_string a);
    if not (Uid.equal a b) then begin
      Buffer.add_char buf ':';
      Buffer.add_string buf (Uid.to_string b)
    end
  done;
  Buffer.contents buf

let cardinality t =
  let rec sum i acc =
    if i = count t then acc
    else sum (i + 1) (Int64.add acc (Int64.succ (Int64.sub
      (Uid.to_int64 (hi t i)) (Uid.to_int64 (lo t i))))) in
  sum 0 0L

(* [mem] bisects for the last interval whose lower bound is at most [x]. *)
let mem x t =
  let rec search low high =
    if low >= high then low - 1
    else
      let mid = (low + high) / 2 in
      if lt x (lo t mid) then search low mid else search (mid + 1) high in
  let i = search 0 (count t) in
  i >= 0 && not (lt (hi t i) x)

let inter a b =
  let na = count a and nb = count b in
  if na = 0 || nb = 0 then empty else
  let out = builder (na + nb) (lo a 0) in
  let rec go i j =
    if i < na && j < nb then begin
      let a1 = lo a i and a2 = hi a i and b1 = lo b j and b2 = hi b j in
      let low = if lt a1 b1 then b1 else a1
      and high = if lt a2 b2 then a2 else b2 in
      if not (lt high low) then push out low high;
      if lt a2 b2 then go (i + 1) j else go i (j + 1)
    end in
  go 0 0;
  build out

let diff a b =
  let na = count a and nb = count b in
  if na = 0 || nb = 0 then a else
  let out = builder (na + nb) (lo a 0) in
  (* [go i first j] emits what remains of interval [i] of [a] from [first]
     on, then the rest of [a], less the intervals of [b] from [j] on. *)
  let rec go i first j =
    if i = na then ()
    else if j = nb then begin
      push out first (hi a i);
      for k = i + 1 to na - 1 do push out (lo a k) (hi a k) done
    end else
      let last = hi a i and b1 = lo b j and b2 = hi b j in
      if lt b2 first then go i first (j + 1)
      else if lt last b1 then (push out first last; next (i + 1) j)
      else begin
        (match Uid.pred b1 with
         | Some p when lt first b1 -> push out first p
         | _ -> ());
        match Uid.succ b2 with
        | Some n when lt b2 last -> go i n (j + 1)
        | _ -> next (i + 1) j
      end
  and next i j = if i < na then go i (lo a i) j in
  go 0 (lo a 0) 0;
  build out

let fold f t acc =
  let rec span u last acc =
    let acc = f u acc in
    if Uid.equal u last then acc
    else match Uid.succ u with
      | Some n -> span n last acc
      | None -> acc in
  let rec go i acc = if i = count t then acc else go (i + 1)
      (span (lo t i) (hi t i) acc) in
  go 0 acc

let iter f t = fold (fun u () -> f u) t ()
let to_list t = List.rev (fold List.cons t [])

let equal a b =
  A.length a = A.length b && A.for_all2 Uid.equal a b

let compare a b =
  let na = A.length a and nb = A.length b in
  let rec go i =
    if i = na then (if i = nb then 0 else -1)
    else if i = nb then 1
    else match Uid.compare (A.get a i) (A.get b i) with
      | 0 -> go (i + 1)
      | n -> n in
  go 0

let pp ppf t =
  Format.pp_print_string ppf (if is_empty t then "(empty)" else to_wire t)
