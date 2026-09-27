let max_uid = 4_294_967_295L

module Uid = struct
  type t = int64
  let of_int64 n =
    if n < 1L || n > max_uid then Error "UID must be in 1..4294967295"
    else Ok n
  let to_int64 n = n
  let to_string = Int64.to_string
  let equal = Int64.equal
  let compare = Int64.compare
  let pp ppf n = Format.fprintf ppf "%Ld" n
end

module Uidvalidity = struct
  type t = int64
  let of_int64 n =
    if n < 1L || n > max_uid then Error "UIDVALIDITY must be in 1..4294967295"
    else Ok n
  let to_int64 n = n
  let equal = Int64.equal
  let compare = Int64.compare
  let pp ppf n = Format.fprintf ppf "%Ld" n
end

module Seq = struct
  type t = int64
  let of_int64 n =
    if n < 1L || n > max_uid then Error "sequence number must be in 1..4294967295"
    else Ok n
  let to_int64 n = n
end

module Modseq = struct
  type t = int64
  let of_int64 n =
    if n < 1L then Error "MODSEQ must be positive" else Ok n
  let to_int64 n = n
  let equal = Int64.equal
  let compare = Int64.compare
  let pp ppf n = Format.fprintf ppf "%Ld" n
end

module Uid_set = struct
  type t = (Uid.t * Uid.t) list
  let empty = []
  let singleton u = [u,u]
  let intervals t = t
  let normalize xs =
    let xs = List.map (fun (a,b) -> if a <= b then a,b else b,a) xs in
    let xs = List.sort (fun (a,_) (b,_) -> Int64.compare a b) xs in
    List.rev (List.fold_left (fun acc (a,b) ->
      match acc with
      | (c,d)::rest when a <= d || (d < max_uid && a = Int64.succ d) ->
          (c, Int64.max b d)::rest
      | _ -> (a,b)::acc) [] xs)
  let of_intervals = normalize
  let of_wire ?(allow_star=false) s =
    let endpoint x =
      if allow_star && x="*" then Ok max_uid
      else if x="" || x.[0]='0' ||
              not (String.for_all (fun c -> c >= '0' && c <= '9') x)
      then Error ("invalid UID set endpoint " ^ x)
      else match Int64.of_string_opt x with
        | Some n when n <= max_uid -> Ok n
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
  let is_empty t = t = []
  let union a b = normalize (a @ b)
  let mem x t = List.exists (fun (a,b) -> a <= x && x <= b) t
  let cardinality t = List.fold_left (fun acc (a,b) ->
    Int64.add acc (Int64.succ (Int64.sub b a))) 0L t
  let to_wire t =
    String.concat "," (List.map (fun (a,b) ->
      if a=b then Int64.to_string a
      else Int64.to_string a ^ ":" ^ Int64.to_string b) t)
  let equal = List.equal (fun (a,b) (c,d) -> a=c && b=d)
  let compare = List.compare (fun (a,b) (c,d) ->
    match Int64.compare a c with 0 -> Int64.compare b d | n -> n)
  let pp ppf t =
    Format.pp_print_string ppf (if t=[] then "(empty)" else to_wire t)
end
