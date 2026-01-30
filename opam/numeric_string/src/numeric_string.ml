open Base

module T = struct
  type t = string [@@deriving sexp]

  (* The natural way to do this would be to split the input list into a list of chunks and
     then recurse down the list. However, the desire to avoid allocation means we have to
     take a more "direct" approach. *)

  let count_chars t ~from ~f =
    let rec count t f i =
      if i = String.length t || not (f t.[i]) then i else count t f (i + 1)
    in
    count t f from - from
  ;;

  let count_zeroes t ~from = count_chars t ~from ~f:(fun c -> Char.equal c '0')
  let count_digits t ~from = count_chars t ~from ~f:Char.is_digit

  let rec compare_substring t1 start1 t2 start2 ~len =
    if len <= 0
    then 0
    else (
      match Char.compare t1.[start1] t2.[start2] with
      | 0 -> compare_substring t1 (start1 + 1) t2 (start2 + 1) ~len:(len - 1)
      | other -> other)
  ;;

  let compare_numbers t1 t2 ~from ~skip_leading_zeros =
    let t1_leading_zeroes = if skip_leading_zeros then count_zeroes t1 ~from else 0 in
    let t2_leading_zeroes = if skip_leading_zeros then count_zeroes t2 ~from else 0 in
    let t1_significant_digits = count_digits t1 ~from:(from + t1_leading_zeroes) in
    let t2_significant_digits = count_digits t2 ~from:(from + t2_leading_zeroes) in
    match Int.compare t1_significant_digits t2_significant_digits with
    | 0 ->
      (match
         compare_substring
           t1
           (from + t1_leading_zeroes)
           t2
           (from + t2_leading_zeroes)
           ~len:t1_significant_digits
       with
       (* break ties by looking at the number of leading_zeros *)
       | 0 -> Int.compare t1_leading_zeroes t2_leading_zeroes
       | result -> result)
    | result -> result
  ;;

  let compare t1 t2 =
    let rec loop t1 t2 i ~previous_chunk_type =
      (* if we reached the end of either string, one is a prefix of the other *)
      if i = String.length t1 || i = String.length t2
      then Int.compare (String.length t1) (String.length t2)
      else (
        let c1 = t1.[i] in
        let c2 = t2.[i] in
        match Char.compare c1 c2 with
        | 0 ->
          let this_chunk_type =
            if not (Char.is_digit c1)
            then `Not_a_number
            else if Char.(c1 = '0')
            then (
              match previous_chunk_type with
              | `Not_a_number | `Number `Leading_zeros -> `Number `Leading_zeros
              | `Number `Significant_digits -> `Number `Significant_digits)
            else `Number `Significant_digits
          in
          loop t1 t2 (i + 1) ~previous_chunk_type:this_chunk_type
        | char_compare ->
          let d1 = Char.is_digit c1 in
          let d2 = Char.is_digit c2 in
          (match d1, d2, previous_chunk_type with
           | false, false, _ ->
             (* two non-numeric chars, just use their comparison *)
             char_compare
           | true, true, _ ->
             (* two digits that are different, compare the numeric chunks as numbers,
                taking into account whether we need to skip any leading zeros or not *)
             let skip_leading_zeros =
               match previous_chunk_type with
               | `Not_a_number | `Number `Leading_zeros -> true
               | `Number `Significant_digits -> false
             in
             compare_numbers t1 t2 ~from:i ~skip_leading_zeros
           (* With one digit and one non-digit, the larger string is the one that is
              continuing the same chunk *)
           | false, true, `Not_a_number | true, false, `Number _ -> 1
           | false, true, `Number _ | true, false, `Not_a_number -> -1))
    in
    (* the choice to start each string with a possibly-empty non-numeric component is what
       makes the right [previous_chunk_type] here be [`Not_a_number]. *)
    loop t1 t2 0 ~previous_chunk_type:`Not_a_number
  ;;
end

include T
include Comparable.Make (T)
