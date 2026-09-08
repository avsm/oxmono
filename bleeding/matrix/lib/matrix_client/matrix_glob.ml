(* Matrix push rules and server ACLs use the same ASCII glob language.  Keep
   the dynamic-programming matcher here so the two callers cannot drift. *)

let is_word_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

(* [ends pattern value start] is the set of indices [j] such that [pattern]
   matches [value.[start .. j-1]], as a boolean array of length
   [String.length value + 1]. The reachable set only moves left-to-right, so
   this is bounded by the product of pattern and value lengths. *)
let ends pattern value start =
  let n = String.length value in
  let current = Array.make (n + 1) false in
  current.(start) <- true;
  String.iter
    (fun pattern_char ->
      let next = Array.make (n + 1) false in
      (match pattern_char with
      | '*' ->
          let reachable = ref false in
          for value_index = 0 to n do
            if current.(value_index) then reachable := true;
            if !reachable then next.(value_index) <- true
          done
      | '?' ->
          for value_index = 0 to n - 1 do
            if current.(value_index) then next.(value_index + 1) <- true
          done
      | expected ->
          for value_index = 0 to n - 1 do
            if current.(value_index) && Char.equal value.[value_index] expected
            then next.(value_index + 1) <- true
          done);
      Array.blit next 0 current 0 (n + 1))
    pattern;
  current

let whole_string ~pattern value =
  let pattern = String.lowercase_ascii pattern in
  let value = String.lowercase_ascii value in
  (ends pattern value 0).(String.length value)

let word_boundary ~pattern value =
  let pattern = String.lowercase_ascii pattern in
  let value = String.lowercase_ascii value in
  if String.equal pattern value then true
  else if String.length pattern = 0 then false
  else
    let n = String.length value in
    let boundary_before i = i = 0 || not (is_word_char value.[i - 1]) in
    let boundary_after j = j = n || not (is_word_char value.[j]) in
    let rec scan start =
      if start > n then false
      else if not (boundary_before start) then scan (start + 1)
      else
        let matching_ends = ends pattern value start in
        let rec check finish =
          if finish > n then false
          else if
            matching_ends.(finish) && finish > start && boundary_after finish
          then true
          else check (finish + 1)
        in
        if check start then true else scan (start + 1)
    in
    scan 0
