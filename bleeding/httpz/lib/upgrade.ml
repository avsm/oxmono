module Char_u = Stdlib_stable.Char_u

type protocol =
  #{ name_first : int
   ; name_last : int
   ; version_first : int
   ; version_last : int
   }

let invalid =
  #{ name_first = 0; name_last = 0; version_first = -1; version_last = -1 }
;;

let[@inline always] char_at (local_ s : string) i =
  Char_u.of_char (String.unsafe_get s i)
;;

let[@inline] parse_range (local_ value : string) first last =
  if first >= last
  then invalid
  else (
    let mutable slash = -1 in
    let mutable valid = true in
    let mutable i = first in
    while valid && i < last do
      if Char_u.equal (char_at value i) #'/'
      then if slash >= 0 then valid <- false else slash <- i;
      i <- i + 1
    done;
    if not valid
    then invalid
    else if slash < 0
            && Header.Syntax.is_token_sub value ~pos:first ~len:(last - first)
    then
      #{ name_first = first
       ; name_last = last
       ; version_first = -1
       ; version_last = -1
       }
    else if slash <= first
            || slash + 1 >= last
            || not (Header.Syntax.is_token_sub value ~pos:first ~len:(slash - first))
            || not
                 (Header.Syntax.is_token_sub
                    value
                    ~pos:(slash + 1)
                    ~len:(last - slash - 1))
    then invalid
    else
      #{ name_first = first
       ; name_last = slash
       ; version_first = slash + 1
       ; version_last = last
       })
;;

let[@inline] parsed p = p.#name_last > p.#name_first

let[@zero_alloc] valid_protocol (local_ value : string) =
  parsed (parse_range value 0 (String.length value))
;;

let[@inline] trim_left (local_ value : string) first last =
  let mutable i = first in
  while i < last && Buf_read.is_space (char_at value i) do
    i <- i + 1
  done;
  i
;;

let[@inline] trim_right (local_ value : string) first last =
  let mutable i = last in
  while i > first && Buf_read.is_space (char_at value (i - 1)) do
    i <- i - 1
  done;
  i
;;

let[@inline] comma (local_ value : string) first length =
  let mutable i = first in
  while i < length && not (Char_u.equal (char_at value i) #',') do
    i <- i + 1
  done;
  i
;;

let[@zero_alloc] valid_protocol_list (local_ value : string) =
  let length = String.length value in
  let mutable valid = true in
  let mutable count = 0 in
  let mutable first = 0 in
  while valid && first <= length do
    let comma = comma value first length in
    let left = trim_left value first comma in
    let right = trim_right value left comma in
    if left = right
    then valid <- false
    else (
      let protocol = parse_range value left right in
      if not (parsed protocol) then valid <- false else count <- count + 1);
    first <- (if comma = length then length + 1 else comma + 1)
  done;
  valid && count > 0
;;

let[@inline] equal_ascii_ci
    (local_ value : string) a_first a_last (local_ selected : string) b_first b_last =
  let length = a_last - a_first in
  if length <> b_last - b_first
  then false
  else (
    let mutable i = 0 in
    while
      i < length
      && Char_u.equal
           (Buf_read.to_lower (char_at value (a_first + i)))
           (Buf_read.to_lower (char_at selected (b_first + i)))
    do
      i <- i + 1
    done;
    i = length)
;;

let[@inline] equal_bytes
    (local_ value : string) a_first a_last (local_ selected : string) b_first b_last =
  let length = a_last - a_first in
  if length <> b_last - b_first
  then false
  else (
    let mutable i = 0 in
    while
      i < length
      && Char_u.equal (char_at value (a_first + i)) (char_at selected (b_first + i))
    do
      i <- i + 1
    done;
    i = length)
;;

let[@inline] same_protocol
    (local_ offer : string) offered (local_ selected : string) wanted =
  equal_ascii_ci
    offer
    offered.#name_first
    offered.#name_last
    selected
    wanted.#name_first
    wanted.#name_last
  && if offered.#version_first < 0
     then wanted.#version_first < 0
     else
       wanted.#version_first >= 0
       && equal_bytes
            offer
            offered.#version_first
            offered.#version_last
            selected
            wanted.#version_first
            wanted.#version_last
;;

let[@zero_alloc] matches_offer
    ~(offer : string @ local) ~(selected : string @ local) =
  let wanted = parse_range selected 0 (String.length selected) in
  if not (parsed wanted)
  then false
  else (
    let length = String.length offer in
    let mutable valid = true in
    let mutable count = 0 in
    let mutable found = false in
    let mutable first = 0 in
    while valid && first <= length do
      let comma = comma offer first length in
      let left = trim_left offer first comma in
      let right = trim_right offer left comma in
      if left <> right
      then (
        let offered = parse_range offer left right in
        if not (parsed offered)
        then valid <- false
        else (
          count <- count + 1;
          if same_protocol offer offered selected wanted then found <- true));
      first <- (if comma = length then length + 1 else comma + 1)
    done;
    valid && count > 0 && found)
;;
