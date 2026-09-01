(* (c) 2017 Hannes Mehnert, all rights reserved *)

module Iarray = Stdlib_stable.Iarray

type 'a s : immutable_data = string iarray

let root : _ @ portable = [::]

let[@inline always] is_letter : _ @ portable = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let[@inline always] is_ldh : _ @ portable = function
  | '0'..'9' | 'a'..'z' | 'A'..'Z' | '-' -> true
  | _ -> false

(* from OCaml 4.13 bytes.ml *)
let for_all : _ @ portable = fun p s ->
  let n = String.length s in
  let rec loop i =
    if i = n then true
    else if p (String.unsafe_get s i) then loop (succ i)
    else false in
  loop 0

let exists : _ @ portable = fun p s ->
  let n = String.length s in
  let rec loop i =
    if i = n then false
    else if p (String.unsafe_get s i) then true
    else loop (succ i) in
  loop 0

let[@inline always] check_host_label : _ @ portable = fun s ->
  String.get s 0 <> '-' && (* leading may not be '-' *)
  String.get s (String.length s - 1) <> '-' && (* trailing may not be '-' *)
  for_all is_ldh s (* only LDH (letters, digits, hyphen)! *)

let host_exn : _ @ portable = fun t ->
  (* TLD should not be all-numeric! *)
  if
    (if Iarray.length t > 0 then
       exists is_letter (Iarray.get t 0)
     else true) &&
    Iarray.for_all check_host_label t
  then
    t
  else
    invalid_arg "invalid host name"

let host : _ @ portable = fun t ->
  try Ok (host_exn t) with
  | Invalid_argument e -> Error (`Msg e)

let check_service_label s =
  if String.length s > 0 && String.unsafe_get s 0 = '_' then
    let srv = String.sub s 1 (String.length s - 1) in
    let slen = String.length srv in
    (* service label: 1-15 characters; LDH; hyphen _not_ at begin nor end; no hyphen following a hyphen *)
    slen > 0 && slen <= 15 &&
    for_all is_ldh srv &&
    String.unsafe_get srv 0 <> '-' &&
    String.unsafe_get srv (slen - 1) <> '-' &&
    List.for_all (fun l -> l <> "")
      (String.split_on_char '-' srv)
  else
    false

let [@inline always] is_proto s =
  s = "_tcp" || s = "_udp" || s = "_sctp"

let[@inline always] check_label_length : _ @ portable = fun s ->
  let l = String.length s in
  l < 64 && l > 0

let[@inline always] check_total_length : _ @ portable = fun t ->
  Iarray.fold_left (fun acc s -> acc + 1 + String.length s) 1 t <= 255

let service_exn t =
  let l = Iarray.length t in
  if
    if l > 2 then
      let name = Iarray.sub t ~pos:0 ~len:(l - 2) in
      check_service_label (Iarray.get t (l - 1)) &&
      is_proto (Iarray.get t (l - 2)) &&
      Iarray.for_all check_label_length name &&
      check_total_length t &&
      match host name with Ok _ -> true | Error _ -> false
    else
      false
  then
    t
  else
    invalid_arg "invalid service name"

let service t =
  try Ok (service_exn t) with
  | Invalid_argument e -> Error (`Msg e)

let raw t = t

let[@inline always] check : _ @ portable = fun t ->
  Iarray.for_all check_label_length t &&
  check_total_length t

let get_label_exn : _ @ portable = fun ?(rev = false) xs idx ->
  let idx' = if rev then idx else pred (Iarray.length xs) - idx in
  try Iarray.get xs idx' with
  | Invalid_argument _ -> invalid_arg "bad index for domain name"

let get_label : _ @ portable = fun ?rev xs idx ->
  try Ok (get_label_exn ?rev xs idx) with
  | Invalid_argument e -> Error (`Msg e)

let find_label_exn ?(rev = false) xs p =
  let l = pred (Iarray.length xs) in
  let check x = x >= 0 && x <= l in
  let rec go next idx =
    if check idx then
      if p (Iarray.get xs idx) then
        idx
      else
        go next (next idx)
    else
      invalid_arg "label not found"
  in
  let next, start = if rev then (succ, 0) else (pred, l) in
  let r = go next start in
  l - r

let find_label ?rev xs p =
  try Some (find_label_exn ?rev xs p) with
  | Invalid_argument _ -> None

let count_labels xs = Iarray.length xs

let prepend_label_exn xs lbl =
  let n = Iarray.of_list [ lbl ] in
  let n = Iarray.append xs n in
  if check_label_length lbl && check_total_length n then n
  else invalid_arg "invalid domain name"

let prepend_label xs lbl =
  try Ok (prepend_label_exn xs lbl) with
  | Invalid_argument e -> Error (`Msg e)

let drop_label_exn : _ @ portable =
 fun ?(rev = false) ?(amount = 1) t ->
  let len = Iarray.length t - amount
  and start = if rev then amount else 0
  in
  Iarray.sub t ~pos:start ~len

let drop_label : _ @ portable = fun ?rev ?amount t ->
  try Ok (drop_label_exn ?rev ?amount t) with
  | Invalid_argument _ -> Error (`Msg "couldn't drop labels")

let append_exn pre post =
  let r = Iarray.append post pre in
  if check_total_length r then r else invalid_arg "invalid domain name"

let append pre post =
  try Ok (append_exn pre post) with
  | Invalid_argument _ -> Error (`Msg "couldn't concatenate domain names")

let of_strings_exn : _ @ portable = fun xs ->
  let labels =
    (* we support both example.com. and example.com *)
    match List.rev xs with
    | ""::rst -> rst
    | rst -> rst
  in
  let t = Iarray.of_list labels in
  if check t then t
  else invalid_arg "invalid domain name"

let of_strings xs =
  try Ok (of_strings_exn xs) with
  | Invalid_argument e -> Error (`Msg e)

let of_string_exn : _ @ portable = function
  | "." -> root
  | s -> of_strings_exn (String.split_on_char '.' s)

let of_string : _ @ portable = fun s ->
  try Ok (of_string_exn s) with
  | Invalid_argument e -> Error (`Msg e)

let of_array = Iarray.of_array

let to_array = Iarray.to_array

let to_strings : _ @ portable = fun ?(trailing = false) dn ->
  let labels = Iarray.to_list dn in
  List.rev (if trailing then "" :: labels else labels)

let to_string : _ @ portable = fun ?trailing dn ->
  match to_strings ?trailing dn with
  | [""] -> "."
  | labels -> String.concat "." labels

let canonical : _ @ portable = fun t ->
  let str = to_string t in
  of_string_exn (String.lowercase_ascii str)

let pp ppf xs = Format.pp_print_string ppf (to_string xs)

let compare_label : _ @ portable = fun a b ->
  String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)

let compare_domain : _ @ portable = fun cmp_sub a b ->
  let al = Iarray.length a and bl = Iarray.length b in
  let rec cmp idx =
    if al = bl && al = idx then 0
    else if al = idx then -1
    else if bl = idx then 1
    else
      match cmp_sub (Iarray.get a idx) (Iarray.get b idx) with
      | 0 -> cmp (succ idx)
      | x -> x
  in
  cmp 0

let compare : _ @ portable = compare_domain compare_label

let equal_label : _ @ portable = fun ?(case_sensitive = false) a b ->
  let cmp = if case_sensitive then String.compare else compare_label in
  cmp a b = 0

let equal : _ @ portable = fun ?(case_sensitive = false) a b ->
  let cmp = if case_sensitive then String.compare else compare_label in
  compare_domain cmp a b = 0

let is_subdomain : _ @ portable = fun ~subdomain ~domain ->
  let supl = Iarray.length domain in
  let rec cmp idx =
    if idx = supl then
      true
    else
      compare_label (Iarray.get domain idx) (Iarray.get subdomain idx) = 0 &&
      cmp (succ idx)
  in
  if Iarray.length subdomain < supl then
    false
  else
    cmp 0

module Ordered = struct
  type t = [ `raw ] s
  let compare = compare_domain compare_label
end

module Host_ordered = struct
  type t = [ `host ] s
  let compare = compare_domain compare_label
end

module Service_ordered = struct
  type t = [ `service ] s
  let compare = compare_domain compare_label
end

type 'a t : immutable_data = 'a s

module Host_map = struct
  include Map.MakePortable(Host_ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Host_set = Set.MakePortable(Host_ordered)

module Service_map = struct
  include Map.MakePortable(Service_ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Service_set = Set.MakePortable(Service_ordered)

module Map = struct
  include Map.MakePortable(Ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Set = Set.MakePortable(Ordered)
