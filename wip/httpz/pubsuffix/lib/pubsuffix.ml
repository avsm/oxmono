type section = Pubsuffix_data.section =
  | ICANN
  | Private

open Pubsuffix_data

type error =
  | Empty_domain
  | Invalid_domain of string
  | Leading_dot
  | Punycode_error of string
  | Domain_is_public_suffix

let pp_error fmt = function
  | Empty_domain -> Format.fprintf fmt "Empty domain"
  | Invalid_domain s -> Format.fprintf fmt "Invalid domain: %s" s
  | Leading_dot -> Format.fprintf fmt "Domain has a leading dot"
  | Punycode_error s -> Format.fprintf fmt "Punycode conversion error: %s" s
  | Domain_is_public_suffix -> Format.fprintf fmt "Domain is itself a public suffix"
;;

let error_to_string err = Format.asprintf "%a" pp_error err
let ( let* ) = Result.bind

(* The generator emits every node's children in label order, so the root's
   roughly 1450 top-level labels cost a binary search rather than a walk. *)
let find_child children label =
  let rec search lo hi =
    if lo >= hi then Null
    else
      let mid = lo + (hi - lo) / 2 in
      let key, child = Stdlib_stable.Iarray.get children mid in
      match String.compare label key with
      | 0 -> This child
      | n when n < 0 -> search lo mid
      | _ -> search (mid + 1) hi
  in
  search 0 (Stdlib_stable.Iarray.length children)

(* Visit deeper matches last, preserving the previous tie-breaking order.
   Exceptions prevail over ordinary rules at every depth. *)
let find_prevailing (root : trie_node) labels =
  let rec traverse node depth remaining best_depth best_section exception_seen =
    let #(best_depth, best_section, exception_seen) =
      match node.rule with
      | Some (Exception, sec) -> #(depth, sec, true)
      | Some (_, sec) when not exception_seen && depth >= best_depth -> #(depth, sec, false)
      | _ -> #(best_depth, best_section, exception_seen)
    in
    match remaining with
    | [] -> #(best_depth, best_section, exception_seen)
    | label :: rest ->
      let #(best_depth, best_section) = match node.wildcard with
        | Some sec when not exception_seen && depth + 1 >= best_depth -> #(depth + 1, sec)
        | _ -> #(best_depth, best_section)
      in
      match find_child node.children label with
      | Null -> #(best_depth, best_section, exception_seen)
      | This child -> traverse child (depth + 1) rest best_depth best_section exception_seen
  in
  traverse root 0 labels 1 ICANN false
;;

let normalize_domain domain =
  if domain = ""
  then Error Empty_domain
  else if domain.[0] = '.'
  then Error Leading_dot
  else (
    let has_trailing_dot = domain.[String.length domain - 1] = '.' in
    let body =
      if has_trailing_dot then String.sub domain 0 (String.length domain - 1) else domain
    in
    if body = ""
    then Error Empty_domain
    else
      let* ascii_domain =
        try Ok (Punycode_idna.to_ascii ~use_std3_rules:true body) with
        | Punycode_idna.Error e ->
          let msg = Format.asprintf "%a" Punycode_idna.pp_error_reason e in
          Error (Punycode_error msg)
      in
      (* [to_ascii] restores a root dot of its own, so a doubled trailing dot
         survives the conversion and would split into an empty final label that
         every lookup below would then treat as a real one. *)
      let labels = String.split_on_char '.' (String.lowercase_ascii ascii_domain) in
      if List.exists (fun label -> label = "") labels
      then Error (Invalid_domain "empty label")
      else Ok (labels, has_trailing_dot))
;;

let labels_to_domain labels has_trailing_dot =
  let domain = String.concat "." labels in
  if has_trailing_dot then domain ^ "." else domain
;;

let take_last n lst =
  let len = List.length lst in
  let rec drop n = function [] -> [] | (_ :: tl as l) -> if n <= 0 then l else drop (n - 1) tl in
  drop (len - n) lst
;;

(* [lookup domain] is the normalized labels of [domain], whether it carried a trailing
   dot, the label count its prevailing rule treats as the public suffix, and the section
   that rule came from. *)
let lookup domain =
  let* labels, has_trailing_dot = normalize_domain domain in
  let #(depth, section, is_exception) =
    find_prevailing Pubsuffix_data.root (List.rev labels)
  in
  let count = if is_exception then depth - 1 else depth in
  Ok (labels, has_trailing_dot, count, section)
;;

let public_suffix_with_section domain =
  let* labels, has_trailing_dot, count, section = lookup domain in
  Ok (labels_to_domain (take_last count labels) has_trailing_dot, section)
;;

let public_suffix domain = Result.map fst (public_suffix_with_section domain)

let registrable_domain_with_section domain =
  let* labels, has_trailing_dot, count, section = lookup domain in
  let count = count + 1 in
  if count > List.length labels
  then Error Domain_is_public_suffix
  else Ok (labels_to_domain (take_last count labels) has_trailing_dot, section)
;;

let registrable_domain domain = Result.map fst (registrable_domain_with_section domain)

let is_public_suffix domain =
  let* labels, _, count, _ = lookup domain in
  Ok (List.length labels = count)
;;

let is_registrable_domain domain =
  let* labels, _, count, _ = lookup domain in
  Ok (List.length labels = count + 1)
;;

let rule_count = Pubsuffix_data.rule_count
let icann_rule_count = Pubsuffix_data.icann_rule_count
let private_rule_count = Pubsuffix_data.private_rule_count
let version = Pubsuffix_data.version
let commit = Pubsuffix_data.commit
