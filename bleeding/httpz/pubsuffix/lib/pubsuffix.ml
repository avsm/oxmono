type section = Pubsuffix_data.section =
  | ICANN
  | Private

open Pubsuffix_data

type error =
  | Empty_domain
  | Invalid_domain of string
  | Leading_dot
  | Punycode_error of string
  | No_public_suffix
  | Domain_is_public_suffix

let pp_error fmt = function
  | Empty_domain -> Format.fprintf fmt "Empty domain"
  | Invalid_domain s -> Format.fprintf fmt "Invalid domain: %s" s
  | Leading_dot -> Format.fprintf fmt "Domain has a leading dot"
  | Punycode_error s -> Format.fprintf fmt "Punycode conversion error: %s" s
  | No_public_suffix -> Format.fprintf fmt "No public suffix found"
  | Domain_is_public_suffix -> Format.fprintf fmt "Domain is itself a public suffix"
;;

let error_to_string err = Format.asprintf "%a" pp_error err
let ( let* ) = Result.bind

type match_result =
  { matched_labels : int
  ; section : section
  ; is_exception : bool
  }

let find_matches (root : trie_node) labels =
  let matches = ref [] in
  let implicit_match = { matched_labels = 1; section = ICANN; is_exception = false } in
  let rec traverse (node : trie_node) depth remaining_labels =
    Option.iter
      (fun (rt, sec) ->
        let m =
          { matched_labels = depth; section = sec; is_exception = rt = Exception }
        in
        matches := m :: !matches)
      node.rule;
    match remaining_labels with
    | [] -> ()
    | label :: rest ->
      Option.iter
        (fun sec ->
          let m = { matched_labels = depth + 1; section = sec; is_exception = false } in
          matches := m :: !matches)
        node.wildcard;
      List.assoc_opt label node.children
      |> Option.iter (fun child -> traverse child (depth + 1) rest)
  in
  traverse root 0 labels;
  if !matches = [] then [ implicit_match ] else !matches
;;

let select_prevailing_rule matches =
  match List.find_opt (fun m -> m.is_exception) matches with
  | Some ex -> ex
  | None ->
    List.fold_left
      (fun best m -> if m.matched_labels > best.matched_labels then m else best)
      (List.hd matches)
      matches
;;

let normalize_domain domain =
  if domain = ""
  then Error Empty_domain
  else if domain.[0] = '.'
  then Error Leading_dot
  else (
    let has_trailing_dot = domain.[String.length domain - 1] = '.' in
    let domain =
      if has_trailing_dot then String.sub domain 0 (String.length domain - 1) else domain
    in
    if domain = ""
    then Error Empty_domain
    else
      let* ascii_domain =
        try Ok (Punycode_idna.to_ascii ~use_std3_rules:true domain) with
        | Punycode_idna.Error e ->
          let msg = Format.asprintf "%a" Punycode_idna.pp_error_reason e in
          Error (Punycode_error msg)
      in
      let labels = String.split_on_char '.' (String.lowercase_ascii ascii_domain) in
      if labels = [] then Error Empty_domain else Ok (labels, has_trailing_dot))
;;

let labels_to_domain labels has_trailing_dot =
  let domain = String.concat "." labels in
  if has_trailing_dot then domain ^ "." else domain
;;

let take_last n lst =
  let len = List.length lst in
  if len <= n then lst else List.filteri (fun i _ -> i >= len - n) lst
;;

let suffix_label_count prevailing =
  if prevailing.is_exception
  then prevailing.matched_labels - 1
  else prevailing.matched_labels
;;

(* [lookup domain] is the normalized labels of [domain], whether it carried a trailing
   dot, the label count its prevailing rule treats as the public suffix, and the section
   that rule came from. *)
let lookup domain =
  let* labels, has_trailing_dot = normalize_domain domain in
  let prevailing =
    select_prevailing_rule (find_matches Pubsuffix_data.root (List.rev labels))
  in
  let count = suffix_label_count prevailing in
  Ok (labels, has_trailing_dot, count, prevailing.section)
;;

let public_suffix_with_section domain =
  let* labels, has_trailing_dot, count, section = lookup domain in
  if count > List.length labels
  then Error No_public_suffix
  else Ok (labels_to_domain (take_last count labels) has_trailing_dot, section)
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
