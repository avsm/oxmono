(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* publicsuffix.ml - Public Suffix List implementation for OCaml

   This implements the PSL algorithm as specified at:
   https://publicsuffix.org/list/

   Algorithm summary:
   1. Match domain against all rules
   2. If no rules match, the prevailing rule is "*" (implicit wildcard)
   3. If more than one rule matches, exception rules take priority
   4. If no exception rule, the rule with the most labels wins
   5. If the prevailing rule is an exception, remove its leftmost label
   6. The public suffix is the labels matching the prevailing rule
   7. The registrable domain is the public suffix plus one additional label
*)

(* Use types from generated data *)
type section = Publicsuffix_data.section = ICANN | Private

(* Bring the trie_node type and its fields into scope *)
open Publicsuffix_data

type t = { root : trie_node }

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
  | Domain_is_public_suffix ->
      Format.fprintf fmt "Domain is itself a public suffix"

let error_to_string err = Format.asprintf "%a" pp_error err
let create () = { root = Publicsuffix_data.get_root () }

(* Find a child node by label (case-insensitive) *)
let find_child (node : trie_node) label =
  let label_lower = String.lowercase_ascii label in
  List.find_opt
    (fun (l, _) -> String.lowercase_ascii l = label_lower)
    node.children
  |> Option.map snd

type match_result = {
  matched_labels : int; (* Number of labels matched *)
  section : section; (* Section of the rule *)
  is_exception : bool; (* Whether this is an exception rule *)
}
(** Result of matching a domain against the trie *)

(** Find all matching rules for a domain. Labels should be in reverse order (TLD
    first). *)
let find_matches (root : trie_node) labels =
  let matches = ref [] in

  (* Track whether we matched the implicit * rule *)
  let implicit_match =
    {
      matched_labels = 1;
      section = ICANN;
      (* Implicit rule is considered ICANN *)
      is_exception = false;
    }
  in

  let rec traverse (node : trie_node) depth remaining_labels =
    (* Check if current node has a rule *)
    Option.iter
      (fun (rt, sec) ->
        let m =
          {
            matched_labels = depth;
            section = sec;
            is_exception = rt = Exception;
          }
        in
        matches := m :: !matches)
      node.rule;

    (* Continue traversing if we have more labels *)
    match remaining_labels with
    | [] -> ()
    | label :: rest ->
        (* Check for wildcard match *)
        node.wildcard_child
        |> Option.iter (fun wc ->
            Option.iter
              (fun (rt, sec) ->
                let m =
                  {
                    matched_labels = depth + 1;
                    section = sec;
                    is_exception = rt = Exception;
                  }
                in
                matches := m :: !matches)
              wc.rule);

        (* Check for exact label match *)
        find_child node label
        |> Option.iter (fun child -> traverse child (depth + 1) rest)
  in

  traverse root 0 labels;

  (* If no matches, return the implicit * rule *)
  if !matches = [] then [ implicit_match ] else !matches

(** Select the prevailing rule from a list of matches. Per the algorithm: 1.
    Exception rules take priority 2. Otherwise, the rule with the most labels
    wins *)
let select_prevailing_rule matches =
  match List.find_opt (fun m -> m.is_exception) matches with
  | Some ex -> ex (* Exception rules take priority *)
  | None ->
      (* Find the rule with the most labels *)
      List.fold_left
        (fun best m ->
          if m.matched_labels > best.matched_labels then m else best)
        (List.hd matches) matches

(** Normalize a domain for lookup:
    - Convert to lowercase
    - Convert IDN to Punycode
    - Split into labels
    - Handle trailing dots *)
let normalize_domain domain =
  if domain = "" then Error Empty_domain
  else if String.length domain > 0 && domain.[0] = '.' then Error Leading_dot
  else
    (* Check for and preserve trailing dot *)
    let has_trailing_dot =
      String.length domain > 0 && domain.[String.length domain - 1] = '.'
    in
    let domain =
      if has_trailing_dot then String.sub domain 0 (String.length domain - 1)
      else domain
    in
    if domain = "" then Error Empty_domain
    else
      (* Convert IDN to ASCII (Punycode) *)
      match Punycode_idna.to_ascii domain with
      | Error e ->
          let msg = Format.asprintf "%a" Punycode_idna.pp_error e in
          Error (Punycode_error msg)
      | Ok ascii_domain ->
          (* Convert to lowercase and split into labels *)
          let ascii_lower = String.lowercase_ascii ascii_domain in
          let labels =
            String.split_on_char '.' ascii_lower
            |> List.filter (fun s -> s <> "")
          in
          if labels = [] then Error Empty_domain
          else Ok (labels, has_trailing_dot)

(** Convert labels back to a domain string *)
let labels_to_domain labels has_trailing_dot =
  let domain = String.concat "." labels in
  if has_trailing_dot then domain ^ "." else domain

(** Take the rightmost n elements from a list *)
let take_last n lst =
  let len = List.length lst in
  if len <= n then lst else List.filteri (fun i _ -> i >= len - n) lst

(** Calculate the number of public suffix labels from a prevailing rule *)
let suffix_label_count prevailing =
  if prevailing.is_exception then
    (* Exception rules: remove leftmost label from the rule *)
    prevailing.matched_labels - 1
  else prevailing.matched_labels

(** Find the prevailing rule for a domain *)
let find_prevailing_rule t labels =
  let rev_labels = List.rev labels in
  let matches = find_matches t.root rev_labels in
  select_prevailing_rule matches

let public_suffix_with_section t domain =
  match normalize_domain domain with
  | Error e -> Error e
  | Ok (labels, has_trailing_dot) ->
      let prevailing = find_prevailing_rule t labels in
      let count = suffix_label_count prevailing in
      if count > List.length labels then Error No_public_suffix
      else
        let suffix_labels = take_last count labels in
        let suffix = labels_to_domain suffix_labels has_trailing_dot in
        Ok (suffix, prevailing.section)

let public_suffix t domain =
  Result.map fst (public_suffix_with_section t domain)

let registrable_domain_with_section t domain =
  match normalize_domain domain with
  | Error e -> Error e
  | Ok (labels, has_trailing_dot) ->
      let prevailing = find_prevailing_rule t labels in
      let count = suffix_label_count prevailing in
      (* Registrable domain = suffix + 1 label *)
      let reg_label_count = count + 1 in
      if reg_label_count > List.length labels then Error Domain_is_public_suffix
      else
        let reg_labels = take_last reg_label_count labels in
        let reg_domain = labels_to_domain reg_labels has_trailing_dot in
        Ok (reg_domain, prevailing.section)

let registrable_domain t domain =
  Result.map fst (registrable_domain_with_section t domain)

let is_public_suffix t domain =
  match normalize_domain domain with
  | Error e -> Error e
  | Ok (labels, _) ->
      let prevailing = find_prevailing_rule t labels in
      let count = suffix_label_count prevailing in
      (* Domain is a public suffix if it has exactly suffix_label_count labels *)
      Ok (List.length labels = count)

let is_registrable_domain t domain =
  match normalize_domain domain with
  | Error e -> Error e
  | Ok (labels, _) ->
      let prevailing = find_prevailing_rule t labels in
      let count = suffix_label_count prevailing in
      let reg_label_count = count + 1 in
      (* Domain is registrable if it has exactly reg_label_count labels *)
      Ok (List.length labels = reg_label_count)

let rule_count _t = Publicsuffix_data.rule_count
let icann_rule_count _t = Publicsuffix_data.icann_rule_count
let private_rule_count _t = Publicsuffix_data.private_rule_count
let version _t = Publicsuffix_data.version
let commit _t = Publicsuffix_data.commit
