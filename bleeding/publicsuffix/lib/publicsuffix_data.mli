(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Auto-generated Public Suffix List data

    This module contains the parsed and compiled Public Suffix List (PSL) data
    as OCaml data structures. The data is generated at build time from the
    official PSL file by the [gen_psl] code generator.

    {1 Public Suffix List Specification}

    The Public Suffix List is maintained by Mozilla and follows the
    specification at {{:https://publicsuffix.org/list/} publicsuffix.org}. The
    list provides an accurate database of domain name suffixes under which
    Internet users can directly register names.

    {2 PSL Format and Rules}

    The PSL defines three types of rules:

    - {b Normal rules}: Standard domain suffixes (e.g., [com], [co.uk]). These
      match exactly as written.

    - {b Wildcard rules}: Prefixed with [*.] (e.g., [*.jp]). These match any
      single label in that position. For example, [*.example.com] matches
      [foo.example.com] but not [example.com] or [bar.foo.example.com].

    - {b Exception rules}: Prefixed with [!] (e.g., [!city.kobe.jp]). These
      override wildcard rules and specify that a particular domain {i is not} a
      public suffix despite a matching wildcard.

    {2 Sections}

    The PSL is divided into two sections:

    - {b ICANN section}: Contains domains delegated by ICANN or present in the
      IANA root zone database. These are official top-level domains and their
      subdivisions (e.g., [com], [co.uk], [k12.ak.us]).

    - {b Private section}: Contains domains submitted by private organizations
      for services that allow subdomain registration (e.g., [blogspot.com],
      [github.io], [herokuapp.com]). Applications may choose to treat these
      differently from ICANN domains.

    {2 Matching Algorithm}

    Per the PSL specification, the matching algorithm:

    1. Matches the domain against all rules in the list 2. If no rules match,
    applies the implicit [*] wildcard rule 3. If multiple rules match, exception
    rules take priority 4. Otherwise, the rule with the most labels wins 5. For
    exception rules, the public suffix is derived by removing the exception's
    leftmost label

    {1 Data Structure}

    This module represents the PSL as a trie (prefix tree) data structure for
    efficient lookup. The trie is constructed with labels in reverse order (TLD
    first), allowing efficient traversal from the top-level domain down to more
    specific labels.

    All domain labels in the trie are:
    - Converted to lowercase
    - Encoded as Punycode for internationalized domain names
    - Stored as UTF-8 strings

    {1 Build-Time Generation}

    This module is automatically generated during the build process:

    1. The [gen_psl.ml] code generator reads [public_suffix_list.dat] 2. It
    parses each rule according to the PSL specification 3. It constructs an
    in-memory trie from all rules 4. It emits OCaml source code representing the
    trie 5. The generated code is compiled into the library

    This approach embeds the entire PSL into the compiled library, requiring no
    runtime file I/O or parsing.

    {1 Interface}

    This module is internal to the library. The main library API is exposed
    through the {!Publicsuffix} module, which provides high-level functions for
    querying the PSL data. *)

(** {1 Types} *)

(** Section of the PSL where a rule originates.

    The PSL is divided into two sections with different governance:
    - [ICANN]: Official domains delegated by ICANN or in the IANA root zone
    - [Private]: Domains submitted by private parties for their services *)
type section = ICANN | Private

(** Rule types defined in the PSL specification.

    - [Normal]: A standard domain suffix that matches exactly (e.g., [com])
    - [Wildcard]: A rule with [*.] prefix that matches any single label
    - [Exception]: A rule with [!] prefix that overrides wildcard matches *)
type rule_type = Normal | Wildcard | Exception

type trie_node = {
  rule : (rule_type * section) option;
  children : (string * trie_node) list;
  wildcard_child : trie_node option;
}
(** A node in the suffix trie.

    The trie is constructed with domain labels in reverse order (TLD first). For
    example, the domain [example.co.uk] would be traversed as [uk] -> [co] ->
    [example].

    - [rule]: If [Some (rt, sec)], this node represents a PSL rule of type [rt]
      from section [sec]
    - [children]: List of (label, child_node) pairs for exact label matches
    - [wildcard_child]: If [Some node], this represents a wildcard match ([*])
      at this position in the domain hierarchy *)

(** {1 Data Access} *)

val get_root : unit -> trie_node
(** Get the root of the suffix trie.

    The root node represents the starting point for all PSL lookups. Domain
    labels should be traversed in reverse order (TLD first) from this root.

    @return The root trie node containing all PSL rules *)

(** {1 Statistics}

    These values reflect the PSL data at the time this module was generated.
    They include all rules from both the ICANN and Private sections. *)

val rule_count : int
(** Total number of rules in the embedded PSL data.

    This includes all Normal, Wildcard, and Exception rules from both sections.
*)

val icann_rule_count : int
(** Number of rules in the ICANN section.

    These are official TLD rules delegated by ICANN or present in the IANA root
    zone database. *)

val private_rule_count : int
(** Number of rules in the Private section.

    These are rules submitted by private organizations for services that allow
    subdomain registration. *)

(** {1 Version Information}

    These values reflect the version and commit information from the PSL data at
    the time this module was generated. *)

val version : string
(** Version string from the PSL data file.

    This is the version identifier from the Public Suffix List source file,
    typically in the format "YYYY-MM-DD_HH-MM-SS_UTC". *)

val commit : string
(** Commit hash from the PSL data file.

    This is the git commit hash from the Public Suffix List repository
    corresponding to the version of the data embedded in this library. *)
