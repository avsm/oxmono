(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Address book query filters.

    A filter selects address objects by the vCard properties they hold, the
    parameters of those properties and the text of either,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.5} RFC 6352
     Section 10.5}. A property name may carry a group prefix, in which case only
    that group matches, Section 10.5.1.

    @canonical Carddav.Filter *)

type test = [ `Anyof | `Allof ]
(** The type for how the conditions of a filter combine. [`Anyof] is a logical
    or and [`Allof] a logical and. *)

type match_type = [ `Equals | `Contains | `Starts_with | `Ends_with ]
(** The type for text match kinds,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.5.4} RFC 6352
     Section 10.5.4}. *)

type text_match = {
  text : string;  (** The text to match against. *)
  match_type : match_type;  (** Defaults to [`Contains]. *)
  collation : string option;
      (** The collation,
          {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-8.3} RFC 6352
           Section 8.3}. [None] is the server's default, [i;unicode-casemap]. *)
  negate : bool;  (** [true] matches when the text does not match. *)
}
(** The type for [CARDDAV:text-match] elements. *)

type param_filter = {
  param : string;  (** The parameter name, such as ["TYPE"]. *)
  param_test : [ `Defined | `Not_defined | `Match of text_match ];
      (** Whether [param] must be present, absent, or match. *)
}
(** The type for [CARDDAV:param-filter] elements,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.5.2} RFC 6352
     Section 10.5.2}. *)

type prop_filter = {
  prop : string;  (** The property name, such as ["EMAIL"] or ["item1.TEL"]. *)
  prop_condition :
    [ `Defined  (** The property exists. *)
    | `Not_defined  (** The property does not exist. *)
    | `Matches of test * text_match list * param_filter list
      (** The property exists and satisfies the matches and the parameter
          filters, combined by the test. *) ];
}
(** The type for [CARDDAV:prop-filter] elements,
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.5.1} RFC 6352
     Section 10.5.1}. The test that combines the matches and the parameter
    filters is held in [`Matches], the only condition that has any to combine.
*)

type t = { test : test; props : prop_filter list }
(** The type for [CARDDAV:filter] elements. An empty [props] matches every
    address object. *)

(** {1 Constructors} *)

val text_match :
  ?match_type:match_type ->
  ?collation:string ->
  ?negate:bool ->
  string ->
  text_match
(** [text_match ~match_type ~collation ~negate text] is a match for [text].
    [match_type] defaults to [`Contains] and [negate] to [false]. *)

val prop :
  ?test:test ->
  ?params:param_filter list ->
  string ->
  text_match list ->
  prop_filter
(** [prop ~test ~params name matches] selects the property [name] whose value
    satisfies [matches] and whose parameters satisfy [params], combined by
    [test]. With neither [matches] nor [params], it selects the property's
    presence and [test] is dropped. [test] defaults to [`Anyof]. *)

val prop_not_defined : string -> prop_filter
(** [prop_not_defined name] selects address objects without the property [name].
*)

val param : string -> text_match option -> param_filter
(** [param name m] selects the parameter [name], with the value matching [m]
    when given. *)

val param_not_defined : string -> param_filter
(** [param_not_defined name] selects properties without the parameter [name]. *)

val v : ?test:test -> prop_filter list -> t
(** [v ~test props] is the filter combining [props] by [test], which defaults to
    [`Anyof]. *)

val all : t
(** [all] matches every address object. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] combine the same property filters by
    the same test, in the same order. *)

(** {1 XML} *)

val to_xml : t -> Httpz_dav.element
(** [to_xml t] is [t] as a [CARDDAV:filter] element. Defaults are omitted. *)

val of_xml : Httpz_dav.element -> (t, string) result
(** [of_xml x] is the filter [x] holds. *)

(** {1 Matching} *)

val matches : t -> Vcard.t -> bool
(** [matches t card] is [true] if [card] satisfies [t] under the rules of
    {{:https://www.rfc-editor.org/rfc/rfc6352.html#section-10.5} RFC 6352
     Section 10.5}, with [i;ascii-casemap] and [i;unicode-casemap] folding ASCII
    case and [i;octet] comparing bytes. A collation this library does not know
    compares bytes. A server instead answers such a request with the
    [CARDDAV:supported-collation] precondition, Section 8.3.1. It is what a test
    checks a server's answer against. *)
