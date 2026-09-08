@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Query filters, sorts and query changes.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.5} RFC 8620
     Section 5.5} defines the [filter] and [sort] arguments of a [/query] call.
    Section 5.6 defines the [AddedItem] of a [/queryChanges] response.

    @canonical Jmap.Proto.Filter *)

(** {1 Filters} *)

type operator =
  [ `And  (** Every condition must match. *)
  | `Or  (** At least one condition must match. *)
  | `Not
    (** No condition must match. RFC 8620 Section 5.5 makes this a NOR over the
        whole condition list, not the negation of a single condition. *) ]
(** The type for filter operators. *)

val operator_jsont : operator Jsont.t
(** [operator_jsont] is the codec for a filter operator. It maps [`And], [`Or]
    and [`Not] to ["AND"], ["OR"] and ["NOT"], and errors on any other string.
*)

type 'condition filter_operator = {
  operator : operator;  (** The operator combining the conditions. *)
  conditions : 'condition filter list;
      (** The filters the operator combines. The list may be empty. *)
}
(** The type for a FilterOperator over conditions of type ['condition]. *)

(** The type for a filter over conditions of type ['condition]. *)
and 'condition filter =
  | Operator of 'condition filter_operator
  | Condition of 'condition
      (** A FilterCondition, whose shape belongs to the data type being queried.
      *)

val filter_jsont : 'c Jsont.t -> 'c filter Jsont.t
(** [filter_jsont condition] is the codec for a filter whose conditions are
    coded by [condition]. An object with an [operator] member is a
    FilterOperator and any other object is a FilterCondition. RFC 8620 Section
    5.5 makes that test sound, since "a FilterCondition object MUST NOT have an
    [operator] property".

    @raise Invalid_argument
      if [condition] is not an object codec, or if it defines a member named
      [operator]. *)

val and_ : 'c filter list -> 'c filter
(** [and_ l] is the filter matching what every filter of [l] matches. It is the
    [AND] FilterOperator over [l], and matches everything when [l] is empty. *)

val or_ : 'c filter list -> 'c filter
(** [or_ l] is the filter matching what at least one filter of [l] matches. It
    is the [OR] FilterOperator over [l], and matches nothing when [l] is empty.
*)

val not_ : 'c filter list -> 'c filter
(** [not_ l] is the filter matching what no filter of [l] matches. RFC 8620
    Section 5.5 makes [NOT] a NOR over the whole list rather than the negation
    of one filter, so [not_ [ f ]] is the negation of [f] and [not_ [ f; g ]]
    matches only what neither matches. *)

(** {1 Comparators} *)

type comparator = {
  property : string;  (** The property to sort by. *)
  is_ascending : bool;
      (** [true] to sort in ascending order, [false] in descending order. *)
  collation : string option;
      (** The collation algorithm to compare strings with. [None] leaves the
          choice to the server. *)
  keyword : string option;
      (** The keyword to sort on. RFC 8621 Section 4.4.2 requires it for the
          ["hasKeyword"], ["allInThreadHaveKeyword"] and
          ["someInThreadHaveKeyword"] sorts of [Email/query]. *)
  unknown : Proto_unknown.t;
      (** The members not defined above, kept verbatim. RFC 8620 Section 5.5
          lets the data type of the query define further Comparator properties,
          of which {!field-keyword} is one. *)
}
(** The type for comparators. *)

val comparator :
  ?is_ascending:bool ->
  ?collation:string ->
  ?keyword:string ->
  ?unknown:Proto_unknown.t ->
  string ->
  comparator
(** [comparator property] is the comparator sorting on [property].
    [is_ascending] defaults to [true]. [collation] and [keyword] are absent
    unless given. [unknown] defaults to {!Jmap.Proto.Unknown.empty}. *)

val comparator_jsont : comparator Jsont.t
(** [comparator_jsont] is the codec for a comparator. An absent [isAscending]
    decodes to [true], and [true] is omitted on encode. *)

(** {1 Query changes} *)

type added_item = {
  id : Proto_id.t;  (** The id of the item added to the results. *)
  index : int64;
      (** The index the item now occupies in the full sorted result list. *)
}
(** The type for the [AddedItem] of a [/queryChanges] response, defined by RFC
    8620 Section 5.6. *)

val added_item_jsont : added_item Jsont.t
(** [added_item_jsont] is the codec for an [AddedItem]. *)
