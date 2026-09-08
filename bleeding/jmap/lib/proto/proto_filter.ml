(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type operator = [ `And | `Or | `Not ]

let operator_assoc : (string * operator) list = [ ("AND", `And); ("OR", `Or); ("NOT", `Not) ]
let operator_jsont = Jsont.enum ~kind:"FilterOperator" operator_assoc

type 'condition filter_operator = {
  operator : operator;
  conditions : 'condition filter list;
}

and 'condition filter =
  | Operator of 'condition filter_operator
  | Condition of 'condition

(* RFC 8620 Section 5.5: "a FilterCondition object MUST NOT have an
   'operator' property", so the presence of that member decides between the
   two shapes and the case member of the object codec can carry it. *)
type tag = [ operator | `Condition ]

let tag_jsont : tag Jsont.t =
  Jsont.enum ~kind:"FilterOperator"
    (List.map (fun (s, op) -> (s, (op :> tag))) operator_assoc)

let tag_to_string = function
  | `And -> "AND"
  | `Or -> "OR"
  | `Not -> "NOT"
  | `Condition -> "a condition"

let filter_jsont (type c) (condition_jsont : c Jsont.t) : c filter Jsont.t =
  let self =
    Jsont.Portable_lazy.from_fun_fixed (fun self ->
      let conditions_jsont =
         Jsont.Object.map ~kind:"FilterOperator" Fun.id
         |> Jsont.Object.mem "conditions"
              (Jsont.list (Jsont.rec' self))
              ~enc:Fun.id
         |> Jsont.Object.finish
       in
       let operator_case op =
         Jsont.Object.Case.map
           ~dec:(fun conditions -> Operator { operator = op; conditions })
           (op :> tag)
           conditions_jsont
       in
       let and_case = operator_case `And in
       let or_case = operator_case `Or in
       let not_case = operator_case `Not in
       let condition_case =
         Jsont.Object.Case.map
           ~dec:(fun c -> Condition c)
           (`Condition : tag) condition_jsont
       in
       let enc_case = function
         | Operator o ->
             let case =
               match o.operator with
               | `And -> and_case
               | `Or -> or_case
               | `Not -> not_case
             in
             Jsont.Object.Case.value case o.conditions
         | Condition c -> Jsont.Object.Case.value condition_case c
       in
       let cases =
         Jsont.Object.Case.
           [ make and_case; make or_case; make not_case; make condition_case ]
       in
       Jsont.Object.map ~kind:"Filter" Fun.id
       |> Jsont.Object.case_mem "operator" tag_jsont cases ~tag_to_string
            ~dec_absent:`Condition ~enc:Fun.id ~enc_case ~enc_omit:(fun tag ->
              tag = `Condition)
       |> Jsont.Object.finish)
  in
  Jsont.Portable_lazy.force self

let operator op conditions = Operator { operator = op; conditions }
let and_ l = operator `And l
let or_ l = operator `Or l
let not_ l = operator `Not l

type comparator = {
  property : string;
  is_ascending : bool;
  collation : string option;
  keyword : string option;
  unknown : Proto_unknown.t;
}

let comparator ?(is_ascending = true) ?collation ?keyword
    ?(unknown = Proto_unknown.empty) property =
  { property; is_ascending; collation; keyword; unknown }

let comparator_make property is_ascending collation keyword unknown =
  { property; is_ascending; collation; keyword; unknown }

let comparator_jsont =
  let kind = "Comparator" in
  Jsont.Object.map ~kind comparator_make
  |> Jsont.Object.mem "property" Jsont.string ~enc:(fun c -> c.property)
  |> Jsont.Object.mem "isAscending" Jsont.bool ~dec_absent:(fun () -> true)
       ~enc:(fun c -> c.is_ascending)
       ~enc_omit:(fun b -> b)
  |> Jsont.Object.opt_mem "collation" Jsont.string ~enc:(fun c -> c.collation)
  |> Jsont.Object.opt_mem "keyword" Jsont.string ~enc:(fun c -> c.keyword)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun c -> c.unknown)
  |> Jsont.Object.finish

type added_item = { id : Proto_id.t; index : int64 }

let added_item_make id index = { id; index }

let added_item_jsont =
  let kind = "AddedItem" in
  Jsont.Object.map ~kind added_item_make
  |> Jsont.Object.mem "id" Proto_id.jsont ~enc:(fun a -> a.id)
  |> Jsont.Object.mem "index" Proto_int53.Unsigned.jsont ~enc:(fun a -> a.index)
  |> Jsont.Object.finish
