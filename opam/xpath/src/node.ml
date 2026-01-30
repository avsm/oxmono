open! Core
module Xml = Simple_xml

(* We do not support comments or processing instructions *)
type t =
  | Root of Xml.element
  | Element of Xml.element
  | Attribute of Xml.Attribute.t
  | Namespace of
      { prefix : string option
      ; value : string
      }
  | Text of string
[@@deriving sexp_of, variants]

let find_prefix_definition ~namespace = function
  | Element element ->
    List.find_map element.attributes ~f:(fun { ns; key; value } ->
      match ns, key with
      | "", "xmlns" when String.equal value namespace -> Some `Default
      | "xmlns", _ when String.equal value namespace -> Some (`Prefixed key)
      | _ -> None)
  | _ -> None
;;

let lang = function
  | Element element ->
    List.find_map element.attributes ~f:(fun { ns; key; value } ->
      match ns, key with
      | "xml", "lang" -> Some value
      | _ -> None)
  | _ -> None
;;

let string_value = function
  | Text s -> s
  | Attribute { value; _ } -> value
  | Namespace { value; _ } -> value
  | Root element | Element element ->
    (match element.children with
     | [] -> ""
     | [ Text s ] -> s
     | _ ->
       let rec all_child_text_nodes (element : Xml.element) =
         let%bind.Sequence child = Sequence.of_list element.children in
         match child with
         | Text s -> Sequence.return s
         | Element element -> all_child_text_nodes element
       in
       let buffer = Buffer.create 128 in
       Sequence.iter (all_child_text_nodes element) ~f:(Buffer.add_string buffer);
       Buffer.contents buffer)
;;

let expanded_name node =
  match node with
  | Element { tag = { ns; tag }; _ } ->
    Some (Expanded_name.create ~namespace:ns ~name:tag)
  | Attribute { ns; key; _ } -> Some (Expanded_name.create ~namespace:ns ~name:key)
  | Namespace { prefix; value = _ } ->
    Some { Expanded_name.namespace = None; name = Option.value prefix ~default:"" }
  | _ -> None
;;

let to_string_hum ?ns_prefix = function
  | Root _ -> "Root"
  | Element element -> Xml.to_string ?ns_prefix (Element element)
  | Attribute attribute -> [%message "" (attribute : Xml.Attribute.t)] |> Sexp.to_string
  | Namespace { prefix; value } ->
    [%message "Namespace" (prefix : string option) (value : string)] |> Sexp.to_string
  | Text text -> [%sexp (text : string)] |> Sexp.to_string
;;
