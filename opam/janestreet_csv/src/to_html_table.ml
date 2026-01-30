open Core
open Csv_common

let convert_attrs string_attrs =
  List.map string_attrs ~f:(fun (name, value) ->
    Tyxml.Html.Unsafe.string_attrib name (Option.value ~default:"" value))
;;

let run
  ?separator
  ~no_header
  ~suppress_header
  ~table_attrs
  ~th_attrs
  ~tr_attrs
  ~td_attrs
  ~border
  ~unescaped_html
  file
  =
  Or_file.with_all file ?separator ~no_header:false ~f:(fun csv ->
    let table_attrs =
      convert_attrs (("border", Some (if border then "1" else "0")) :: table_attrs)
    in
    let tr_attrs = convert_attrs tr_attrs in
    let field_to_html =
      if unescaped_html then Tyxml.Html.Unsafe.data else Tyxml.Html.txt
    in
    let header =
      if no_header || suppress_header
      then None
      else (
        let th_attrs = convert_attrs th_attrs in
        Some
          (Tyxml.Html.tr
             ~a:tr_attrs
             (List.map csv.header ~f:(fun value ->
                Tyxml.Html.(th ~a:th_attrs [ field_to_html value ])))))
    in
    let rows =
      let td_attrs = convert_attrs td_attrs in
      (if no_header && not suppress_header then csv.header :: csv.lines else csv.lines)
      |> List.map ~f:(fun line ->
        Tyxml.Html.tr
          ~a:tr_attrs
          (List.map line ~f:(fun value ->
             Tyxml.Html.(td ~a:td_attrs [ field_to_html value ]))))
    in
    let document = Tyxml.Html.table ~a:table_attrs (Option.to_list header @ rows) in
    Format.printf "%a" (Tyxml.Html.pp_elt ()) document)
;;
