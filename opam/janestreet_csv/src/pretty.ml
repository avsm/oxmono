open Core

module Pad_floats = struct
  type t =
    { digits_before_dot : int
    ; digits_after_and_including_dot : int
    }

  let empty = { digits_before_dot = 1; digits_after_and_including_dot = 0 }

  let width { digits_before_dot; digits_after_and_including_dot } =
    digits_before_dot + digits_after_and_including_dot
  ;;

  let of_string x =
    match String.index x '.' with
    | None -> { digits_before_dot = String.length x; digits_after_and_including_dot = 0 }
    | Some digits_before_dot ->
      { digits_before_dot
      ; digits_after_and_including_dot = String.length x - digits_before_dot
      }
  ;;

  let max a b =
    { digits_before_dot = Int.max a.digits_before_dot b.digits_before_dot
    ; digits_after_and_including_dot =
        Int.max a.digits_after_and_including_dot b.digits_after_and_including_dot
    }
  ;;

  let max_length = List.fold ~init:empty ~f:(fun acc string -> max acc (of_string string))

  let pad desired string =
    let actual = of_string string in
    let spaces n = String.make n ' ' in
    let left_padding = spaces (desired.digits_before_dot - actual.digits_before_dot) in
    let right_padding =
      spaces
        (desired.digits_after_and_including_dot - actual.digits_after_and_including_dot)
    in
    String.concat [ left_padding; string; right_padding ]
  ;;
end

let max_length xs = List.fold xs ~init:1 ~f:(fun w x -> Int.max w (String.length x))

let string_padding x w =
  let w = Int.max 1 w in
  let n = String.length x in
  if w > n then Some (String.make (w - n) ' ') else None
;;

let pad_right x ~width =
  match string_padding x width with
  | None -> x
  | Some pad -> x ^ pad
;;

let matches of_string x =
  match of_string x with
  | _ -> true
  | exception _ -> false
;;

let may_be_numeric = function
  | "" -> true
  | n ->
    let n = String.chop_prefix_if_exists ~prefix:"$" n in
    matches Int.of_string n || matches Float.of_string n
;;

let col_type col = if List.for_all col ~f:may_be_numeric then `Number else `String

type t =
  { header_lines : string list
  ; row_lines : string list
  }

let lines t = t.header_lines @ t.row_lines

let print t =
  List.iter ~f:print_endline t.header_lines;
  List.iter ~f:print_endline t.row_lines
;;

let prettify_internal ~space ~suppress_header csv =
  let sep_width = space in
  let sep = String.make space ' ' in
  let header_block columns =
    List.rev
      (let hd, tl =
         List.fold_right
           columns
           ~init:("", [])
           ~f:(fun (header, col_type) (base_row, header_rows) ->
             let bar_place, column_width =
               match col_type with
               | `String width -> 0, width
               | `Number (pad_float : Pad_floats.t) ->
                 pad_float.digits_before_dot - 1, Pad_floats.width pad_float
             in
             let header_width = String.length header in
             let header_offset = Int.max 0 (bar_place + 1 - header_width) in
             let pad = String.make column_width ' ' in
             let bar_pad =
               String.mapi pad ~f:(fun i c -> if i = bar_place then '|' else c)
             in
             let rec add = function
               | [] -> [ String.make header_offset ' ' ^ header, 0 ]
               | (text, num_blanks) :: rest ->
                 if column_width + num_blanks >= header_width
                 then (
                   let pad text = pad ^ sep ^ text in
                   let text = Bytes.of_string (pad text) in
                   Bytes.From_string.blit
                     ~src:header
                     ~src_pos:0
                     ~dst:text
                     ~dst_pos:header_offset
                     ~len:header_width;
                   (Bytes.to_string text, 0)
                   :: List.map rest ~f:(fun (text, num_blanks) ->
                     pad text, column_width + sep_width + num_blanks))
                 else (bar_pad ^ sep ^ text, 0) :: add rest
             in
             let base_row =
               if String.equal base_row ""
               then String.rstrip bar_pad
               else bar_pad ^ sep ^ base_row
             in
             base_row, add header_rows)
       in
       hd :: List.map ~f:fst tl)
  in
  match csv |> Csv_shape.create_verbose |> Csv_shape.to_error_string with
  | Error csv -> Error csv
  | Ok () ->
    let cols =
      csv
      |> List.transpose_exn
      |> List.map ~f:(function
        | [] -> assert false
        | header :: values ->
          let col_type = col_type values in
          let width, pad =
            match col_type with
            | `String ->
              let width = max_length values in
              `String width, pad_right ~width
            | `Number ->
              let pad_floats = Pad_floats.max_length values in
              `Number pad_floats, Pad_floats.pad pad_floats
          in
          let header_info = header, width in
          header_info, List.map values ~f:pad)
    in
    let header_lines =
      if suppress_header then [] else header_block (List.map cols ~f:fst)
    in
    let row_lines =
      List.map ~f:(String.concat ~sep) (List.transpose_exn (List.map cols ~f:snd))
    in
    Ok { header_lines; row_lines }
;;

(* bootstrap *)
let prettify ~space ~suppress_header csv =
  match prettify_internal ~space ~suppress_header csv with
  | Ok result -> Ok result
  | Error error_csv ->
    let error_csv_lines =
      match prettify_internal ~space:2 ~suppress_header:false error_csv with
      | Ok t -> lines t
      | Error _ -> List.map error_csv ~f:(fun row -> String.concat ~sep:"," row)
    in
    Error
      (String.concat
         ~sep:"\n"
         ("Error: lines with different numbers of columns" :: "" :: error_csv_lines))
;;
