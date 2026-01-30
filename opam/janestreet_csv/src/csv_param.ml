open! Core
module Time = Time_float_unix
include Csv_param_intf

module T = struct
  include Command.Param

  let file_stdin_anon = Csv_common.Or_file.anon
  let file_stdin_flag = Csv_common.Or_file.flag
  let files = anon (sequence ("filename" %: Filename_unix.arg_type))

  (* flags *)

  let reverse =
    flag "-reverse" no_arg ~doc:" reverse sorting order" ~aliases:[ "--reverse" ]
  ;;

  let field' ~aliases =
    flag "-field" (required string) ~doc:"<field> field name to sort on" ~aliases
  ;;

  let field = field' ~aliases:[ "--field" ]

  let time_field =
    flag
      "-time-field"
      (required string)
      ~doc:"<field> field with times for snapping to grid"
      ~aliases:[ "--time-field" ]
  ;;

  let start_time =
    flag
      "-start"
      (required Time.arg_type)
      ~doc:"<time> time to start grid"
      ~aliases:[ "--start" ]
  ;;

  let stop_time =
    flag
      "-stop"
      (required Time.arg_type)
      ~doc:"<time> time to stop grid"
      ~aliases:[ "--stop" ]
  ;;

  let grid_step =
    flag
      "-step"
      (required Time.Span.arg_type)
      ~doc:"<span> time span for grid step"
      ~aliases:[ "--step" ]
  ;;

  let max_width =
    flag "-limit-width-to" (optional int) ~doc:" maximum column width in output"
  ;;

  let prefer_split_on_spaces =
    flag_optional_with_default_doc_sexp
      "-prefer-split-on-spaces"
      bool
      [%sexp_of: bool]
      ~default:true
      ~doc:"BOOL prefer splitting lines on spaces rather than mid-word"
  ;;

  let regexp_arg = Command.Arg_type.create Re2.create_exn

  let regexp =
    flag
      "-regexp"
      (required regexp_arg)
      ~doc:"<regexp> regexp to search for (re2 style)"
      ~aliases:[ "--regexp" ]
  ;;

  let invert =
    flag
      "-invert-match"
      no_arg
      ~doc:" Invert the sense of matching, to select non-matching lines."
      ~aliases:[ "-v" ]
  ;;

  let skip_lines =
    flag
      "-skip-lines"
      (optional int)
      ~doc:"<nr-of-lines> drop this many lines from the beginning of the input"
      ~aliases:[ "--skip-lines" ]
  ;;

  let default_sep = ','
  let default_space = 2

  let sep_arg =
    Arg_type.create (fun s ->
      if String.( = ) s "\\t"
      then '\t'
      else if String.length s <> 1
      then failwithf "Delimiter <%s> not 1 char!" s ()
      else s.[0])
  ;;

  let sep =
    flag
      "-sep"
      ~aliases:[ "-d"; "-delim"; "--sep" ]
      (optional_with_default default_sep sep_arg)
      ~doc:(sprintf "CHAR csv separator (default: '%c')" default_sep)
  ;;

  let key_specifier =
    let open Command.Param in
    flag
      "k"
      (required string)
      ~doc:"SPEC comma-separated list of fields comprising the key"
  ;;

  let no_header =
    let open Command.Param in
    flag
      "-nh"
      no_arg
      ~doc:"  do not treat the first row as a header row"
      ~aliases:[ "-no-header"; "--no-header" ]
  ;;

  let no_headers_use_indices_instead =
    flag
      "-no-headers"
      ~doc:"treat every row as data; interpret [-fields] as zero-based indices"
      no_arg
  ;;

  let space =
    let open Command.Param in
    flag
      "-s"
      (optional_with_default default_space int)
      ~doc:
        (sprintf "NUM how far apart to space out columns (default: '%d')" default_space)
  ;;

  let suppress_header =
    let open Command.Param in
    flag
      "-sh"
      no_arg
      ~doc:" keep the header row from appearing in the output"
      ~aliases:[ "-suppress-header"; "--suppress-header" ]
  ;;

  let utf8 =
    let open Command.Param in
    flag "-utf8" no_arg ~doc:" emit the output with utf8 encoding"
  ;;

  let fields_gen ~doc =
    let open Command.Param in
    let arg_type =
      Arg_type.comma_separated ~strip_whitespace:true string ~allow_empty:true
    in
    flag ~aliases:[ "--fields" ] "-fields" (optional_with_default [] arg_type) ~doc
  ;;

  let fields = fields_gen ~doc:" named fields to extract, comma separated"

  let fields_backward_compat =
    let open Command.Param in
    let arg_type =
      Arg_type.comma_separated ~strip_whitespace:true string ~allow_empty:true
    in
    flag
      ~aliases:[ "-f"; "--field" ]
      "-field"
      (optional_with_default [] arg_type)
      ~doc:
        " named fields to sort on, comma separated (outermost sort first; sorted on all \
         fields if omitted)"
  ;;

  let reverse_fields =
    let open Command.Param in
    let arg_type =
      Arg_type.comma_separated ~strip_whitespace:true string ~allow_empty:true
    in
    flag
      ~aliases:[ "--reverse-fields" ]
      "-reverse-fields"
      (optional_with_default [] arg_type)
      ~doc:" fields for which to reverse the sort order"
  ;;

  let pop_fields =
    fields_gen ~doc:" named fields required to count as fully populated, comma separated"
  ;;

  let exclude_fields =
    let open Command.Param in
    flag
      ~aliases:[ "--invert-match" ]
      "-v"
      no_arg
      ~doc:" exclude specified fields rather than extract them"
  ;;

  module Key_value_pair = struct
    type t = string * string option

    let of_string s =
      match String.split s ~on:'=' with
      | [ key ] -> ((key, None) : t)
      | [ key; value ] -> ((key, Some value) : t)
      | _ -> failwithf "Couldn't parse key=value pair: %s" s ()
    ;;

    let arg_type = Arg_type.create of_string

    let flag name desc =
      let open Command.Param in
      flag
        ("-" ^ name)
        ~aliases:[ "--" ^ name ]
        (listed arg_type)
        ~doc:(sprintf " %s attribute(s) in HTML output (e.g. \"align=center\")" desc)
    ;;
  end

  let table_attrs = Key_value_pair.flag "table" "Table"
  let th_attrs = Key_value_pair.flag "th" "Table header"
  let tr_attrs = Key_value_pair.flag "tr" "Table row"
  let td_attrs = Key_value_pair.flag "td" "Table cell"

  (* This could be specified through the more general table attributes above, but
     [Html.Html_table] handles this attribute separately, requiring a separate
     command-line option. *)
  let border =
    let open Command.Param in
    flag "-border" ~aliases:[ "--border" ] no_arg ~doc:" Visible borders in HTML output"
  ;;
end

include T
include Applicative.Make_let_syntax (T) (Open_on_rhs_intf) (T)
