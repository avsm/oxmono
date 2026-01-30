open! Core
open! Async

module Open_on_rhs_intf = struct
  module type S = sig
    include module type of struct
      include Command.Param
    end

    val file_stdin_anon : Csv_common.Or_file.t t
    val file_stdin_flag : Csv_common.Or_file.t t
    val files : string list t
    val reverse : bool t
    val reverse_fields : string list t
    val field : string t
    val field' : aliases:string list -> string t
    val time_field : string t
    val start_time : Time_float.t t
    val stop_time : Time_float.t t
    val grid_step : Time_float.Span.t t
    val max_width : int option t
    val prefer_split_on_spaces : bool t
    val regexp_arg : Re2.t Arg_type.t
    val regexp : Re2.t t
    val invert : bool t
    val skip_lines : int option t
    val sep_arg : char Arg_type.t
    val sep : char t
    val key_specifier : string t
    val no_header : bool t
    val no_headers_use_indices_instead : bool t
    val space : int t
    val suppress_header : bool t
    val utf8 : bool t
    val fields_gen : doc:string -> string list t
    val fields : string list t
    val fields_backward_compat : string list t
    val pop_fields : string list t
    val exclude_fields : bool t
    val table_attrs : (string * string option) list t
    val th_attrs : (string * string option) list t
    val tr_attrs : (string * string option) list t
    val td_attrs : (string * string option) list t
    val border : bool t
  end
end

module type Csv_param = sig
  module Open_on_rhs_intf = Open_on_rhs_intf
  include Open_on_rhs_intf.S

  include
    Applicative.Let_syntax
    with type 'a t := 'a Command.Param.t
    with module Open_on_rhs_intf := Open_on_rhs_intf
end
