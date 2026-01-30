(** Convert CSV to HTML table *)

val run
  :  ?separator:char
  -> no_header:bool (** [no_header] treats the first line as a row of data. *)
  -> suppress_header:bool
       (** [suppress_header] asserts that the first line is a header, and omits it from
           the output *)
  -> table_attrs:(string * string option) list
  -> th_attrs:(string * string option) list
  -> tr_attrs:(string * string option) list
  -> td_attrs:(string * string option) list
  -> border:bool
  -> unescaped_html:bool
  -> Csv_common.Or_file.t
  -> unit
