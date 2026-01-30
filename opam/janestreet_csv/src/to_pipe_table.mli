(** Convert CSV to Confluence table markup *)

val run
  :  ?separator:char
  -> no_header:bool (** [no_header] treats the first line as a row of data. *)
  -> suppress_header:bool
       (** [suppress_header] asserts that the first line is a header, and omits it from
           the output *)
  -> Csv_common.Or_file.t
  -> unit
