(** OwnTracks Recorder response codecs. *)

val locations_response_jsont : Owntracks_location.t list Jsont.t
(** [locations_response_jsont] accepts an array or a [data] wrapper. *)

val list_response_jsont : string list Jsont.t
(** [list_response_jsont] accepts an array or a [results] wrapper. *)

val decode_locations :
  Bytesrw.Bytes.Reader.t -> (Owntracks_location.t list, string) result

val decode_list : Bytesrw.Bytes.Reader.t -> (string list, string) result
