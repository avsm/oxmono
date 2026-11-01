(** Toplevel printers for {!Json_pointer}, [Jsont.json], and [Jsont.Error.t].

    Printers are automatically installed when the library is loaded:
    {v # #require "json-pointer.top";; v}

    After loading, JSON Pointers display in their string representation:
    {[
      # Json_pointer.of_string "/foo/0";;
      - : Json_pointer.t = "/foo/0"
    ]}

    JSON values will display as formatted JSON strings:
    {v
      # Jsont.Json.list [Jsont.Json.int 1; Jsont.Json.int 2];;
      - : Jsont.json = [1, 2]
    v}

    And errors will display as readable messages:
    {[
      # Json_pointer.of_string "invalid";;
      Exception: Jsont.Error: Invalid JSON Pointer: must be empty or start with '/'
    ]} *)

val pointer_printer : Format.formatter -> Json_pointer.t -> unit
(** [pointer_printer] formats a JSON Pointer in its string representation.
    Suitable for use with [#install_printer]. *)

val json_printer : Format.formatter -> Jsont.json -> unit
(** [json_printer] formats a [Jsont.json] value as a human-readable JSON string.
    Suitable for use with [#install_printer]. *)

val error_printer : Format.formatter -> Jsont.Error.t -> unit
(** [error_printer] formats a [Jsont.Error.t] as a human-readable error message.
    Suitable for use with [#install_printer]. *)

val install : unit -> unit
(** [install ()] installs all printers. This is called automatically when the
    library is loaded, but can be called again if needed (e.g., in test
    environments where automatic initialization doesn't run). *)
