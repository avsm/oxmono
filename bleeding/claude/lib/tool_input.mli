(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Opaque tool input with typed accessors.

    Tool inputs are JSON objects representing parameters passed to tools. This
    module provides type-safe accessors while hiding the JSON structure from
    most client code. *)

type t
(** Abstract type for tool inputs. *)

(** {1 Typed Accessors} *)

val get_string : t -> string -> string option
(** [get_string t key] returns the string value for [key], if present and a
    string. *)

val get_int : t -> string -> int option
(** [get_int t key] returns the integer value for [key], if present and an int.
*)

val get_bool : t -> string -> bool option
(** [get_bool t key] returns the boolean value for [key], if present and a bool.
*)

val get_float : t -> string -> float option
(** [get_float t key] returns the float value for [key], if present and a float.
*)

val get_string_list : t -> string -> string list option
(** [get_string_list t key] returns the string list for [key], if present and a
    list of strings. *)

val keys : t -> string list
(** [keys t] returns all keys in the input. *)

val is_empty : t -> bool
(** [is_empty t] returns true if the input has no keys. *)

(** {1 Escape Hatch} *)

val to_json : t -> Jsont.json
(** [to_json t] returns the underlying JSON for advanced use cases. *)

val of_json : Jsont.json -> t
(** [of_json json] wraps JSON as a tool input. *)

(** {1 Construction} *)

val empty : t
(** [empty] is an empty tool input. *)

val add_string : string -> string -> t -> t
(** [add_string key value t] adds a string field. *)

val add_int : string -> int -> t -> t
(** [add_int key value t] adds an integer field. *)

val add_bool : string -> bool -> t -> t
(** [add_bool key value t] adds a boolean field. *)

val add_float : string -> float -> t -> t
(** [add_float key value t] adds a float field. *)

val of_assoc : (string * Jsont.json) list -> t
(** [of_assoc assoc] creates tool input from an association list. *)

val of_string_pairs : (string * string) list -> t
(** [of_string_pairs pairs] creates tool input from string key-value pairs. *)
