@@ portable

type t = private {
  house_number : string option;
  road : string option;
  city : string option;
  town : string option;
  village : string option;
  county : string option;
  state : string option;
  postcode : string option;
  country : string option;
  country_code : string option;
  fields : Tags.t;
}
(** Common Nominatim address components plus all provider-defined fields. OSM
    address categories vary by country, so every component is optional. *)

val jsont : t Jsont.t
(** [jsont] maps a string-valued address object. All fields survive encoding,
    including components not exposed as named record fields. *)

val find : string -> t -> string option
(** [find key address] returns any address component, including provider-
    specific keys. *)
