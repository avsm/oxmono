module type S = sig
  @@ portable
  type t : immutable_data
  val of_int : int -> t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val jsont : t Jsont.t
end

module Make () : S = struct
  type t = int

  let max_exact_json_integer = 9_007_199_254_740_991

  let of_int i =
    if i < 0 || i > max_exact_json_integer then
      invalid_arg "Zulip ID must be an exact nonnegative JSON integer"
    else i

  let to_int i = i
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int

  let jsont =
    Jsont.map ~kind:"Zulip identifier"
      ~dec:(fun n ->
        if
          (not (Float.is_finite n))
          || (not (Float.is_integer n))
          || n < 0.
          || n > float_of_int max_exact_json_integer
        then
          Jsont.Error.msgf Jsont.Meta.none
            "ID must be an exact nonnegative JSON integer";
        int_of_float n)
      ~enc:(fun i -> float_of_int (to_int i))
      Jsont.number
end

module User = Make ()
module Channel = Make ()
module Message = Make ()
module Recipient = Make ()
module Event = Make ()
module User_group = Make ()
module Channel_folder = Make ()
module Linkifier = Make ()
module Profile_field = Make ()
module Attachment = Make ()
module Draft = Make ()
module Scheduled_message = Make ()
module Reminder = Make ()
module Saved_snippet = Make ()
