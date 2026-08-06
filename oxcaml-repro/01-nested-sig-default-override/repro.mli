@@ portable

(* The file-level default is [portable]. The item-level [@@ nonportable]
   below should exempt [M.pp], but the override is ignored inside a
   nested signature: the compiler still requires [M.pp] to be portable. *)
module M : sig
  val ok : int -> int
  val pp : Format.formatter -> int -> unit @@ nonportable
end
