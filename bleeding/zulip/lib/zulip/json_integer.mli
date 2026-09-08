@@ portable

(** Exactly representable JSON integers. *)

val jsont : int Jsont.t
(** [jsont] is a codec for integers from [-(2^53 - 1)] through [2^53 - 1].
    Decoding rejects fractions and nonfinite or out-of-range numbers. Encoding
    rejects out-of-range integers. *)
