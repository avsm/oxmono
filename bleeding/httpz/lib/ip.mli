(** This module recognizes IP address literals as a resolver does.

    A host string that a policy compares, a cookie domain, or a redirect target
    may hold an IPv4 address in any spelling
    {{:https://man7.org/linux/man-pages/man3/inet_aton.3.html}inet_aton(3)}
    accepts: one to four dot-separated parts, each decimal, hexadecimal with an
    [0x] prefix, or octal with a leading [0], the last part filling every byte
    the earlier ones left. So [127.1], [2130706433], [0x7f000001] and
    [0177.0.0.1] all name 127.0.0.1. A check that recognizes only the dotted
    quad admits the other spellings as ordinary names, which is how a string
    blocklist is bypassed; {!ipv4_canonical} folds them to one form instead.

    IPv6 recognition defers to {!Httpz.Uriz.Scanner.is_ipv6}, the strict RFC 3986 scanner.
*)

val ipv4_of_string : string -> int option @@ portable
(** [ipv4_of_string s] is the 32-bit address [s] denotes, in host byte order,
    when all of [s] is an inet_aton(3) IPv4 literal. It is [None] for an empty
    part, more than one trailing dot, more than four parts, a part above the range its
    position leaves it, or any byte that is not a digit of the part's base. *)

val ipv4_canonical : string -> string option @@ portable
(** [ipv4_canonical s] is [s] rendered as a dotted quad when it is an IPv4
    literal in any spelling, and [None] otherwise. *)

val is_ipv4_literal : string -> bool @@ portable
(** [is_ipv4_literal s] is [true] when {!ipv4_of_string} recognizes [s]. *)

val is_ipv6_literal : string -> bool @@ portable
(** [is_ipv6_literal s] is [true] when [s] is an RFC 3986 IPv6 address, with or
    without the brackets an authority wraps it in. An IPv4-mapped tail such as
    [::ffff:127.0.0.1] is one, and is not folded to its IPv4 form: only a check
    on the resolved address covers that. *)

val is_literal : string -> bool @@ portable
(** [is_literal s] is [true] when [s] is an IPv4 or IPv6 literal. A name that
    satisfies it cannot be a DNS name, so it neither suffix-matches nor carries
    a public suffix. *)
