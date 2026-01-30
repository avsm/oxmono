open Base

(** Numeric string comparison, also sometimes called "natural comparison", is the string
    ordering that orders numeric subcomponents as numbers, so that e.g. "xyz2" comes
    before "xyz10", rather than after (as it does when they are compared as plain
    strings). As a spec this is incomplete, and various implementations around the web
    fill in the missing details differently. This one errs on the side of doing less,
    rather than more, and in particular doesn't do any special handling of minus signs or
    decimal points.

    Precisely: first, split both input strings into a possibly-empty initial segment of
    non-numeric characters, followed by alternating non-empty segments of numeric
    characters and non-numeric characters. Then compare corresponding segments, using
    string comparison on non-numeric segments and integer comparison on numeric segments;
    if you run out of segments on one side, the shorter segment list compares smaller.
    (Allowing the initial non-numeric segment to be empty ensures we always compare
    corresponding segments of the same kind).

    Leading zeroes in numeric components complicate the above spec a little, since they
    mean two distinct strings can have equal numeric value. The choice we make is to sort
    by numeric value first, but break ties by string length, so that e.g. "1" < "01" < "2"
    < "02". This ensures that numeric string equality coincides with ordinary string
    equality, so that e.g. sets and maps of strings behave as you'd expect (although they
    have distinct serializations / orderings, so they're not identical). See the README
    for more discussion.

    The comparison functions do not allocate, so that they can be used in as wide a
    variety of contexts as possible. *)
type t = string

include Comparable.S with type t := t
include Sexpable.S with type t := t
