Numeric string comparison
=========================

# Numeric string comparison
A comparison function (and assorted sets / maps / etc.) for strings
that sorts numeric fragments of strings according to their numeric
value, so that e.g. "abc2" < "abc10".

People often call this "natural sort", and link to this:

https://blog.codinghorror.com/sorting-for-humans-natural-sort-order/

which links to a few other posts about the matter. Sometimes it's
called "alphanum sort" or "human sort" or something. "Numeric string
compare" is chosen as the name that gets closest to expressing what it
actually does (to me, "natural sort" sounds like something that might
include a different treatment of case, special handling of whitespace
or punctuation, etc.)

See the comments in [the
interface](
          https://github.com/janestreet/numeric_string/blob/master/src/numeric_string.mli)
for a precise specification of the algorithm.

## Tie-breaking

One of the main ambiguities in the spec is what to do with strings
that have equal numeric value because of leading zeroes.

This library makes the decision to preserve normal string equality,
since having "equal" values that aren't really equal is often more
confusing than it's worth (you have to start thinking about things
like which copy to keep when you're removing duplicates). Hence we
have to break ties somehow.

You could imagine trying as hard as possible to preserve the notion
that equal numeric values are equal, and e.g. compare "a01a" < "a1b" <
"a01c", yet still break ties in "a1a" vs. "a01a" somehow. I can't
think of any simple semantics that implements this idea -- it's much
easier to explain an algorithm that always works
component-by-component.

So then the question is how to break ties within numeric components.
One idea I had was to always sort numeric components by length first,
even when they had leading zeroes. This still gets you the result you
expect without leading zeroes, and moreover it implements an intuition
that if you see (say) "ab001" in a name, it should be assumed there's
a three-digit format there, so we should group it together with other
names that have three digits there, rather than putting it close to
"ab1" and "ab2".

While I think there's a good argument for that approach, I think it's
not obvious enough, so people wouldn't expect that behaviour without
thinking about it carefully. Aiming, therefore, to implement the least
surprising behaviour, I just always sort components by numeric value
first, and then break ties in some arbitrary way -- the easiest is by
length. Then "1" appears next to "01" in the ordering, which is I
think what people would expect even if it's not obviously what they
want.
