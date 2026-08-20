An inline slug link reads [the hello note](:hello-note), and a bare one
reads [:hello-note]. Both resolve through the entry set.

A reference slug link reads [these words][:hello-note]. That form is the
one that travels through a `Cmarkit.Meta.key`: the resolver tags the label
and the mapper reads the tag back.

A paper reads [:a-paper], a project reads [:a-project], an idea reads
[:an-idea] and a video reads [:a-video]. Each entry kind builds a different
sidenote.

A slug that names nothing reads [:no-such-entry], and falls back to a
plain link.

A contact link reads [@ada], and a reference contact link reads
[Ada Lovelace][@ada]. Both of those are reference links and travel through
the authorlink key. An inline one, [the author of it](@ada), does not: it is
detected from its destination instead, and keeps its own link text.

A tag link reads [ocaml](##ocaml) and a kind link reads
[papers](###papers).

An ordinary link to [example](https://example.com/b) must be untouched by
any of this.
