"ppx_rope"
==========

`[%rope STRING]` constructs `Rope.t`s similarly to how
[`[%string STRING]`](%{root}/ppx/ppx_string/README.mdx) constructs strings.

Ropes allow for efficient string concantention - recursive string constructions can be
dramatically more efficient by using ropes. They also have some overhead, so smaller
constructions can be a bit slower.

# Usage

The syntax is shared with `[%string STRING]`.

The `%{expr}` syntax within `STRING` interpolates a rope, so `expr` must be of type
`Rope.t`.

The `%{expr#Module}` syntax still calls `Module.to_string`, then applies
`Rope.of_string` to the result. This avoids needing to add `to_rope` to lots of modules,
since `to_string` is much more common.

Be careful, `%{expr#Rope}` will round-trip through `Rope.to_string` and defeat some of the
performance gains of using ropes.

The `%{expr#Module:length}` syntax pads whatever rope is constructed by the part before
the `:` by adding spaces on the left. The syntax for padding a `Rope.t` is
`%{expr#:length}`, not `%{expr:length}`.

`Dedent` functionality is also available under the `[%rope_dedent STRING]` extension. The
syntax is otherwise the same.
