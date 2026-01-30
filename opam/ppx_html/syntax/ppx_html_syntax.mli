open! Core

(** This library contains the minimal amount of dependencies for the "syntax" of ppx_html

    This is used so that we add a minimal amount of dependencies while implementing
    ppx_html auto-formatting. We do not need to depend on html escaping/ppx_css as
    ppx_html only depends on these when it is actually "generating" code.

    The "ppx html formatting" needs to understand ppx_html syntax for a couple of reasons.
    We are using [prettier](https://prettier.io/) for the "html" formatting; howver
    ppx_html has some extra syntax of its own:

    {[
      [%html
        {|
         <%{Vdom.Node.div}>
          <div attr=%{value} %{attr}>
            %{child}
          </div>
         </>
         |}]
    ]}

    giving this syntax to prettier as-is result in failure, so there is a preprocessing
    step, that turns the above segment into something like:

    {[
      [%html
        {|
         <unique-id-1>
          <div attr=unique-id-2 unique-id-3>
            unique-id-4
          </div>
         </>
         |}]
    ]}

    and then prettier runs, and then the unique identifiers are regex-replaced back into
    the original interpolated OCaml. *)

module Model = Model
module Model_parser = Model_parser

module For_testing : sig
  module Ocaml_parsing = Ocaml_parsing
end
