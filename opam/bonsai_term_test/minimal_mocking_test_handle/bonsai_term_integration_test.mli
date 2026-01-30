open! Core

(* What is this library for? We already have [bonsai_term_test] why would I need this
   "bonsai_term_integration_test" library?

   For context:

   - [bonsai_term] is a library you can use to build TUIs with Bonsai.
   - [bonsai_term_test] lets you write tests for your bonsai_term app.

   Unfortunately, tests written with [bonsai_term_term] are "mock" tests which are
   convenient to write, but they do not cover the logic inside of the "bonsai term loop".

   What is the "bonsai term loop"?

   1. Compute the up-to-date [ View.t ] of the bonsai app.
   2. Draw it to stdout.
   3. Wait for the next event(s) (keyboard/mouse input to stdin, timer, window resize,
      stdin closing)
   4. Apply the [ ~handler:(Event.t -> unit Effect.t) ] function to the received event(s).

   This "bonsai term loop" is untested by bonsai_term_test, but is testable by
   bonsai_term_integration_test. Letting you test things like:

   - The actual ascii codes/text that we write to stdout.
   - The logic inside of [bonsai_term]
   - The specific order in which events and frames are processed.
   - [bonsai_term] breaking bugs.

   Why not just always write tests in [bonsai_term_integration_test]?

   While these tests give us as close as possible to "e2e"/"zero-mocking" coverage, they
   are less convenient to write. You need to "control"/"drive" each frame loop yourself,
   and if you accidentally do step [3] without also scheduling an event, then your test
   will never finish and eventually timeout!

   For more information on these tests, refer to [Handle].
*)

module Handle = Handle
module Capability = Bonsai_term_test.Capability
module Frame_outcome = Bonsai_term.Private.Frame_outcome
module File_descriptors = File_descriptors
