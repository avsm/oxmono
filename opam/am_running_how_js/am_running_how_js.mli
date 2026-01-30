open! Core

(** [am_running_how] provides information on how the code is currently being run:
    - [`Node_test] means that the code is being run using node as part of an expect_test
    - [`Node_jsdom_test] means that the code is being run using node as part of an
      expect_test that enables jsdom to simulate a browser environment.
    - [`Node_benchmark] means that the code is being run using node as part of a benchmark
    - [`Node] means that the code is being run using node, but not as part of an
      expect_test or a benchmark
    - [`Browser_test] means that the code is being run in a browser as part of an
      expect_test
    - [`Browser_benchmark] means that the code is being run in the browser as part of a
      benchmark
    - [`Browser] means that the code is being run in a browser but not as part of a
      benchmark *)
val am_running_how
  : [ `Browser
    | `Browser_test
    | `Browser_benchmark
    | `Node
    | `Node_benchmark
    | `Node_test
    | `Node_jsdom_test
    ]

(** Returns true iff [am_running_how] is [`Browser], [`Browser_benchmark], or
    [`Browser_test]. *)
val am_in_browser : bool

(** Like [am_in_browser], but also returns [true] if [`Node_jsdom_test]. *)
val am_in_browser_like_api : bool
