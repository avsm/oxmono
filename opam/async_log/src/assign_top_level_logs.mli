(*_ This module is internal to [Async_log]. It is [include]d in the export module so that
    any code that uses [Async_log] does the top-level side effect in this module to assign
    logging logic in upstream dependencies, like [Async_kernel.Monitor0.try_with_log_exn].
*)
