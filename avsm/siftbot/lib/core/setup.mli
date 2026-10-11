val handle :
  Eio_unix.Stdenv.base -> Siftbot_setup.request -> Siftbot_setup.response
(** [handle env request] lists redacted settings or saves a local profile and
    its shared JMAP bearer credential. It performs no network requests. *)
