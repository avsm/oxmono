type body =
  | Empty
  | String of string @@ global
  | Stream of {
      length : int64 option;
      write : (Body.Sink.t -> unit) @@ global;
      trailers : Headers.t;
    }
  | Handoff of {
      kind : Body.handoff_kind;
      run : (Body.Socket.t -> unit) @@ global;
    }

type outcome = {
  status : Status.t;
  headers : Headers.t;
  last_modified : float option;
  body : body;
  content_length : int64 option;
}

type writer = outcome @ local -> unit

val[@zero_alloc strict] call_writer : writer @ local -> outcome @ local -> unit @@ portable
val[@zero_alloc] write_precondition_failed :
  Headers.t @ local -> Req.t @ local -> writer @ local -> unit @@ portable
val[@zero_alloc] write_internal_error : Req.t @ local -> writer @ local -> unit @@ portable
val[@zero_alloc] decide :
  has_now:bool -> float# -> Req.t @ local -> Resp.description @ local -> writer @ local -> unit @@ portable
