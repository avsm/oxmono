type config = {
  max_retries : int;
  backoff_factor : float;
  backoff_max : float;
  status_forcelist : int list;
  allowed_methods : Http.Method.t list;
  respect_retry_after : bool;
  jitter : bool;
  retry_exception : (exn -> bool) option;
  retry_response : (Middleware.request -> Middleware.response -> bool) option;
}

let default = {
  max_retries = 3;
  backoff_factor = 0.5;
  backoff_max = 120.0;
  status_forcelist = [ 429; 500; 502; 503; 504 ];
  allowed_methods = [ `GET; `HEAD; `PUT; `DELETE; `OPTIONS ];
  respect_retry_after = true;
  jitter = true;
  retry_exception = None;
  retry_response = None;
}

let v ?(max_retries = default.max_retries)
    ?(backoff_factor = default.backoff_factor)
    ?(backoff_max = default.backoff_max)
    ?(status_forcelist = default.status_forcelist)
    ?(allowed_methods = default.allowed_methods)
    ?(respect_retry_after = default.respect_retry_after)
    ?(jitter = default.jitter)
    ?retry_exception ?retry_response () =
  { max_retries; backoff_factor; backoff_max; status_forcelist;
    allowed_methods; respect_retry_after; jitter;
    retry_exception; retry_response }
