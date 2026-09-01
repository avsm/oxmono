type config =
  { max_retries : int
  ; backoff_factor : float
  ; backoff_max : float
  ; status_forcelist : int list
  ; allowed_methods : Http.Method.t list
  ; respect_retry_after : bool
  ; jitter : bool
  ; retry_exception : (exn -> bool) option
  ; retry_response : (Middleware.request -> Middleware.response -> bool) option
  ; retry_request : (Middleware.request -> bool) option
  }

let default =
  { max_retries = 3
  ; backoff_factor = 0.5
  ; backoff_max = 120.0
  ; status_forcelist = [ 429; 500; 502; 503; 504 ]
  ; allowed_methods = [ `GET; `HEAD; `PUT; `DELETE; `OPTIONS ]
  ; respect_retry_after = true
  ; jitter = true
  ; retry_exception = None
  ; retry_response = None
  ; retry_request = None
  }
;;

let max_retries_limit = 100

let validate config =
  if config.max_retries < 0 || config.max_retries > max_retries_limit
  then
    invalid_arg
      (Printf.sprintf
         "Fetch.Retry: max_retries must be between 0 and %d"
         max_retries_limit);
  let non_negative_finite name value =
    if not (Float.is_finite value) || value < 0.
    then
      invalid_arg
        ("Fetch.Retry: " ^ name ^ " must be finite and non-negative")
  in
  non_negative_finite "backoff_factor" config.backoff_factor;
  non_negative_finite "backoff_max" config.backoff_max;
  if List.exists (fun status -> status < 100 || status > 599) config.status_forcelist
  then invalid_arg "Fetch.Retry: status_forcelist contains an invalid status"
;;

let v
  ?(max_retries = default.max_retries)
  ?(backoff_factor = default.backoff_factor)
  ?(backoff_max = default.backoff_max)
  ?(status_forcelist = default.status_forcelist)
  ?(allowed_methods = default.allowed_methods)
  ?(respect_retry_after = default.respect_retry_after)
  ?(jitter = default.jitter)
  ?retry_exception
  ?retry_response
  ?retry_request
  ()
  =
  let config =
    { max_retries
    ; backoff_factor
    ; backoff_max
    ; status_forcelist
    ; allowed_methods
    ; respect_retry_after
    ; jitter
    ; retry_exception
    ; retry_response
    ; retry_request
    }
  in
  validate config;
  config
;;
