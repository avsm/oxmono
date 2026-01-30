(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.retry" ~doc:"HTTP Request Retry Logic"
module Log = (val Logs.src_log src : Logs.LOG)

(** Custom retry predicate for responses.
    Per Recommendation #14: Allow user-defined retry logic.
    The predicate receives (method, status, headers) and returns true to retry.
    This runs in addition to the built-in status_forcelist check. *)
type response_predicate = Method.t -> int -> Headers.t -> bool

(** Custom retry predicate for exceptions.
    Returns true if the exception should trigger a retry. *)
type exception_predicate = exn -> bool

type config = {
  max_retries : int;
  backoff_factor : float;
  backoff_max : float;
  status_forcelist : int list;
  allowed_methods : Method.t list;
  respect_retry_after : bool;
  jitter : bool;
  retry_response : response_predicate option;  (** Per Recommendation #14 *)
  retry_exception : exception_predicate option;  (** Per Recommendation #14 *)
  strict_method_semantics : bool;
  (** When true, raise an error if asked to retry a non-idempotent method.
      Per RFC 9110 Section 9.2.2: Non-idempotent methods should not be retried.
      Default is false (just log a debug message). *)
}

let default_config = {
  max_retries = 3;
  backoff_factor = 0.3;
  backoff_max = 120.0;
  status_forcelist = [408; 429; 500; 502; 503; 504];
  allowed_methods = [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `TRACE];
  respect_retry_after = true;
  jitter = true;
  retry_response = None;
  retry_exception = None;
  strict_method_semantics = false;
}

let create_config
    ?(max_retries = 3)
    ?(backoff_factor = 0.3)
    ?(backoff_max = 120.0)
    ?(status_forcelist = [408; 429; 500; 502; 503; 504])
    ?(allowed_methods = [`GET; `HEAD; `PUT; `DELETE; `OPTIONS; `TRACE])
    ?(respect_retry_after = true)
    ?(jitter = true)
    ?retry_response
    ?retry_exception
    ?(strict_method_semantics = false)
    () =
  Log.debug (fun m -> m "Creating retry config: max_retries=%d backoff_factor=%.2f \
    strict_method_semantics=%b custom_predicates=%b"
    max_retries backoff_factor strict_method_semantics
    (Option.is_some retry_response || Option.is_some retry_exception));
  {
    max_retries;
    backoff_factor;
    backoff_max;
    status_forcelist;
    allowed_methods;
    respect_retry_after;
    jitter;
    retry_response;
    retry_exception;
    strict_method_semantics;
  }

(** Check if a response should be retried based on built-in rules only.
    Use [should_retry_response] for full custom predicate support.
    @raise Error.t if strict_method_semantics is enabled and method is not idempotent *)
let should_retry ~config ~method_ ~status =
  let method_allowed = List.mem method_ config.allowed_methods in
  let status_retryable = List.mem status config.status_forcelist in
  let should = method_allowed && status_retryable in
  (* Per RFC 9110 Section 9.2.2: Only idempotent methods should be retried automatically *)
  if status_retryable && not method_allowed then begin
    if config.strict_method_semantics then
      raise (Error.invalid_requestf
        "Cannot retry %s request: method is not idempotent \
         (RFC 9110 Section 9.2.2). Disable strict_method_semantics to allow."
        (Method.to_string method_))
    else
      Log.debug (fun m -> m "Not retrying %s request (status %d): method is not idempotent \
        (RFC 9110 Section 9.2.2)" (Method.to_string method_) status)
  end else
    Log.debug (fun m -> m "Should retry? method=%s status=%d -> %b"
      (Method.to_string method_) status should);
  should

(** Check if a response should be retried, including custom predicates.
    Per Recommendation #14: User-defined retry logic.
    Returns true if either built-in rules or custom predicate says to retry. *)
let should_retry_response ~config ~method_ ~status ~headers =
  (* Check built-in rules first *)
  let builtin_should_retry =
    List.mem method_ config.allowed_methods &&
    List.mem status config.status_forcelist
  in
  (* Check custom predicate if provided *)
  let custom_should_retry = match config.retry_response with
    | Some predicate -> predicate method_ status headers
    | None -> false
  in
  let should = builtin_should_retry || custom_should_retry in
  Log.debug (fun m -> m "Should retry response? method=%s status=%d builtin=%b custom=%b -> %b"
    (Method.to_string method_) status builtin_should_retry custom_should_retry should);
  should

(** Check if an exception should trigger a retry, including custom predicates.
    Per Recommendation #14: User-defined retry logic. *)
let should_retry_exn ~config exn =
  match config.retry_exception with
  | Some predicate -> predicate exn
  | None -> false

let calculate_backoff ~config ~attempt =
  let base_delay = config.backoff_factor *. (2.0 ** float_of_int attempt) in
  let delay =
    if config.jitter then
      (* Add random jitter between 0 and base_delay *)
      base_delay +. Random.float base_delay
    else
      base_delay
  in
  let final_delay = min delay config.backoff_max in
  Log.debug (fun m -> m "Backoff calculation: attempt=%d base=%.2f jitter=%b -> %.2f seconds"
    attempt base_delay config.jitter final_delay);
  final_delay

(** Parse Retry-After header and cap to backoff_max to prevent DoS.
    Per RFC 9110 Section 10.2.3 and Recommendation #5:
    Cap server-specified Retry-After values to prevent malicious servers
    from causing indefinite client blocking. *)
let parse_retry_after ?(backoff_max = 120.0) value =
  Log.debug (fun m -> m "Parsing Retry-After header: %s" value);

  let raw_delay =
    (* First try to parse as integer (delay in seconds) *)
    match int_of_string_opt value with
    | Some seconds ->
        Log.debug (fun m -> m "Retry-After is %d seconds" seconds);
        Some (float_of_int seconds)
    | None ->
        (* Try to parse as HTTP-date (IMF-fixdate per RFC 9110 Section 5.6.7) *)
        match Http_date.parse value with
        | Some time ->
            let now = Unix.time () in
            let target = Ptime.to_float_s time in
            let delay = max 0.0 (target -. now) in
            Log.debug (fun m -> m "Retry-After is HTTP date, delay=%.2f seconds" delay);
            Some delay
        | None ->
            Log.warn (fun m -> m "Failed to parse Retry-After header: %s" value);
            None
  in
  (* Cap to backoff_max to prevent DoS from malicious Retry-After values *)
  match raw_delay with
  | Some delay when delay > backoff_max ->
      Log.warn (fun m -> m "Retry-After delay %.2fs exceeds backoff_max %.2fs, capping"
        delay backoff_max);
      Some backoff_max
  | other -> other

let with_retry ~sw:_ ~clock ~config ~f ~should_retry_exn =
  let rec attempt_with_retry attempt =
    Log.info (fun m -> m "Attempt %d/%d" attempt (config.max_retries + 1));

    match f () with
    | result ->
        if attempt > 1 then
          Log.info (fun m -> m "Request succeeded after %d attempts" attempt);
        result
    | exception exn when attempt <= config.max_retries && should_retry_exn exn ->
        let delay = calculate_backoff ~config ~attempt in

        Log.warn (fun m -> m "Request failed (attempt %d/%d): %s. Retrying in %.2f seconds..."
          attempt (config.max_retries + 1) (Printexc.to_string exn) delay);

        (* Sleep for the backoff duration *)
        Eio.Time.sleep clock delay;

        attempt_with_retry (attempt + 1)
    | exception exn ->
        if attempt > config.max_retries then
          Log.err (fun m -> m "Request failed after %d attempts: %s"
            attempt (Printexc.to_string exn))
        else
          Log.err (fun m -> m "Request failed and won't be retried: %s"
            (Printexc.to_string exn));
        raise exn
  in
  attempt_with_retry 1

let pp_config ppf config =
  Format.fprintf ppf "@[<v>Retry Configuration:@,\
    @[<v 2>\
    max_retries: %d@,\
    backoff_factor: %.2f@,\
    backoff_max: %.1f seconds@,\
    status_forcelist: [%a]@,\
    allowed_methods: [%a]@,\
    respect_retry_after: %b@,\
    jitter: %b\
    @]@]"
    config.max_retries
    config.backoff_factor
    config.backoff_max
    Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_print_int) config.status_forcelist
    Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ")
      (fun ppf m -> pp_print_string ppf (Method.to_string m))) config.allowed_methods
    config.respect_retry_after
    config.jitter

let log_retry ~attempt ~delay ~reason =
  Log.info (fun m -> m "Retry attempt %d scheduled in %.2f seconds. Reason: %s"
    attempt delay reason)