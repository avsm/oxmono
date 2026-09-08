type err =
  | Network of string
  | Policy_denied of string
  | Tls of string
  | Http of { status : int; body : string }
  | Json of string
  | Matrix of {
      errcode : Matrix_client.Error.errcode;
      error : string;
      retry_after_ms : int option;
    }
  | Not_logged_in
  | No_content
  | Cancelled

type Eio.Exn.err += E of err

let err e = Eio.Exn.create (E e)

let pp_err fmt = function
  | Network msg -> Format.fprintf fmt "Network error: %s" msg
  | Policy_denied msg -> Format.fprintf fmt "Request denied by policy: %s" msg
  | Tls msg -> Format.fprintf fmt "TLS error: %s" msg
  | Http { status; body } -> Format.fprintf fmt "HTTP %d: %s" status body
  | Json msg -> Format.fprintf fmt "JSON error: %s" msg
  | Matrix { errcode; error; _ } ->
      Format.fprintf fmt "Matrix error %s: %s"
        (Matrix_client.Error.errcode_to_string errcode)
        error
  | Not_logged_in -> Format.fprintf fmt "Not logged in"
  | No_content -> Format.fprintf fmt "No content"
  | Cancelled -> Format.fprintf fmt "Operation cancelled"

let () =
  Eio.Exn.register_pp (fun fmt -> function
    | E e ->
        pp_err fmt e;
        true
    | _ -> false)

let of_client_error = function
  | Matrix_client.Error.Matrix_error e ->
      Matrix
        {
          errcode = e.errcode;
          error = e.error;
          retry_after_ms = e.retry_after_ms;
        }
  | Matrix_client.Error.Network_error msg -> Network msg
  | Matrix_client.Error.Policy_denied msg -> Policy_denied msg
  | Matrix_client.Error.Tls_error msg -> Tls msg
  | Matrix_client.Error.Json_error msg -> Json msg
  | Matrix_client.Error.Http_error { status; body } -> Http { status; body }
  | Matrix_client.Error.No_session -> Not_logged_in
  | Matrix_client.Error.No_content -> No_content

let with_context context fn =
  try fn ()
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "%s" context

let raise_client_error ?context e =
  match context with
  | None -> raise (err (of_client_error e))
  | Some context ->
      with_context context (fun () -> raise (err (of_client_error e)))

let unwrap ?context = function
  | Ok v -> v
  | Error e -> raise_client_error ?context e

let is_retryable = function
  | Network _ -> true
  | Http { status; _ } -> status >= 500 || status = 429
  | Matrix { errcode; _ } -> errcode = Matrix_client.Error.M_LIMIT_EXCEEDED
  | Policy_denied _ | Tls _ | Json _ | Not_logged_in | No_content | Cancelled ->
      false
