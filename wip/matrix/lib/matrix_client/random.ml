type t = Eio.Flow.source_ty Eio.Resource.t

let of_source src = (src :> Eio.Flow.source_ty Eio.Resource.t)
let of_env env = of_source env#secure_random

(* A fresh buffer per call keeps this stateless, and therefore usable from
   several fibers or domains at once. The buffer is wiped after the copy so
   that the key material does not linger in an unreachable bigarray. *)
let generate t n =
  if n < 0 then
    invalid_arg
      (Printf.sprintf "Matrix_client.Random.generate: negative length %d" n)
  else if n = 0 then ""
  else begin
    let buf = Cstruct.create_unsafe n in
    Fun.protect
      ~finally:(fun () -> Cstruct.memset buf 0)
      (fun () ->
        Io_context.with_context
          (Printf.sprintf "reading %d secure-random bytes" n) (fun () ->
            Eio.Flow.read_exact t buf);
        Cstruct.to_string buf)
  end

let txn_id t =
  "m"
  ^ Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
      (generate t 16)
