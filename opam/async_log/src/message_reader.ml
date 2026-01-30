open! Core
open! Async_kernel
open! Import

let read_from_reader format r ~pipe_w =
  match format with
  | `Sexp | `Sexp_hum ->
    let sexp_pipe = Reader.read_sexps r in
    let%map () = Pipe.transfer sexp_pipe pipe_w ~f:Message.Stable.V2.t_of_sexp in
    Pipe.close pipe_w
  | `Bin_prot ->
    let rec loop () =
      match%bind Reader.read_bin_prot r Message.Stable.V2.bin_reader_t with
      | `Eof ->
        Pipe.close pipe_w;
        return ()
      | `Ok msg -> Pipe.write pipe_w msg >>= loop
    in
    loop ()
;;

let pipe_of_reader format reader =
  Pipe.create_reader ~close_on_exception:false (fun pipe_w ->
    read_from_reader format reader ~pipe_w)
;;

let pipe format filename =
  Pipe.create_reader ~close_on_exception:false (fun pipe_w ->
    Reader.with_file filename ~f:(fun reader -> read_from_reader format reader ~pipe_w))
;;

module Expert = struct
  let read_one format reader =
    match format with
    | `Sexp | `Sexp_hum ->
      let%map sexp = Reader.read_sexp reader in
      Reader.Read_result.map sexp ~f:Message.Stable.V2.t_of_sexp
    | `Bin_prot -> Reader.read_bin_prot reader Message.Stable.V2.bin_reader_t
  ;;
end
