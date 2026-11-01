external available : Unix.file_descr -> int64 = "zarr_proxy_available_bytes"

let with_fd path label f =
  Eio.Path.with_open_in ~follow:false path (fun file ->
      let fd =
        match Eio_unix.Resource.fd_opt file with
        | Some fd -> fd
        | None -> invalid_arg "Native filesystem required"
      in
      (* Eio has no filesystem-space query or directory fsync operation. *)
      Eio_unix.run_in_systhread (fun () -> Eio_unix.Fd.use_exn label fd f))

let free path = with_fd path "cache free space" available
let sync path = with_fd path "cache directory fsync" Unix.fsync
