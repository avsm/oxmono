(** Codec registry for external codec extensions.

    Allows external packages (e.g. zarr-blosc) to register codec builders
    so the core codec chain can construct them from metadata JSON. *)

open Ztypes

(** The class of a codec: array-to-array, array-to-bytes, or bytes-to-bytes. *)
type codec_class =
  | ArrayToArray of Codec_intf.array_to_array
  | ArrayToBytes of Codec_intf.array_to_bytes
  | BytesToBytes of Codec_intf.bytes_to_bytes

(** A codec builder takes the JSON configuration, dtype, and chunk shape,
    and returns a classified codec or an error. *)
type codec_builder = Yojson.Safe.t -> Dtype.t -> int array -> (codec_class, error) Stdlib.result

(** Internal registry table *)
let registry : (string, codec_builder) Hashtbl.t = Hashtbl.create 16

(** Register a codec builder under the given name.
    Replaces any previous registration for that name. *)
let register name builder =
  Hashtbl.replace registry name builder

(** Look up a codec builder by name. *)
let find name =
  Hashtbl.find_opt registry name

(** Check whether a codec with the given name is registered. *)
let is_registered name =
  Hashtbl.mem registry name
