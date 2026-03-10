(** {1 Gzip Codec}

    Gzip compression using the [decompress] library (RFC 1951 DEFLATE
    wrapped in RFC 1952 gzip format).

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/codecs/gzip/index.html}
    Zarr v3.1 spec, "gzip" codec}.  This is a {b bytes-to-bytes} codec.

    {b JSON config:} [\{"level": 0-9\}] where 0 = no compression,
    1 = fastest/least, 9 = slowest/most.  Default level is 5. *)

val create : int -> Zarr_codec.bytes_to_bytes
(** [create level] builds a gzip codec at the given compression level
    (0-9). *)
