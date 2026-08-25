(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** NumPy [.npy] version 1.0 for float32 blocks.

    A writer for the one shape this library produces: a C-order float32
    {!Zarrz.Slab.t}, which is what {!Dataset.Region.t} and {!Patch.t}
    carry. [numpy.load] reads the result, so a region or a patch written
    here drops straight into the Python tooling.

    Nothing here does I/O. {!header} and {!to_string} are strings a
    caller writes wherever it likes, which keeps the module free of any
    dependency on an I/O library.

    A file is the 6 byte magic, the version [1 0], the header length as
    two little-endian bytes and the header itself, padded with spaces to
    a multiple of 64 bytes and ending in a newline, then the elements.
    That is what [numpy.save] emits, byte for byte.

    {2 Endianness}

    The elements are written as they sit in the slab, which is native
    endian, under a [<f4] descriptor that claims little endian. The two
    agree only on a little-endian host, so {!header} refuses to run on a
    big-endian one rather than write a file that reads back byte
    swapped. *)

val header : Zarrz.Slab.t -> string
(** [header s] is the magic, the version, the header length and the
    padded header of the file holding [s], a string whose length is a
    multiple of 64.

    @raise Invalid_argument if [s] is not [Zarrz.Dtype.Float32], if the
    host is big endian, or if the shape is so long that its header will
    not fit the 16 bit length of version 1.0. *)

val to_string : Zarrz.Slab.t -> string
(** [to_string s] is {!header} followed by the elements of [s], the
    whole file. It copies the elements, so a large block is better
    written as {!header} and then the bytes of
    [Zarrz.Slab.bigstring s].

    @raise Invalid_argument as {!header} does. *)
