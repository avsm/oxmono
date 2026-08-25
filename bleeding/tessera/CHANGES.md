## Unreleased

- Initial Tessera client: zone routing, projection, probe, region and
  patch reads over the zarrz store backends.

- Store handle and per-zone datasets: point probes that route across a
  UTM seam, and dequantised region reads on the zone's native grid.
- Opening a store written by zarr-python costs one request. The
  consolidated node map answers zone enumeration and every zone open.
- Point reads go through a cache of 32 by 32 tiles, so a revisited tile
  costs no request.

- Square patches centred on a point: sliced off one zone's grid where
  the patch fits in it, and merged onto a patch-centred transverse
  Mercator grid where it crosses a UTM seam, each pixel taken from the
  zone owning its longitude.
- A NumPy `.npy` writer for float32 blocks, so a region or a patch
  loads in the Python tooling as written.
- A `tessera` command with `info`, `probe`, `region` and `patch`
  subcommands, over the public store or a local directory.
