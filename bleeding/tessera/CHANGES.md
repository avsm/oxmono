## Unreleased

- Initial Tessera client: zone routing, projection, probe, region and
  patch reads over the zarrz store backends.

- Store handle and per-zone datasets: point probes that route across a
  UTM seam, and dequantised region reads on the zone's native grid.
- Opening a store written by zarr-python costs one request. The
  consolidated node map answers zone enumeration and every zone open.
- Point reads go through a cache of 32 by 32 tiles, so a revisited tile
  costs no request.
