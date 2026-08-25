# ocaml-proj, vendored

Bindings to the PROJ coordinate transformation library, vendored from
https://github.com/geocaml/ocaml-proj at commit
440c7e0084352f314f16d91e6b26a1699598596d (no upstream release exists).
Every file under `src/`, `test/` and `example/` is byte identical to
that commit except for the one patch below. This README replaces the
upstream one.

## Patches

`src/lib_c/function_description.ml:84`: `proj_coord` bound its four
arguments as ctypes `float`, which is C `float`, while PROJ declares
`PJ_COORD proj_coord(double, double, double, double)`. Every input
coordinate was truncated to single precision, displacing a UTM
northing by up to 0.5 m before PROJ ever saw it. Changed the four
`float` to `double`. Found by the tessera geodesy golden tests
(2026-08-25), which showed `Crs.inverse` off by 0.21 m against pyproj
until the fix. Worth an upstream pull request.

The copy is here because no opam release of `proj` exists to install,
and `bleeding/tessera` needs CRS transformations (WGS84 to the UTM
zones of the Tessera store). Two of the three packages are usable:

- `proj` is a virtual library carrying the interface.
- `proj_c` implements it with ctypes over the system PROJ. It needs
  `libproj-dev` (PROJ 9 was used here), `pkg-config` and the PROJ
  database (`/usr/share/proj/proj.db`) at run time for EPSG lookups.
- `proj_js` is not usable in this workspace. It needs `brr`, which is
  not installed. Nothing here depends on it, and vendored directories
  only build what a consumer pulls in, so it is inert.

`Transformation.normalize_for_visualization` is the equivalent of
pyproj's `always_xy`: without it an EPSG:4326 transformation takes
latitude first.

## Re-vendoring checklist

1. Copy the new upstream over `src/`, `test/`, `example/`,
   `dune-project` and the opam files. Remove `.git`. Keep this README,
   updating the commit hash above.
2. Confirm the copy is unpatched with a diff against the upstream
   checkout.
3. Vendored `runtest` aliases are inert, so verify through the
   consumer: `dune build @bleeding/tessera/all @bleeding/tessera/runtest
   --force`. The tessera projection tests compare against
   pyproj-generated golden vectors and are the guard tests for this
   copy.
