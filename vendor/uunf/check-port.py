#!/usr/bin/env python3
"""Compare this Uunf port with a pristine upstream source tree."""

import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

COMPARE = r"""
let forms = [`NFC; `NFD; `NFKC; `NFKD]
let checked = ref 0
let check text =
  List.iter (fun form ->
    let before = Stock.Uunf_string.normalize_utf_8 form text in
    let after = Uunf_string.normalize_utf_8 form text in
    if before <> after then failwith "normalization differs";
    incr checked) forms
let () =
  let buffer = Buffer.create 8192 in
  for scalar = 0 to 0x10ffff do
    if Uchar.is_valid scalar then
      Buffer.add_utf_8_uchar buffer (Uchar.of_int scalar);
    if scalar mod 1024 = 1023 then
      (check (Buffer.contents buffer); Buffer.clear buffer)
  done;
  check (Buffer.contents buffer);
  let random = Random.State.make [|170; 52; 39|] in
  for _ = 1 to 20000 do
    Buffer.clear buffer;
    for _ = 1 to 100 do
      let scalar = match Random.State.int random 4 with
        | 0 -> 0x300 + Random.State.int random 0x70
        | 1 -> 0x1100 + Random.State.int random 0x100
        | 2 -> 0xac00 + Random.State.int random 11172
        | _ -> Random.State.int random 0x110000 in
      if Uchar.is_valid scalar then
        Buffer.add_utf_8_uchar buffer (Uchar.of_int scalar)
    done;
    check (Buffer.contents buffer)
  done;
  Printf.printf "Uunf differential: %d comparisons passed, all Unicode scalars and 20,000 mixed sequences, all four forms.\n" !checked
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("upstream", type=Path, help="pristine checkout or archive")
    args = parser.parse_args()
    upstream = args.upstream.resolve() / "src"
    modules = ["uunf_fmt", "uunf_tmap", "uunf_tmapbool", "uunf_tmapbyte",
               "uunf_data", "uunf", "uunf_string"]
    for name in modules:
        if not (upstream / (name + ".ml")).is_file():
            parser.error(f"missing upstream source: {name}.ml")
    with tempfile.TemporaryDirectory(prefix="uunf-differential-") as directory:
        root = Path(directory)
        port = Path(__file__).resolve().parent
        shutil.copytree(port, root / "uunf",
                        ignore=shutil.ignore_patterns("__pycache__", "_build"))
        (root / "dune-project").write_text(
            "(lang dune 3.21)\n(name uunf_diff)\n")
        (root / "dune").write_text(
            "(executable (name compare) (libraries uunf))\n")
        (root / "stock.ml").write_text("\n".join(
            "module " + name.capitalize() + " = struct\n"
            + (upstream / (name + ".ml")).read_text() + "\nend\n"
            for name in modules))
        (root / "compare.ml").write_text(COMPARE)
        subprocess.run(["dune", "exec", "--root", str(root),
                        "--profile", "release", "./compare.exe"],
                       cwd=root, check=True)


if __name__ == "__main__":
    main()
