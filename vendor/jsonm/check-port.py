#!/usr/bin/env python3
"""Compare Jsonm against a pristine upstream source tree."""
import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

COMPARE = r'''
let lexemes decode range pp d =
  let rec loop n acc =
    if n > 10000 then failwith "decoder did not terminate";
    let item = decode d in
    let rendered = match item with
      | `Error e -> Format.asprintf "%a" pp e
      | _ -> Marshal.to_string item [] in
    let acc = (rendered, range d) :: acc in
    match item with `End -> List.rev acc | _ -> loop (n + 1) acc
  in loop 0 []
let count = ref 0
let check s =
  let a = Stock.decoder (`String s) and b = Jsonm.decoder (`String s) in
  let a = lexemes Stock.decode Stock.decoded_range Stock.pp_error a in
  let b = lexemes Jsonm.decode Jsonm.decoded_range Jsonm.pp_error b in
  if a <> b then failwith (Printf.sprintf "different decoding: %S" s);
  incr count
let () =
  List.iter check [""; "null"; "true"; "1e999"; "[1,2]"; "{}";
    "{\"x\":1,\"x\":2}"; "\"\\ud800\""; "\"\\ud834\\udd1e\"";
    "[1,]"; "{}{}"; "\"\255\""; "\239\187\191true"];
  let random = Random.State.make [|102|] in
  for _ = 1 to 5000 do
    let s = String.init (Random.State.int random 100)
      (fun _ -> Char.chr (Random.State.int random 256)) in
    check s
  done;
  Printf.printf "Jsonm differential: %d token/error/position comparisons passed.\n" !count
'''

def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('upstream', type=Path)
    source = p.parse_args().upstream / 'src/jsonm.ml'
    if not source.is_file():
        p.error(f'missing {source}')
    port = Path(__file__).resolve().parent
    with tempfile.TemporaryDirectory(prefix='jsonm-diff-') as directory:
        root = Path(directory)
        shutil.copytree(port, root / 'jsonm')
        (root / 'dune-project').write_text('(lang dune 3.21)\n(name jsonm_diff)\n')
        (root / 'dune').write_text('(executable (name compare) (libraries jsonm uutf))\n')
        (root / 'stock.ml').write_text(source.read_text())
        (root / 'compare.ml').write_text(COMPARE)
        subprocess.run(['dune', 'exec', '--root', str(root), '--profile', 'release', './compare.exe'], cwd=root, check=True)

if __name__ == '__main__':
    main()
