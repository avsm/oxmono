#!/bin/sh -ex

export OPAMROOT=`pwd`/_opamroot
export OPAMYES=1

if [ ! -d _opam ]; then
  opam init -any --bare --disable-sandboxing
fi 

if [ ! -d _local ]; then
  opam switch create . --repos lox=git+https://github.com/oxcaml/opam-repository.git,default -y
fi

opam exec -- dune build --profile=release
