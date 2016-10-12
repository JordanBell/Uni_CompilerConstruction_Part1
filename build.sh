#!/bin/bash
cd src
ocamlbuild -use-menhir -use-ocamlfind Parser.native
cd ../

