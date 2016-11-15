#!/bin/bash
clear
cd src
ocamlbuild -use-menhir -use-ocamlfind Main.native
ocamlbuild -use-menhir -use-ocamlfind Interpreter.native
ocamlbuild -use-menhir -use-ocamlfind CodeGenerator.native
ocamlbuild -use-menhir -use-ocamlfind CodeGenerator_x86.native
cd ../
