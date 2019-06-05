# MiniML Interpreter

「プログラミング言語処理系」と「計算機科学実験及演習３」の履修者へ：この README は課題を行うにあたって重要な情報を含んでいるので，ちゃんと全部読むこと．

This directory contains the source files for MiniML interpreter, an
interpreter of a subset of OCaml used in the class "Implementation of
Programming Languages" provided by the Department of Engineering,
Kyoto University.

## Required software/library

You need OCaml (http://ocaml.org/) and a parser generator Menhir
(http://gallium.inria.fr/~fpottier/menhir/) to build this interpreter.

We strongly recommend installing opam (https://opam.ocaml.org/), the
standard package manager for OCaml as of 2017. You can install many
useful libraries with opam.

- Read https://opam.ocaml.org/doc/Install.html for installing opam to
  your computer. (The computer system at the Keisanki course already
  has opam.)
- (You need to do this step only once.) Type `opam init` first. To
  install menhir, type `opam install menhir dune ounit`.
- To update the package list, type `opam update`.
- To upgrade all the packages installed to your system, type `opam upgrade`.
- For more detailed usage, see https://opam.ocaml.org/doc/Usage.html

## Building and invoking MiniML interpreter

Software written in OCaml

- Type `dune build` to build.
- Type `dune exec miniml` to invoke the interpreter.
- Type `dune runtest` to run tests


## Files

This directory contains the following files.

- `main.ml`: The entry point of the entire interpreter.
- `cui.ml`: The function of the CUI.
- `src/syntax.ml`: Definition of the type for MiniML abstract syntax trees.
- `src/eval.ml`: The functions that evaluate MiniML expressions/declarations.
- `src/parser.mly`: The definition of the MiniML parser.
- `src/lexer.mll`: The definition of the MiniML lexer.
- `src/environment.mli`: The interface of the ADT (abstract data type) for
  an environment -- an important data structure often used in
  interpreters and compilers.
- `src/environment.ml`: The implementation of the ADT for an environment.
- `src/mySet.mli`: The interface of the ADT for a set.
- `src/mySet.ml`: The implementation of the ADT for a set.
- `src/typing.ml`: The implementation of MiniML type inference (to be
  implemented by students.)

- `test/dune`: The config file of `dune runtest`. 
  - After implementing the exercises, remove the corresponding symbol `;`.