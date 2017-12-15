# MiniLucy

### Compilation

To compile this program, you need an installation of OCaml with the Menhir library.

Then :

```
make
```

### Cleaning directory

```
make clean
```

### Execution

```
./minilucy -v file[.lus|.elus]
```

## Lustre Compiler to imperative language.

There are two target language : C and OCaml.
The Lustre to C language compiler follows the step described in the paper "Clock-directed Modular Code Generation of Synchronous Data-flow Languages".
The Lustre to OCaml language compiler add parallel execution of instruction.

To run the automatic test :

```
# For testing the to OCaml compiler
make test

# For testing the to C compiler
make test_c
```

## Extended Lustre Compiler to mini-lustre language

* Paper : "A Conservative Extension of Synchronous Data-flow with State Machines"

The compiler takes an elus file and produces a typed and clocked lustre ast (see ast_lustre.ml for description).

Right now, we have no example that works from end-to-end.

To test the extended lustre compiler run on its own run the execution command on a .elus file, don't forget the `-v` flag to see the result of the compilation. The extended lustre steps are : `SYNTACTIC ANALYSIS ELUS`, `TYPING ELUS`, `CLOCKING ELUS` and `TRANSLATION TO LUS`.
You can find 3 examples in `tests/syntax/good`, they are named `automaton.elus`, `match.elus` and `chrono.elus`.

## Authors

* **Nicolas ASSOUAD** - *Initial work* - [fondation451](https://github.com/fondation451)
* **Ismail LAHKIM BENNANI** - *Initial work* - [ismailbennani](https://github.com/ismailbennani)
