# MiniLucy

Lustre Compiler to imperative language.

## Compilation

To compile this program, you need an installation of OCaml with the Menhir library.

Then :

```
make
```

## Cleaning directory

```
make clean
```



## Running Test

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

## Authors

* **Nicolas ASSOUAD** - *Initial work* - [fondation451](https://github.com/fondation451)
* **Ismail LAHKIM BENNANI** - *Initial work* - [ismailbennani](https://github.com/ismailbennani)
