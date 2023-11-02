# Flint2-Examples

## Introduction

Examples for the [Flint2](https://hackage.haskell.org/package/Flint2)
Haskell wrapper of the [Flint](https://flintlib.org) library. All of
these examples were originally coded in C by the flintlib
collaboration.

Most of these are directly "translated" from C to Haskell so they are
written using "imperative" constructs in Haskell to demonstrate the
use of the Haskell wrapper (not intended as examples of 
elegant Haskell code). The structure of programs follows closely the
structure of the C code (with some exceptions:
e.g. swinnerton_dyer_poly uses a different algorithm, logistic uses a
monad transformer instead of a for loop ...)

There are some basic benchmarks in some of the programs. In some 
cases multithreading is available. Checkout the options by typing 
prog -h. 

## Installation

- Install the C-library available from [Flint](https://flintlib.org). 
   There are packages available for various operating systems.

- Install the Haskell interface with

```bash
cabal install Flint2 --lib
```

- Check that your Cabal default local bin directory does not contain any
  conflicting names (see below for a list of binaries)!
- Install the examples with 

```bash
cabal install Flint2-Examples
```

## Quick start

A simple example would factorization of integers. To find out what
options are available use -h:

```bash
factor_integers -h
```

which prints

```bash
Factor integers.

Usage: factor_integer INTEGER [-t|--threads THREADS] [--timing]

  Factor integers.

Available options:
  INTEGER                  Integer given as expression.
  -t,--threads THREADS     number of threads
  --timing                 timing
  -h,--help                Show this help text
```

Now typing 

```bash
factor_integer 472314979327
```
returns
```bash
[(97,1),(433,1),(11245327,1)]
```

## A more advanced example: complex_plot
Try complex_plot by typing:

```bash
complex_plot
```
This plots  the phase of the *Klein invariant* in the upper half
plane. This example has many more options. Using the help option
one obtains

```bash
Plotting special functions in the complex plane.

Usage: complex_plot [--xa XA] [--xb XB] [--ya YA] [--yb YB] [--width WIDTH] 
                    [--height HEIGHT] [-c|--color-mode COLOR-MODE] 
                    [-f|--function FUNCTION] [-o|--output IMAGE-FILE]

  plotting special functions.

Available options:
  -c,--color-mode COLOR-MODE
                           possible values: 0 .. 6
  -f,--function FUNCTION   possible values: agm, ai, barnesg, besseli, besselj,
                           besselk, bessely, bi, digamma, ellipp, ellipsigma,
                           ellipzeta, erf, fresnelc, fresnels, gamma, lgamma,
                           modeta, modetaq, modj, modjq, modlambda, modlambdaq,
                           zeta
  -o,--output IMAGE-FILE   write output to IMAGE-FILE
  -h,--help                Show this help tex
```


## Source code

To study the source code download the code from Github with. More
documentation will hopefully available soon. Also check the flintlib site.

```bash
git clone https://github.com/monien/Flint2-Examples.git
```


## List of available examples

Installed are

* bernoulli
* binet
* calcium
* class_poly
* complex_plot
* crt
* delta_qexp
* dft
* elementary
* factor_integer
* fmpq_poly
* fmpz_mod_poly
* fmpz_mpoly_factor
* fmpz_poly_factor_zassenhaus
* fpwrap
* fq_poly
* hilbert_matrix
* hilbert_matrix_ca
* integrals
* keiper_li
* l_central
* l_values
* logistic
* machin
* multi_crt
* padic
* partitions
* pi_digits
* primegen
* qadic
* radix
* stirling_matrix
* swinnerton_dyer_poly
* taylor_integrals
* zeta_zeros
