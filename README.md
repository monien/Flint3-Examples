# Flint2-Examples

Examples for the [Flint2](https://hackage.haskell.org/package/Flint2)
Haskell wrapper of the [Flint](https://flintlib.org) library. 

Most of these are directly "translated" from C to Haskell so they are
written using "imperative" constructs in Haskell to demonstrate the
use of the Haskell wrapper (not intended as examples of 
elegant Haskell code). The structure of programs follows closely the
structure of the C code (with some exceptions:
e.g. swinnerton_dyer_poly uses a different algorithm, logistic uses a
monad transformer instead of a for loop ...)

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
* fmpz_mpoly_factor
* fmpz_poly_factor_zassenhaus
* fpwrap
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
* stirling_matrix
* swinnerton_dyer_poly
* taylor_integrals
* zeta_zeros
