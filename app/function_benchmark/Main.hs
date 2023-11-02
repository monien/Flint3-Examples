import Text.Printf
import System.CPUTime
import Foreign.C.Types
import Control.Monad

import Data.Number.Flint

main = do
  withNewArb $ \x -> do
    withNewArb $ \y -> do
      withNewArb $ \res -> do
        forM_ funs $ \(fun, desc, limit) -> do
          printf "\n%s\n" desc
          forM_ (takeWhile (<=limit) $ map (10^) [1..]) $ \n -> do
            let prec = round (fromIntegral n / logBase 10 2 + 1)
            printf "%12d" (toInteger n)
            arb_sqrt_ui x 2 prec
            arb_sub_ui x x 1 prec
            
            flint_set_num_threads 1
            timeItNamed " 1 thread  " $ fun res x n prec

            putStr " "

            flint_set_num_threads 8
            timeItNamed " 8 threads " $ fun res x n prec

            putStr "\n"
            

funs = 
  [ ( f0, "const_pi, n digits", 10^8)
  , ( f1, "const_euler, n digits", 10^7)
  , ( f2, "exp(x), n digits", 10^7)
  , ( f3, "log(x), n digits", 10^7)
  , ( f4, "sin(x), n digits", 10^7)
  , ( f5, "atan(x), n digits", 10^7)
  , ( f6, "erf(x), n digits", 10^6)
  , ( f7, "gamma(x), n digits", 10^5)
  , ( f8, "zeta(x), n digits", 10^4)
  , ( f9, "eta(ix), n digits", 10^6)
  , ( f10, "bernoulli(n)", 10^6)
  , ( f11, "partitions(n^2)", 10^7)
  ]

f0 res x n prec = arb_const_pi res prec
f1 res x n prec = arb_const_euler res prec
f2 res x n prec = arb_exp res x prec
f3 res x n prec = arb_log res x prec
f4 res x n prec = arb_sin res x prec
f5 res x n prec = arb_atan res x prec
f6 res x n prec = arb_hypgeom_erf res x prec
f7 res x n prec = arb_gamma res x prec
f8 res x n prec = arb_zeta res x prec

f9 res x n prec = do
  withNewAcb $ \z -> do
    arb_set (acb_imagref z) x
    acb_modular_eta z z prec
    arb_set res (acb_realref z)
  return ()

-- ?? 
f10 res x n prec = do
  withNewFmpq $ \t -> do
    bernoulli_fmpq_ui t (fromIntegral n)
  return ()
  
-- ??
f11 res x n prec = do
  withNewFmpz $ \t -> do
    fmpz_set_si t n
    fmpz_mul t t t
    partitions_fmpz_fmpz t t 0
  return ()
  
timeItNamed s f = do
  t0 <- getCPUTime
  result <- f 
  t1 <- getCPUTime
  let dt = fromIntegral (t1 - t0) / 10^9  :: Double
  printf "%s: %12.4f ms" s dt
  