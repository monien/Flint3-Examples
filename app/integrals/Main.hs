import System.IO.Unsafe
import Options.Applicative
import Control.Monad
import Foreign.C.Types

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Calculate integrals using acb_calculate."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

-- run params@(Parameters from to prec reltol abstol
--                        twice heap verbosity
--                        deg eval depth num_threads) = do
  -- when (to > length integrands) $ error "index to large."

run params = do
  print params
  flint_set_num_threads $ num_threads params
  
data Parameters = Parameters {
    from        :: Int 
  , to          :: Int
  , prec        :: CULong
  , reltol      :: String
  , abstol      :: String
  , twice       :: Bool
  , heap        :: Bool
  , verbosity   :: Int
  , deg         :: CLong
  , eval        :: CLong
  , depth       :: CLong
  , num_threads :: CInt
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> option auto (
      help "calculate integrals from list from index."
   <> long "from"
   <> value 0
   <> metavar "start")
  <*> option auto (
      help "calculate integrals from list to index."
   <> long "end"
   <> value (length integrands)
   <> metavar "to")
  <*> option auto (
      help "precision in bits (default p = 64)."
   <> long "prec"
   <> short 'p'
   <> value 64
   <> metavar "p")
  <*> option str (
      help "approximate relative accuracy goal (p)."
   <> long "realtol"
   <> value "64"
   <> metavar "reltol")
  <*> option str (
      help "approximate absolute accuracy goal (default 2^-p)."
   <> long "abstol"
   <> value "2^-64"
   <> metavar "abstol")
  <*> switch (
      help "run twice (to see overhead of computing nodes)."
   <> long "twice")
  <*> switch (
      help "use heap for subinterval queue."
   <> long "heap")
  <*> option auto (
      help "verbosity level."
   <> short 'v'
   <> long "verbosity"
   <> value 0
   <> metavar "verbosity")
  <*> option auto (
      help "use quadrature degree up to n"
   <> long "deg"
   <> value 4
   <> metavar "degree")
  <*> option auto (
      help "limit number of function evaluations to n."
   <> long "eval"
   <> metavar "eval")
  <*> option auto (
      help "limit subinterval queue size to n"
   <> long "depth"
   <> metavar "depth")
  <*> option auto (
      help "number of threads"
   <> long "threads"
   <> value 1 
   <> metavar "THREADS")

-- parseArbMag = eitherReader $ \s -> do
  
-- data FluxCapacitor = ...

-- parseFluxCapacitor :: ReadM FluxCapacitor
-- parseFluxCapacitor = eitherReader $ \s -> ...

-- option parseFluxCapacitor ( long "flux-capacitor" )

-- parseTile :: Parser Tile
-- parseTile = do
--   x <- decimal
--   char ','
--   y <- decimal
--   char ':'
--   t <- decimal `sepBy` (char ',')
--   char ';'
--   return $ Tile x y t
  
-- integrands ------------------------------------------------------------------

f_sin z res param order prec = do
  when (order > 1) $ error "f_sin: Would be needed for Taylor method."
  acb_sin res z prec
  return 0

f_floor res z param order prec = do
  when (order > 1) $ error "f_floor: Would be needed for Taylor method."
  acb_real_floor res z 1 prec
  return 0

f_circle res z param order prec = do
  when (order > 1) $ error "f_circle: Would be needed for Taylor method."
  acb_one res 
  acb_submul res z z prec 
  acb_real_sqrtpos res res 1 prec
  return 0

f_atanderiv res z param order prec = do
  when (order > 1) $ error "f_circle: Would be needed for Taylor method."
  acb_mul res z z prec
  acb_add_ui res res 1 prec
  acb_inv res res prec
  return 0

f_rump res z param order prec = do
  when (order > 1) $ error "f_rump: Would be needed for Taylor method."
  acb_exp res z prec
  acb_add res res z prec
  acb_sin res res prec
  return 0

f_helfgott res z param order prec = do
  when (order > 1) $ error "f_helfgott: Would be needed for Taylor method."
  acb_add_si res z 10 prec
  acb_mul res res z prec
  acb_add_si res res 19 prec
  acb_mul res res z prec
  acb_add_si res res (-6) prec
  acb_mul res res z prec
  acb_add_si res res (-6) prec
  acb_real_abs res res 1 prec
  flag <- acb_is_finite res
  when (flag == 1) $ do
    withNewAcb $ \t -> do
      acb_exp t z prec
      acb_mul res res t prec
    return ()
  return 0

f_zeta res z param order prec = do
  when (order > 1) $ error "f_zeta: Would be needed for Taylor method."
  acb_zeta res z prec
  return 0

integrands =
  [
    f_sin
  , f_floor
  , f_circle
  , f_atanderiv
  , f_rump
  , f_helfgott
  , f_zeta
  -- , f_essing2
  -- , f_essing
  -- , f_factorial1000
  -- , f_gamma
  -- , f_sin_plus_small
  -- , f_exp
  -- , f_gaussian
  -- , f_monster
  -- , f_spike
  -- , f_sech
  -- , f_sech3
  -- , f_log_div1p
  -- , f_log_div1p_transformed
  -- , f_elliptic_p_laurent_n
  -- , f_zeta_frac
  -- , f_lambertw
  -- , f_max_sin_cos
  -- , f_er, f_bent
  -- , f_airy_ai
  -- , f_horror
  -- , f_sqrt
  -- , f_rsqrt
  -- , f_rgamma
  -- , f_gaussian_twist
  -- , f_exp_airy
  -- , f_sin_cos_frac
  -- , f_sin_near_essing
  -- , f_scaled_bessel
  ]

description = [
    "int_0^100 sin(x) dx"
  , "4 int_0^1 1/(1+x^2) dx"
  , "2 int_0^{inf} 1/(1+x^2) dx   (using domain truncation)"
  , "4 int_0^1 sqrt(1-x^2) dx"
  , "int_0^8 sin(x+exp(x)) dx"
  , "int_1^101 floor(x) dx"
  , "int_0^1 |x^4+10x^3+19x^2-6x-6| exp(x) dx"
  , "1/(2 pi i) int zeta(s) ds  (closed path around s = 1)"
  , "int_0^1 sin(1/x) dx  (slow convergence, use -heap and/or -tol)"
  , "int_0^1 x sin(1/x) dx  (slow convergence, use -heap and/or -tol)"
  , "int_0^10000 x^1000 exp(-x) dx"
  , "int_1^{1+1000i} gamma(x) dx"
  , "int_{-10}^{10} sin(x) + exp(-200-x^2) dx"
  , "int_{-1020}^{-1010} exp(x) dx  (use -tol 0 for relative error)"
  , "int_0^{inf} exp(-x^2) dx   (using domain truncation)"
  , "int_0^1 sech(10(x-0.2))^2 + sech(100(x-0.4))^4 + sech(1000(x-0.6))^6 dx"
  , "int_0^8 (exp(x)-floor(exp(x))) sin(x+exp(x)) dx  (use higher -eval)"
  , "int_0^{inf} sech(x) dx   (using domain truncation)"
  , "int_0^{inf} sech^3(x) dx   (using domain truncation)"
  , "int_0^1 -log(x)/(1+x) dx   (using domain truncation)"
  , "int_0^{inf} x exp(-x)/(1+exp(-x)) dx   (using domain truncation)"
  , "int_C wp(x)/x^(11) dx   (contour for 10th Laurent coefficient of Weierstrass p-function)"
  , "N(1000) = count zeros with 0 < t <= 1000 of zeta(s) using argument principle"
  , "int_0^{1000} W_0(x) dx"
  , "int_0^pi max(sin(x), cos(x)) dx"
  , "int_{-1}^1 erf(x/sqrt(0.0002)*0.5+1.5)*exp(-x) dx"
  , "int_{-10}^10 Ai(x) dx"
  , "int_0^10 (x-floor(x)-1/2) max(sin(x),cos(x)) dx"
  , "int_{-1-i}^{-1+i} sqrt(x) dx"
  , "int_0^{inf} exp(-x^2+ix) dx   (using domain truncation)"
  , "int_0^{inf} exp(-x) Ai(-x) dx   (using domain truncation)"
  , "int_0^pi x sin(x) / (1 + cos(x)^2) dx"
  , "int_0^3 sin(0.001 + (1-x)^2)^(-3/2)) dx  (slow convergence, use higher -eval)"
  , "int_0^{inf} exp(-x) I_0(x/3)^3 dx   (using domain truncation)"
  , "int_0^{inf} exp(-x) I_0(x/15)^{15} dx   (using domain truncation)"
  , "int_{-1-i}^{-1+i} 1/sqrt(x) dx"
  , "int_0^{inf} 1/gamma(x) dx   (using domain truncation)"]
