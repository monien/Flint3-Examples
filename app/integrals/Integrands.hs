module Integrands (
  integrands
) where

import Foreign.Ptr (nullPtr)
import Control.Monad
import Data.Number.Flint

integrands = zip description hFunctions

testFunction f x = do
  withNewAcb $ \res -> do
    withNewAcb $ \t -> do
      acb_set_d t x
      putStr"testFunction: arg = "
      acb_printn t 16 arb_str_no_radius
      putStr "\n"
      flag <- f res t nullPtr 0 1024
      putStr "testFunction: res = "
      acb_printn res 16 arb_str_no_radius
      putStr "\n"
  return ()
  
f_sin res z param order prec = do
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

hFunctions :: [CAcbCalcFunc]
hFunctions =
  [
    f_sin
  , f_atanderiv
  , f_atanderiv
  , f_circle
  , f_rump
  , f_floor
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

description = 
  [ "int_0^100 sin(x) dx"
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
  , "int_0^{inf} 1/gamma(x) dx   (using domain truncation)"
  ]

