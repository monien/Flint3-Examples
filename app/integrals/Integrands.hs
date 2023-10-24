module Integrands (
  description
, makeFunPtr
-- * Functions
, f_airy_ai
, f_atanderiv
, f_circle
, f_elliptic_p_laurent_n
, f_erf_bent
, f_essing
, f_essing2
, f_exp
, f_exp_airy
, f_factorial1000
, f_floor
, f_gamma
, f_gaussian
, f_gaussian_twist
, f_helfgott
, f_horror
, f_lambertw
, f_log_div1p
, f_log_div1p_transformed
, f_max_sin_cos
, f_monster
, f_rgamma
, f_rsqrt
, f_rump
, f_scaled_bessel
, f_sech
, f_sech3
, f_sin
, f_sin_cos_frac
, f_sin_near_essing
, f_sin_plus_small
, f_spike
, f_sqrt
, f_zeta
, f_zeta_frac
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Control.Monad

import Data.Number.Flint

integrands = zip description hFunctions

foreign import ccall safe "wrapper"
  makeFunPtr :: CAcbCalcFunc -> IO (FunPtr CAcbCalcFunc)

--------------------------------------------------------------------------------

-- f(z) = Ai(z)
f_airy_ai res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_hypgeom_airy res nullPtr nullPtr nullPtr z prec
  return 0

-- f(z) = 1/(1+z^2)
f_atanderiv res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_mul res z z prec
  acb_add_ui res res 1 prec
  acb_inv res res prec
  return 0

-- f(z) = sqrt(1-z^2)
f_circle res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_one res 
  acb_submul res z z prec 
  acb_real_sqrtpos res res (if order /= 0 then 1 else 0) prec
  return 0

f_elliptic_p_laurent_n res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  n <- peek (castPtr param) :: IO CLong
  withNewAcb $ \tau -> do
    acb_onei tau
    acb_modular_elliptic_p res z tau prec
    acb_pow_si tau z (-n-1) prec
    acb_mul res res tau prec
  return 0

-- f(z) = erf(z/sqrt(0.0002)*0.5 +1.5)*exp(-z)
-- example provided by Silviu-Ioan Filip
f_erf_bent res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_set_ui t 1250
    acb_sqrt t t prec
    acb_mul t t z prec
    acb_set_d res 1.5
    acb_add res res t prec
    acb_hypgeom_erf res res prec

    acb_neg t z
    acb_exp t t prec
    acb_mul res res t prec
  return 0

-- f(z) = sin(1/z)
-- Assume z on real interval
f_essing res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  isReal <- (==1) <$> acb_is_real z
  containsZero <- (==1) <$> arb_contains_zero (acb_realref z)
  if order == 0 && isReal && containsZero then do
    acb_zero res
    mag_one (arb_radref (acb_realref res))
  else do
    acb_inv res z prec
    acb_sin res res prec
  return 0

-- f(z) = z*sin(1/z)
-- Assume z on real interval
f_essing2 res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  isReal <- (==1) <$> acb_is_real z
  containsZero <- (==1) <$> arb_contains_zero (acb_realref z)
  if order == 0 && isReal && containsZero then do
    acb_zero res
    mag_one (arb_radref (acb_realref res))
  else do
    acb_inv res z prec
    acb_sin res res prec
  acb_mul res res z prec
  return 0

-- f(z) = exp(z)
f_exp res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_exp res z prec
  return 0

-- f(z) = exp(-z) Ai(-z)
f_exp_airy res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_neg t z
    acb_hypgeom_airy res nullPtr nullPtr nullPtr t prec
    acb_exp t t prec
    acb_mul res res t prec
  return 0

-- f(z) = exp(-z)*z^1000
f_factorial1000 res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_pow_ui t z 1000 prec
    acb_neg res z
    acb_exp res res prec
    acb_mul res res t prec
  return 0

-- f(z) = floor(z)
f_floor res z param order prec = do
  when (order > 1) $ error "f_floor: Would be needed for Taylor method."
  acb_real_floor res z (if order /= 0 then 1 else 0) prec
  return 0

-- f(z) = gamma(z)
f_gamma res z param order prec = do
  when (order > 1) $ error "f_floor: Would be needed for Taylor method."
  acb_gamma res z prec
  return 0

-- f(z) = exp(-z^2)
f_gaussian res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_mul z z z prec
  acb_neg z z
  acb_exp res z prec
  return 0

-- f(z) = exp(-z^2+iz)
f_gaussian_twist res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_mul_onei res z
  acb_submul res z z prec
  acb_exp res res prec
  return 0

-- | /f_helfgott/ /res/ /z/ /param/ /order/ /prec/
--
-- f(z) = |z^4 + 10z^3 + 19z^2 - 6z - 6| exp(z)
-- (for real z)
-- Helfgott's integral on MathOverflow
f_helfgott res z param order prec = do
  when (order > 1) $ error "f_helfgott: Would be needed for Taylor method."
  acb_add_si res z 10 prec
  acb_mul res res z prec
  acb_add_si res res 19 prec
  acb_mul res res z prec
  acb_add_si res res (-6) prec
  acb_mul res res z prec
  acb_add_si res res (-6) prec
  acb_real_abs res res (if order /= 0 then 1 else 0) prec
  isFinite <- (==1) <$> acb_is_finite res
  when isFinite $ do
    withNewAcb $ \t -> do
      acb_exp t z prec
      acb_mul res res t prec
    return ()
  return 0

f_horror res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \s -> do
    withNewAcb $ \t -> do
      acb_real_floor res z (if order /= 0 then 1 else 0) prec
      isFinite <- (==1) <$> acb_is_finite res
      when isFinite $ do
        acb_sub res z res prec
        acb_set_d t 0.5
        acb_sub res res t prec
        acb_sin_cos s t z prec
        acb_real_max s s t (if order /= 0 then 1 else 0) prec
        acb_mul res res s prec
  return 0

f_lambertw res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  bits <- acb_rel_accuracy_bits z
  let prec' = min prec (bits + 10)
  withNewAcb $ \t -> do
    when (order /= 0 ) $ do
      arb_const_e (acb_realref t) prec'
      acb_inv t t prec'
      acb_add t t z prec'
      containsZero <- (==1) <$> arb_contains_zero         (acb_imagref t)
      nonPositive  <- (==1) <$> arb_contains_nonpositive (acb_realref t)
      when (containsZero && nonPositive) $ acb_indeterminate t
      return ()
    isFinite <- (==1) <$> acb_is_finite t
    if isFinite then do
      withNewFmpz $ \k -> do
        acb_lambertw res z k 0 prec'
      return ()
    else do
      acb_indeterminate res
      return ()
  return 0

-- f(z) = -log(z) / (1 + z)
f_log_div1p res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_add_ui t z 1 prec
    acb_log res z prec
    acb_div res res t prec
    acb_neg res res
  return 0

-- f(z) = z exp(-z) / (1 + exp(-z))
f_log_div1p_transformed res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_neg t z
    acb_exp t t prec
    acb_add_ui res t 1 prec
    acb_div res t res prec
    acb_mul res res z prec
  return 0

-- f(z) = max(sin(z), cos(z))
f_max_sin_cos res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \s -> do
    withNewAcb $ \c -> do
      acb_sin_cos s c z prec
      acb_real_max res s c (if order /= 0 then 1 else 0) prec
  return 0

f_monster res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_exp t z prec
    acb_real_floor res t (if order /= 0 then 1 else 0) prec
    isFinite <- (==1) <$> acb_is_finite res
    when isFinite $ do
      acb_sub res t res prec
      acb_add t t z prec
      acb_sin t t prec
      acb_mul res res t prec
  return 0

-- f(z) = rgamma(z)
f_rgamma res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_rgamma res z prec
  return 0

-- f(z) = rsqrt(z)
f_rsqrt res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_rsqrt_analytic res z (if order /= 0 then 1 else 0) prec
  return 0

-- f(z) = sin(z + exp(z)) -- Rump's oscillatory example
f_rump res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_exp res z prec
  acb_add res res z prec
  acb_sin res res prec
  return 0

-- f(z) = exp(-z) (I_0(z/k))^k, from Bruno Salvy
f_scaled_bessel res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  k <- peek (castPtr param)
  withNewAcb $ \nu -> do
    acb_init nu
    acb_div_ui res z k prec
    acb_hypgeom_bessel_i_scaled res nu res prec
    acb_pow_ui res res k prec
    acb_clear nu
  return 0

-- f(z) = sech(z)
f_sech res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_sech res z prec
  return 0

-- f(z) = sech^3(z)
f_sech3 res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_sech res z prec
  acb_cube res res prec
  return 0

-- f(z) = sin(z)
f_sin res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_sin res z prec
  return 0

-- f(z) = z sin(z) / (1 + cos(z)^2)
f_sin_cos_frac res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \s -> do
    withNewAcb $ \c -> do
      acb_sin_cos s c z prec
      acb_mul c c c prec
      acb_add_ui c c 1 prec
      acb_mul s s z prec
      acb_div res s c prec
  return 0

-- f(z) = sin((1/1000 + (1-z)^2)^(-3/2)), example from
-- Mioara Jolde's thesis (suggested by Nicolas Brisebarre)
f_sin_near_essing res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    withNewAcb $ \u -> do
      acb_sub_ui t z 1 prec
      acb_neg t t
      acb_mul t t t prec
      acb_one u
      acb_div_ui u u 1000 prec
      acb_add t t u prec
      acb_set_d u (-1.5)
      acb_pow_analytic t t u (if order /= 0 then 1 else 0) prec
      acb_sin res t prec
  return 0

-- f(z) = sin(z) + exp(-200-z^2)
f_sin_plus_small res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \t -> do
    acb_mul t z z prec
    acb_add_ui t t 200 prec
    acb_neg t t
    acb_exp t t prec
    acb_sin res z prec
    acb_add res res t prec
  return 0

-- f(z) = sech(10(x-0.2))^2 + sech(100(x-0.4))^4 + sech(1000(x-0.6))^6
f_spike res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  withNewAcb $ \a -> do
    withNewAcb $ \b -> do
      withNewAcb $ \c -> do
        acb_mul_ui a z 10 prec
        acb_sub_ui a a 2 prec
        acb_sech a a prec
        acb_pow_ui a a 2 prec

        acb_mul_ui b z 100 prec
        acb_sub_ui b b 40 prec
        acb_sech b b prec
        acb_pow_ui b b 4 prec

        acb_mul_ui c z 1000 prec
        acb_sub_ui c c 600 prec
        acb_sech c c prec
        acb_pow_ui c c 6 prec

        acb_add res a b prec
        acb_add res res c prec
  return 0

-- f(z) = sqrt(z)
f_sqrt res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_sqrt_analytic res z (if order /= 0 then 1 else 0) prec
  return 0

-- f(z) = zeta(z)
f_zeta res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  acb_zeta res z prec
  return 0

-- f(z) = zeta'(z) / zeta(z)
f_zeta_frac res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  t <- _acb_vec_init 2
  acb_dirichlet_zeta_jet t z 0 2 prec
  acb_div res (t `advancePtr` 1) t prec
  _acb_vec_clear t 2
  return 0

-- examples --------------------------------------------------------------------

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

--------------------------------------------------------------------------------

hFunctions :: [CAcbCalcFunc]
hFunctions =
  [
    f_airy_ai
  , f_atanderiv
  , f_circle
  , f_elliptic_p_laurent_n
  , f_erf_bent
  , f_essing
  , f_essing2
  , f_exp
  , f_exp_airy
  , f_factorial1000
  , f_floor
  , f_gamma
  , f_gaussian
  , f_gaussian_twist
  , f_helfgott
  , f_horror
  , f_lambertw
  , f_log_div1p
  , f_log_div1p_transformed
  , f_max_sin_cos
  , f_monster
  , f_rgamma
  , f_rsqrt
  , f_rump
  , f_scaled_bessel
  , f_sech
  , f_sech3
  , f_sin
  , f_sin_cos_frac
  , f_sin_near_essing
  , f_sin_plus_small
  , f_spike
  , f_sqrt
  , f_zeta
  , f_zeta_frac
  ]

lift :: (Ptr CAcb -> Ptr CAcb -> Ptr () -> CLong -> CLong -> IO ())
     ->  Ptr CAcb -> Ptr CAcb -> Ptr () -> CLong -> CLong -> IO CInt
lift f res z param order prec = do
  when (order > 1) $ error "order > 1 would be needed for Taylor method."
  f res z param order prec
  return 0

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
