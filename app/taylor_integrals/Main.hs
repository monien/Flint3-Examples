import Options.Applicative
import Control.Monad

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array

import Data.Number.Flint

main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Compute integrals using d decimal digits of precision."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters digits) = do
  print params
  let goal = round (fromIntegral digits / logBase 10 2)
      prec = round (1.1 * fromIntegral goal)
  withNewAcb $ \r -> do
    withNewAcb $ \s -> do
      withNewAcb $ \a -> do
        withNewAcb $ \b -> do
          withNewArf $ \inr -> do
            withNewArf $ \outr -> do
              -- Sin integrals
              putStrLn $ replicate 64 '-'
              putStrLn "Integral of sin(t) from 0 to 100."
              arf_set_d inr 0.125
              arf_set_d outr 1.0
              acb_set_si a 0
              acb_set_si b 100
              f <- makeFunPtr sinx
              acb_calc_integrate_taylor r f nullPtr a b inr outr goal prec
              putStrLn "RESULT:"
              acb_printn r digits 0; putStr "\n"
              -- Elliptic integral
              putStrLn $ replicate 64 '-'
              putStrLn "Elliptic integral F(phi, m) = integral of \
                       \1/sqrt(1 - m*sin(t)^2)"
              arf_set_d inr 0.125
              arf_set_d outr 1.0
              acb_set_si a 0
              acb_set_si b 6
              f <- makeFunPtr elliptic
              acb_calc_integrate_taylor r f nullPtr a b inr outr goal prec
              acb_set_si a 6
              arb_set_si (acb_realref b) 6
              arb_set_si (acb_imagref b) 6
              acb_calc_integrate_taylor s f nullPtr a b inr outr goal prec
              acb_add r r s prec
              putStrLn "RESULT:"
              acb_printn r digits 0; putStr "\n"
              -- Bessel integral
              putStrLn $ replicate 64 '-'
              putStrLn "Bessel function J_n(z) = (1/pi) * integral of \
                       \cos(t*n - z*sin(t))"
              arf_set_d inr 0.1
              arf_set_d outr 0.5
              let prec' = 3*prec
              acb_set_si a 0
              acb_const_pi b prec'
              f <- makeFunPtr bessel
              acb_calc_integrate_taylor r f nullPtr a b inr outr prec prec'
              acb_div r r b prec
              putStrLn "RESULT:"
              acb_printn r digits 0; putStr "\n"
              
                           
  
data Parameters = Parameters {
    digits :: CLong 
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "compute integrals using d decimal digits of precision."
   <> metavar "d")

--------------------------------------------------------------------------------

foreign import ccall safe "wrapper"
  makeFunPtr :: CAcbCalcFunc -> IO (FunPtr CAcbCalcFunc)

sinx :: Ptr CAcb -> Ptr CAcb -> Ptr () -> CLong -> CLong -> IO CInt
sinx out inp params order prec = do
  let xlen = min 2 order
  acb_set out inp
  when (xlen > 1) $ do acb_one (out `advancePtr` 1)
  _acb_poly_sin_series out out xlen order prec
  return 0

elliptic :: Ptr CAcb -> Ptr CAcb -> Ptr () -> CLong -> CLong -> IO CInt
elliptic out inp params order prec = do
  t <- _acb_vec_init order
  acb_set t inp
  when (order > 1) $ do acb_one (t `advancePtr` 1)
  _acb_poly_sin_series t t (min 2 order) order prec
  _acb_poly_mullow out t order t order order prec
  _acb_vec_scalar_mul_2exp_si t out order (-1)
  acb_sub_ui t t 1 prec
  _acb_vec_neg t t order
  _acb_poly_rsqrt_series out t order order prec
  _acb_vec_clear t order
  return 0

bessel :: Ptr CAcb -> Ptr CAcb -> Ptr () -> CLong -> CLong -> IO CInt
bessel out inp params order prec = do

  t <- _acb_vec_init order

  withNewAcb $ \z -> do
    acb_set t inp
    when (order > 1) $ do acb_one (t `advancePtr` 1)

    let n = 10
    arb_set_si (acb_realref z) 20
    arb_set_si (acb_imagref z) 10

    -- z sin t
    _acb_poly_sin_series out t (min 2 order) order prec
    _acb_vec_scalar_mul out out order z prec

    -- t n
    _acb_vec_scalar_mul_ui t t (min 2 order) n prec

    _acb_poly_sub out t (min 2 order) out order prec

    _acb_poly_cos_series out out order order prec

  _acb_vec_clear t order

  return 0

