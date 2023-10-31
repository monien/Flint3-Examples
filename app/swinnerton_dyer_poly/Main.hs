import Control.Monad
import Data.Word
import Data.Bits

import System.TimeIt
import Options.Applicative

import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array

import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  desc = "Computes the coefficients of the Swinnerton-Dyer polynomial."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n) = do
  print params
  swinnerton_dyer_poly n
  
-- Parser Parameters -----------------------------------------------------------

data Parameters = Parameters {
  n :: Int
  } deriving (Show, Eq)

parameters :: Parser Parameters
parameters = Parameters
  <$> argument (0 `between` 20) (
      help "n"
   <> metavar "n")

between a b = eitherReader $ \s -> do
  let result = read s
  if a <= result && result <= b  then 
    Right result
  else
    Left $ "expected number in range [" ++ show a ++ " .. " ++ show b ++ "]."

--------------------------------------------------------------------------------

f x 0 = x; f x n = f ((map (0:) x) ++ (map (1:) x)) (pred n)

g [] = 0; g (x:xs) = (g xs - x) * (g xs + x)

swinnerton_dyer_poly n = do
  ctx <- newCaCtx
  xj <- newCa ctx
  poly <- newCaPoly ctx
  factor <- newCaPoly ctx
  sqrt_primes <- forM [1 .. fromIntegral n] $ \j -> do
    p <- n_nth_prime j
    (w, _) <- withNewCa ctx $ \x -> do
      withCaCtx ctx $ \ctx -> do
        ca_set_si x (fromIntegral p) ctx
        ca_sqrt x x ctx
    return w
  -- check roots
  forM_ sqrt_primes $ \w -> do
    withCaCtx ctx $ \ctx -> do
      withCa w $ \w -> do
        ca_print w ctx; putStr "\n"
  -- initialize factor
  withCaCtx ctx $ \ctx -> do
    withCaPoly factor $ \f -> do
      ca_poly_x f ctx
    withCaPoly poly $ \poly -> do
      ca_poly_one poly ctx
  -- calculate factors
  forM_ [0..2^n-1] $ \j -> do
    withCaCtx ctx $ \ctx -> do
      withCa xj $ \xj -> do
        ca_zero xj ctx
    zipWithM_
      (\k w -> do
        withCa xj $ \xj -> do
          withCaCtx ctx $ \ctx -> do
            withCa w $ \w -> do
              if testBit (j :: Int) k then do
                ca_add xj xj w ctx
              else do
                ca_sub xj xj w ctx    
      ) [0 .. n - 1] sqrt_primes
    withCaCtx ctx $ \ctx -> do
      withCa xj $ \xj -> do
        withCaPoly factor $ \f -> do
          ca_poly_set_coeff_ca f 0 xj ctx 
          ca_print xj ctx
          ca_poly_print f ctx
          withCaPoly poly $ \poly -> do
            ca_poly_mul poly poly f ctx
  withCaCtx ctx $ \ctx -> do
    withCaPoly poly $ \poly -> do
      ca_poly_print poly ctx; putStr "\n"
          

