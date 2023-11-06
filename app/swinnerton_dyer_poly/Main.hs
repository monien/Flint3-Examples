import Control.Monad
import Data.Word
import Data.Bits

import System.TimeIt
import Options.Applicative

import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array

import Data.Number.Flint

main = timeItNamed "time"
     $ run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Computes the coefficients of the Swinnerton-Dyer polynomial."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n) = swinnerton_dyer_poly n
  
-- Parser Parameters -----------------------------------------------------------

newtype Parameters = Parameters {
  n :: CULong
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

swinnerton_dyer_poly n = do
  ctx <- newCaCtx
  poly <- newCaPoly ctx
  withCaPoly poly $ \poly -> do
    withNewCaPoly ctx $ \u -> do
      withNewCaPoly ctx $ \v -> do 
        withNewCaPoly ctx $ \tmp -> do
          withNewCa ctx $ \w -> do
            withCaCtx ctx $ \ctx -> do
              ca_poly_x poly ctx
              forM_ [1 .. n] $ \j -> do
                p <- n_nth_prime j            
                ca_set_si w (fromIntegral p) ctx
                ca_sqrt w w ctx
                ca_poly_x tmp ctx
                ca_poly_set_coeff_ca tmp 0 w ctx
                ca_poly_compose u poly tmp ctx
                ca_neg w w ctx
                ca_poly_set_coeff_ca tmp 0 w ctx
                ca_poly_compose v poly tmp ctx
                ca_poly_mul poly u v ctx
  withCaCtx ctx $ \ctx -> do
    withCaPoly poly $ \poly -> do
      ca_poly_print poly ctx; putStr "\n"

