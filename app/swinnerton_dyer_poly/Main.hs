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
  n :: CLong
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
  let m = 2 ^ n
  print n
  print m
  primes <- mapM n_nth_prime [1.. fromIntegral n]
  print primes
  forM_ [0..2^n-1] $ \j -> do
    print $ map (testBit (j :: CLong)) [0 .. fromIntegral n-1]