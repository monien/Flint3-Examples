import Options.Applicative
import Control.Monad
import Control.Monad.State

import Foreign.C.Types
import Foreign.Ptr

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Compute nth coefficient of the q-expansion of the weight\
       \12 cusp form."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n) = do
  withNewFmpz $ \k -> do
    fmpz_set_si k  n
    withNewFmpz $ \c -> do
      arith_ramanujan_tau c k
      putStr $ "Coefficient of q^" ++ show n ++ " is "
      fmpz_print c
      putStr "\n"
    
data Parameters = Parameters {
    n :: CLong
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "positive integer n"
   <> metavar "n")