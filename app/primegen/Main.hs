import Options.Applicative
import Control.Monad
import Foreign.C.Types
import Data.Time
import System.TimeIt

import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Generate primes."
      <> header "Generate primes.")

run params@(Parameters n nthreads) = do
  withNewNPrimes $ \prime_iter -> do
    replicateM_ n $ do
      prime <- n_primes_next prime_iter
      -- print prime
      when (prime < 100) $ do
        withFmpz x $ \x -> do
          fmpz_mul_ui x x prime
        return ()
  
data Parameters = Parameters {
    n :: CULong
  , nthreads :: CInt
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
    help "n" <>
    metavar "n")
  <*> option auto (
    help "number of threads" <>
    long "threads" <>
    value 1 <>
    metavar "threads")

  
  
