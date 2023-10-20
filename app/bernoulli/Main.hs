import Options.Applicative
import Control.Monad
import Foreign.C.Types
import Data.Time
import System.TimeIt

import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Calculates bernoulli numbers."
      <> header "Calculates bernoulli numbers.")

run params@(Parameters n nthreads) = do
  flint_set_num_threads nthreads
  x <- newFmpq
  withFmpq x $ \x -> bernoulli_fmpq_ui x n
  when (n <= 100) $ print x
  
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
