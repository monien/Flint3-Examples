import Options.Applicative
import Control.Monad
import Foreign.C.Types

import Data.Number.Flint

main = run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Generate primes."
      <> header "Generate primes.")

run params@(Parameters n count) = do
  withNewNPrimes $ \prime_iter -> do
    replicateM_ n $ do
      prime <- n_primes_next prime_iter
      print prime
  
data Parameters = Parameters {
    n :: Int
  , count :: Bool
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "n"
   <> metavar "n")
  <*> option auto (
      help "count primes."
   <> short 'c'
   <> long "count"
   <> value False
   <> metavar "count")


  
  
