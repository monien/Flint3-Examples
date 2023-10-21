import Options.Applicative
import Control.Monad
import Control.Monad.State

import Foreign.C.Types
import Foreign.Ptr

import Data.Number.Flint

main = run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Generate primes."
      <> header "Generate primes.")

run params@(Parameters n count) = do
  prime_iter <- newNPrimes
  if count then do
    (_, (prime_pi, _)) <- runStateT (countPrimes params) (0, prime_iter)
    print prime_pi
  else do
    _ <- runStateT (printPrimes params) prime_iter
    return ()
    
printPrimes :: Parameters -> StateT NPrimes IO ()
printPrimes params@(Parameters n count) = do
  prime_iter <- get
  (_, prime) <- liftIO $ withNPrimes prime_iter n_primes_next
  when (prime < n) $ do
    liftIO $ print prime
    put prime_iter
    printPrimes params

countPrimes :: Parameters -> StateT (Int, NPrimes) IO ()
countPrimes params@(Parameters n count) = do
  (nprimes, prime_iter) <- get
  (_, prime) <- liftIO $ withNPrimes prime_iter n_primes_next
  when (prime < n) $ do
    put (succ nprimes, prime_iter)
    countPrimes params
    
data Parameters = Parameters {
    n :: CULong
  , count :: Bool
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "n"
   <> metavar "n")
  <*> switch (
      help "count primes"
   <> short 'c'
   <> long "count")
