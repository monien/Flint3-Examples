import Options.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Array 

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Reports the imaginary parts of consecutive nontrivial zeros \
         \of the Riemann zeta function."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n count prec digits platt verbosity num_threads) = do
  print params
  -- withNewFmpz $ \requested -> do
  --   withNewFmpz $ \count -> do
  --     withNewFmpz $ \nstart -> do
  --       withFmpz $ \n -> do
  --         fmpz_one nstart
  --         fmpz_set_si requested (-1)

data Parameters = Parameters {
    n :: Fmpz
  , count :: Fmpz
  , prec :: CLong 
  , digits :: CLong
  , platt :: Bool
  , verbosity :: Int
  , num_threads :: Int
  } deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> option auto (
      help "positive integer n"
   <> value 10
   <> metavar "n")
  <*> option auto (
      long "count"
   <> value 0
   <> metavar "count")
  <*> option auto (
      help "precision."
   <> long "prec"
   <> short 'p'
   <> value 64
   <> metavar "prec")
  <*> option auto (
      help "number of digits."
   <> long "digits"
   <> metavar "digits")
  <*> switch (
      help "use platt algorithm."
   <> long "platt")
  <*> option auto (
      help "verbosity."
   <> long "verbosity"
   <> short 'v'
   <> value 0
   <> metavar "verbosity")
  <*> option auto (
      help "number of threads."
   <> long "threads"
   <> value 1
   <> metavar "threads")
   
print_zeros p n len digits = do
  withNewFmpz $ \k -> do
    fmpz_set k n
    forM_ [0 .. len-1] $ \i -> do
      fmpz_print k
      putStr "\t"
      arb_printn (p `advancePtr` i) digits arb_str_no_radius
      putStr "\n"
      fmpz_add_ui k k 1


