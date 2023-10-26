import Options.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Array 

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Reports the imaginary parts of consecutive nontrivial zeros \
         \of the Riemann zeta function."
  opts = info (options <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run opts@(Options n count accuracy platt verbosity num_threads) = do
  print opts
  let (prec, digits) = case accuracy of
        Precision p -> (p, round (fromIntegral p * logBase 10 2 + 1))
        Digits d    -> (round (fromIntegral digits / logBase 10 2 + 3), d)
  putStrLn $ "prec = " ++ show prec
  putStrLn $ "digits = " ++ show digits
  -- withNewFmpz $ \requested -> do
  --   withNewFmpz $ \count -> do
  --     withNewFmpz $ \nstart -> do
  --       withFmpz $ \n -> do
  --         fmpz_one nstart
  --         fmpz_set_si requested (-1)

data Options = Options {
  n :: Fmpz
, count :: Fmpz
, accuracy :: Accuracy
, platt :: Bool
, verbosity :: Int
, num_threads :: Int
} deriving Show

data Accuracy = Precision CLong | Digits CLong deriving Show

options :: Parser Options
options = Options
  <$> option pos (
      help "positive integer n"
   <> short 'n'
   <> value 10
   <> metavar "n")
  <*> option pos (
      long "count"
   <> value 0
   <> short 'c'
   <> metavar "count")
  <*> optionAccuracy
  <*> switch (
      help "use platt algorithm."
   <> long "platt")
  <*> option pos (
      help "verbosity."
   <> long "verbosity"
   <> short 'v'
   <> value 0
   <> metavar "verbosity")
  <*> option pos (
      help "number of threads."
   <> long "threads"
   <> short 't'
   <> value 1
   <> metavar "threads")

optionAccuracy = optionPrecision <|> optionDigits

optionPrecision :: Parser Accuracy
optionPrecision =  Precision <$> option pos (
      help "precision."
   <> long "prec"
   <> short 'p'
   <> value 64
   <> metavar "precision")

optionDigits :: Parser Accuracy
optionDigits = Digits <$> option pos (
      help "number of digits."
   <> long "digits"
   <> short 'd'
   <> metavar "digits")
   
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"

print_zeros p n len digits = do
  withNewFmpz $ \k -> do
    fmpz_set k n
    forM_ [0 .. len-1] $ \i -> do
      fmpz_print k
      putStr "\t"
      arb_printn (p `advancePtr` i) digits arb_str_no_radius
      putStr "\n"
      fmpz_add_ui k k 1


