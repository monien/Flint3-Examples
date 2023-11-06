import System.Environment
import System.TimeIt

import Options.Applicative
import Control.Monad
import Control.Monad.State
import Text.Read (readMaybe)

import Options.Applicative

import Foreign.C.Types
import Foreign.Ptr

import Data.Number.Flint

main = timeItNamed "time"
     $ run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Calculates class polynomial for negative discriminant."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run (Parameters d num_threads) = do
  flint_set_num_threads num_threads
  res <- newFmpzPoly
  withFmpzPoly res $ \res -> acb_modular_hilbert_class_poly res d
  when (abs d <= 100) $ print res

data Parameters = Parameters {
    d :: CLong
  , num_threads :: CInt
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument discriminant (
      help "absolute value of (-D)"
   <> metavar "D")
  <*> option auto (
      help "number of threads"
   <> long "threads"
   <> value 1
   <> metavar "threads")

discriminant :: (Read a, Integral a, Show a) => ReadM a
discriminant = eitherReader $ \s -> do
  let d = negate $ read s
  if d < 0 then
    if d `mod` 4 == 1 || d `mod` 4 == 0 then
      Right d
    else
      Left $ "discriminant D (=" ++ show d ++ ") `mod` 4 /= 0, 1."
  else
    Left $ "discriminant D (=" ++ show d ++ ") > 0."
    