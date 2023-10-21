import System.TimeIt

import Options.Applicative
import Control.Monad
import Control.Monad.State

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable 

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Factor multivariate polynomial in three variables {x, y, z}."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters poly num_threads timing suppress) = do
  mctx <- newFmpzMPolyCtx 3 ord_lex
  f <- newFmpzMPoly mctx
  fac <- newFmpzMPolyFactor mctx
  vars <- newArray =<< traverse newCString ["x", "y", "z"]
  (_, (_, flag)) <- withFmpzMPolyCtx mctx $ \mctx -> do
    withFmpzMPoly f $ \f -> do
      withCString poly $ \poly -> do 
        fmpz_mpoly_set_str_pretty f poly vars mctx
  if flag /= 0 then do
    putStrLn "unable to parse polynomial."
  else do
    withFmpzMPolyCtx mctx $ \mctx -> do
      withFmpzMPoly f $ \f -> do
        withFmpzMPolyFactor fac $ \fac -> do
          if timing then do
            timeItNamed "time for factorization" $ fmpz_mpoly_factor fac f mctx
          else do
            fmpz_mpoly_factor fac f mctx
          if not suppress then do
            fmpz_mpoly_print_pretty f vars mctx
            putStr " ="
            fmpz_mpoly_factor_print_pretty fac vars mctx
            putStr "\n"
          else do
            CFmpzMPolyFactor _ _ _ _ num _ <- peek fac
            putStrLn $ "polynomial has " ++ show num ++ " irreducible factors."
    return ()

data Parameters = Parameters {
    poly :: String
  , num_threads :: Int
  , timing :: Bool
  , suppress :: Bool
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument str (
      help "multivariate polynomial in x, y and z."
   <> metavar "POLYNOMIAL")
  <*> option auto (
      help "number of threads"
   <> short 't'
   <> long "threads"
   <> value 1
   <> metavar "THREADS")
  <*> switch (
      help "timing"
   <> long "timing")
  <*> switch (
      help "suppress output"
   <> long "suppress")
