import System.TimeIt
import System.IO.Unsafe

import Options.Applicative
import Control.Monad
import Control.Monad.State

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable 

import Data.List (intercalate)

import Data.Number.Flint

main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Factor integers."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters expression num_threads timing) = do
  flint_set_num_threads num_threads
  case parseExpression expression of
    Just n ->
      if timing then do
        timeItNamed "time for factorization" $ printFactorization n
      else do
        printFactorization n
    _ -> putStrLn "Could not parse expression."

printFactorization x = putStrLn $ showFactorization $ factor x
  
showFactorization x = s where
  s = intercalate (" . ") $ map showTerm x
  showTerm (x, e)
    | e == 1 = show x
    | otherwise = show x ++ "^" ++ show e
    
parseExpression expression = unsafePerformIO $ do
  mctx <- newFmpzMPolyCtx 0 ord_lex
  f <- newFmpzMPoly mctx
  fac <- newFmpzMPolyFactor mctx
  (_, (_, flag)) <- withFmpzMPolyCtx mctx $ \mctx -> do
    withFmpzMPoly f $ \f -> do
      withCString expression $ \poly -> do 
        fmpz_mpoly_set_str_pretty f poly nullPtr mctx
  if flag == 0 then do
    n <- newFmpz
    withFmpzMPolyCtx mctx $ \mctx -> do
      withFmpzMPoly f $ \f -> do
        withFmpz n $ \n -> do
          fmpz_mpoly_get_fmpz n f mctx
    return $ Just n
  else do
    return $ Nothing
    
data Parameters = Parameters {
    expression :: String
  , num_threads :: CInt
  , timing :: Bool
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument str (
      help "Integer given as expression (e.g. 2^64+1)"
   <> metavar "INTEGER")
  <*> option auto (
      help "number of threads"
   <> short 't'
   <> long "threads"
   <> value 1
   <> metavar "THREADS")
  <*> switch (
      help "timing"
   <> long "timing")
