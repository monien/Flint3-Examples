{-# language ScopedTypeVariables #-}
import Options.Applicative

import Control.Monad
import Foreign.C.Types
import Foreign.Ptr 
import Foreign.Marshal.Array
import Foreign.Storable

import Data.Number.Flint

main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Reconstruct integer using the chinese remainder theorem."
  hdesc = "Fast tree version of the integer Chinese Remainder code."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n num_primes) = do
  print params
  primes <- mapM n_nth_prime [1..fromIntegral num_primes]
  withArray primes $ \primes -> do
    comb <- newFmpzComb (castPtr primes) (fromIntegral num_primes)
    withFmpzComb comb $ \comb -> do
      comb_temp <- newFmpzCombTemp comb
      withFmpzCombTemp comb_temp $ \comb_temp -> do
        withFmpz n $ \x -> do
          withNewFmpz $ \y -> do
            allocaArray num_primes $ \(residues :: Ptr CLong) -> do
              --  Reduce modulo all primes 
              fmpz_multi_mod_ui (castPtr residues) x comb comb_temp
              -- Reconstruct
              fmpz_multi_CRT_ui y (castPtr residues) comb comb_temp 1
              forM_ [0 .. fromIntegral num_primes - 1] $ \i -> do
                p <- peek (primes   `advancePtr` i)
                r <- peek (residues `advancePtr` i)
                putStrLn $ "residue mod " ++ show p ++ " = " ++ show r
              putStr "reconstruction = "
              fmpz_print y
              putStr "\n"
  
data Parameters = Parameters {
    n :: Fmpz
  , num_primes :: Int
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "n to be reconstructed"
   <> metavar "n")
  <*> option auto (
      help "number of primes [2, 3, ...] to use"
   <> long "np"
   <> value 1
   <> metavar "num_primes")
