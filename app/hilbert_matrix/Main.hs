import Foreign.Ptr (nullPtr)
import Foreign.C.Types

import Options.Applicative
import Control.Monad
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.Number.Flint

main = run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "calculates the determinant of the Hilbert matrix"
      <> header "Hilbert matrix determinant")

run p@(Parameters eig n) = do
 print p
 hilbertMatrix eig (fromIntegral n) 20
 
data Parameters = Parameters {
  eig :: Bool,
  n :: Int
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> switch (
   long "eig" <>
   help "calculating det as a product of eigenvalues.")
  <*> argument auto (
   help "dimension of Hilbert matrix." <>
   metavar "n")


hilbertMatrix :: Bool -> CLong -> CLong -> IO ()
hilbertMatrix eig n prec = do
  _ <- withNewArb $ \det -> do
    _ <- withNewArbMat n n $ \a -> do
      arb_mat_hilbert a prec
      if not eig then
        arb_mat_det det a prec
      else do
        _ <- withNewAcbMat n n $ \r -> do
          _ <- withNewAcbMat n n $ \c -> do
            acb_mat_set_arb_mat c a
            e <- _acb_vec_init n
            _ <- acb_mat_approx_eig_qr e nullPtr r c nullPtr 0 prec
            simple <- acb_mat_eig_simple e nullPtr nullPtr c e r prec
            if simple == 1 then do
              arb_one det
              forM_ [0 .. fromIntegral n-1] $ \j -> do
                arb_zero (castPtr (e `advancePtr` j) `advancePtr` 1)
              _acb_vec_sort_pretty e n
              acb_get_real det e
            else
              arb_indeterminate det
            _acb_vec_clear e n
          return ()
        return ()
      return ()
    zero <- arb_contains_zero det
    if zero == 1 then do
      hilbertMatrix eig n (prec + 20)
    else do
      putStrLn $ "success with prec " ++ show prec ++ " bits."
      putStr "value: "
      arb_printd det 16
      putStr "\n"
  return ()
  