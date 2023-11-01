import Options.Applicative
import Control.Monad

import System.TimeIt

import Foreign.C.Types
import Foreign.Marshal.Array

import Text.Printf

import Data.Number.Flint

main = timeIt $ run =<< execParser opts where
  desc = "This program constructs the Hilbert matrixq as exact \
         \algebraic numbers, and verifies the exact trace and \
         \determinant formulas."

  opts = info (options <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Options n useQQbar useVieta) = do
  print params
  if useQQbar then do
    withNewFmpqMat n n $ \mat -> do
      withNewQQbar $ \trace -> do
        withNewQQbar $ \det -> do
        
          eig <- _qqbar_vec_init n
          
          fmpq_mat_hilbert_matrix mat
          qqbar_eigenvalues_fmpq_mat eig mat 0
          
          putStrLn "Trace:"
          forM_ [0 .. fromIntegral n - 1] $ \i -> do
            qqbar_add trace trace (eig `advancePtr` i)
            deg <- qqbar_degree trace
            putStr $ show i ++ " " ++ show n ++ ": "; print deg
          qqbar_print trace; putStr "\n"
          
          putStrLn "Determinant:"
          qqbar_one det
          forM_ [0 .. fromIntegral n - 1] $ \i -> do
            qqbar_mul det det (eig `advancePtr` i)
            deg <- fromIntegral <$> qqbar_degree det 
            putStr $ show i ++ " " ++ show n ++ ": "; print deg
          qqbar_print det; putStr "\n"
          
          _qqbar_vec_clear eig n
    return ()
  else do
    ctx <- newCaCtx
    withCaCtx ctx $ \ctx -> do
      -- Verification requires high-degree algebraics.
      ca_ctx_set_option ctx ca_opt_qqbar_deg_limit 10000
      if useVieta then do 
        ca_ctx_set_option ctx ca_opt_vieta_limit n
      else do
        ca_ctx_set_option ctx ca_opt_vieta_limit 0
    withNewCaMat n n ctx $ \mat -> do
      withNewCa ctx $ \trace -> do
        withNewCa ctx $ \det -> do
          withNewCa ctx $ \t -> do
            withNewCaVec 0 ctx $ \eig -> do 
              withCaCtx ctx $ \ctx -> do 
        
                ca_mat_hilbert mat ctx

                allocaArray (fromIntegral n) $ \mul -> do
                  ca_mat_eigenvalues eig mul mat ctx

                -- note: in general, we should use the multiplicities, but
                -- we happen to know that the eigenvalues are simple here

                ca_mat_trace trace mat ctx
                ca_mat_det det mat ctx

                putStrLn "Trace:"
                forM_ [0 .. fromIntegral n - 1] $ \i -> do
                  ca_add t t  (ca_vec_entry_ptr eig i) ctx
                ca_print trace ctx; putStr "\n"
                ca_print t ctx; putStr "\n"
                res <- ca_check_equal trace t ctx
                putStr $ "Equal: " ++ show res ++ "\n\n"

                putStrLn "Det:"
                ca_one t ctx
                forM_ [0 .. fromIntegral n - 1] $ \i -> do
                  ca_mul t t  (ca_vec_entry_ptr eig i) ctx
                ca_print det ctx; putStr "\n"
                ca_print t ctx; putStr "\n"
                res <- ca_check_equal det t ctx
                putStr $ "Equal: " ++ show res ++ "\n\n"
    putStr "\n."
          
data Options = Options {
    n :: CLong
  , useQQbar :: Bool
  , useVieta :: Bool
} deriving Show

options :: Parser Options
options = Options
  <$> argument auto (
      help "n, the dimension of the Hilbert matrix"
   <> metavar "n")
  <*> switch (
      help "use QQbar arithmetic."
   <> long "qqbar")
  <*> switch (
      help "use Vieta formula."
   <> long "vieta")
