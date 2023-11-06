import System.IO
import System.TimeIt

import Control.Monad 
import Options.Applicative

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free) 
import Foreign.Marshal.Array (advancePtr)

import Data.Number.Flint

main = timeItNamed "time"
     $ run =<< customExecParser (prefs showHelpOnEmpty) opts where
  desc = "Calculates keipers series."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n prec num_threads outFile) = do
  print params
  
  flint_set_num_threads num_threads
  
  let len = n + 1
      workingPrecision = case prec of
        Just p  -> p
        Nothing -> round (1.1 * fromIntegral len + 50)
  print workingPrecision
  z <- _arb_vec_init len

  keiper_li_series z len workingPrecision

  case outFile of
    Just fileName -> do
      mode <- newCString "w"
      out_file_name <- newCString fileName
      fp <- fopen out_file_name mode
      withNewFmpz $ \man -> do
        withNewFmpz $ \exp -> do
          withNewArf $ \t -> do
            forM_ [0 .. fromIntegral len - 1] $ \j -> do
            
              arf_get_fmpz_2exp man exp (arb_midref (z `advancePtr` j))
              
              withCString (show j ++ " ") $ \s -> do fputs s fp
              fmpz_fprint fp man
              withCString " " $ \s -> fputs s fp
              fmpz_fprint fp exp
              withCString " +/- " $ \s -> fputs s fp
              
              arf_set_mag t (arb_radref (z `advancePtr` j))
              arf_get_fmpz_2exp man exp t

              fmpz_fprint fp man
              withCString " " $ \s -> fputs s fp
              fmpz_fprint fp exp
              withCString "\n" $ \s -> fputs s fp
      fclose fp
      mapM_ free [mode, out_file_name]
      return ()
    Nothing -> do               
      forM_ [0 .. len - 1] $ \j -> do
        putStr $ show j ++ ": "
        arb_printd (z `advancePtr` (fromIntegral j)) 50
        putStr "\n"

-- c file ----------------------------------------------------------------------

foreign import ccall "stdio.h fopen"
  fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall "stdio.h fclose"
  fclose :: Ptr CFile -> IO CInt

foreign import ccall "stdio.h fputs"
  fputs :: CString -> Ptr CFile -> IO CInt

data Parameters = Parameters {
    n :: CLong
  , prec :: Maybe CLong
  , num_threads :: CInt
  , outFile :: Maybe String
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "n"
   <> metavar "n")
  <*> optional optionPrecision
  <*> option auto (
      help "number of threads"
   <> long "threads"
   <> value 1
   <> metavar "threads")
  <*> optional optionOutput

optionPrecision = option auto  (
      help "precision"
   <> long "prec"
   <> short 'p'
   <> metavar "precision")

optionOutput = strOption (
      help "output file"
   <> short 'o'
   <> metavar "write output to file")

--------------------------------------------------------------------------------

keiper_li_series z len prec = do

  t <- _arb_vec_init len
  u <- _arb_vec_init len
  v <- _arb_vec_init len

  timeItNamed "zeta: " $ do 
    arb_zero t
    arb_one (next t)
    arb_one u
    _arb_poly_zeta_series v t 2 u 0 len prec
    _arb_vec_neg v v len

  timeItNamed "log: " $ do
    _arb_poly_log_series t v len len prec

  -- add log(gamma(1+s/2))
  timeItNamed "gamma: " $ do
    arb_one u
    arb_one (next u)
    arb_mul_2exp_si (next u) (next u) (-1)
    _arb_poly_lgamma_series v u 2 len prec
    _arb_vec_add t t v len prec

  -- subtract 0.5 s log(pi)
  arb_const_pi u prec
  arb_log u u prec
  arb_mul_2exp_si u u (-1)
  arb_sub (next t) (next t) u prec

  -- add log(1-s)
  arb_one u
  arb_set_si (next u) (-1)
  _arb_poly_log_series v u 2 len prec
  _arb_vec_add t t v len prec

  timeItNamed "binomial transform: " $ do
    arb_set z t
    _arb_vec_neg (next t) (next t) (len - 1)
    _arb_poly_binomial_transform (next z) (next t) (len - 1) (len - 1) prec
    
next x = x `advancePtr` 1

  
  