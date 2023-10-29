import Options.Applicative
import Control.Monad
import System.TimeIt
import Foreign.C.Types

import Data.Number.Flint

main = timeItNamed "binet" $ run =<< execParser opts where
  desc = "This program computes the n-th Fibonacci number using Binet’s formula"
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n limit) = do
  print params
  
  ctx <- newCaCtx

  [sqrt5, phi, psi, t, u] <- replicateM 5 (newCa ctx)

  withCaCtx ctx $  \ctx -> do
    case limit of
      Just prec -> ca_ctx_set_option ctx ca_opt_prec_limit prec
      _         -> return ()
    withCa sqrt5 $ \sqrt5 -> do
      withCa phi $ \phi -> do
        withCa psi $ \psi -> do
          withCa t $ \t -> do
            withCa u $ \u -> do
              ca_sqrt_ui sqrt5 5 ctx
              ca_add_ui phi sqrt5 1 ctx
              ca_div_ui phi phi 2 ctx
              ca_ui_sub psi 1 phi ctx
              withFmpz n $ \n -> do
                ca_pow_fmpz t phi n ctx
                ca_pow_fmpz u psi n ctx
              ca_sub t t u ctx
              ca_div t t sqrt5 ctx
              ca_print t ctx; putStr "\n"
              
data Parameters = Parameters {
    n :: Fmpz
  , limit :: Maybe CLong
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument auto (
      help "nth power"
   <> metavar "n")
  <*> optional optionLimit

optionLimit = option auto (
      help "limiting precision"
   <> long "limit"
   <> metavar "limit")
     
