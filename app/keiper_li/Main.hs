import System.TimeIt
import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  desc = "Calculates keipers series."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n num_threads) = do
  flint_set_num_threads num_threads
  x <- newFmpq
  withFmpq x $ \x -> bernoulli_fmpq_ui x n
  when (n <= 100) $ print x

data Parameters = Parameters {
    n :: CULong
  , prec :: CULong
  , num_threads :: CInt
  , outFile :: Ptr CFile
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> option auto (
      help "n"
   <> short 'n'
   <> metavar "n")
  <*> option auto (
      help "precision"
   <> long "prec"
   <> short 'p'
   <> metavar "precision")
  <*> option auto (
      help "number of threads"
   <> long "threads"
   <> value 1
   <> metavar "threads")

keipersLiSeries z len prec = do

  t <- _arb_vec_init len
  u <- _arb_vec_init len
  v <- _arb_vec_init len

  timeItNamed "zeta: " do 
    arb_zero t
    arb_one (next t)
    arb_one u
    _arb_poly_zeta_series v t 2 u 0 len prec
    _arb_vec_neg v v len

  timeItNamed "log: " do
    _arb_poly_log_series t v len len prec

  -- add log(gamma(1+s/2))
  timeItNamed "gamma: " do
    arb_one u
    arb_one (next u)
    arb_mu_2exp_si (next u) (next u) (-1)
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

  
  