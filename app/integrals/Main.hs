import GHC.Conc

import System.IO.Unsafe
import System.CPUTime

import Options.Applicative

import Control.Monad
import Control.Applicative ((<|>))

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (with)
import Foreign.Storable

import Text.ParserCombinators.ReadP hiding (option)
import Text.Read (readMaybe)
import Text.Printf

import Data.Bits
import Data.Char
import Data.List (intercalate)
import Data.Number.Flint

import Integrands

main = run =<< execParser opts where
  hDesc = "Calculate integrals using acb_calculate."
  desc = "Calculate integrals from list in range 0:"
       ++ show (length description) ++ ". "
       ++ "For a list of implemented integrals use --list."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header hDesc)

run params = do
  if list params then do
    putStrLn "List of implemented integrals:\n"
    mapM_ (uncurry (printf "  %2d  %s\n")) $ zip [0 :: Int ..] description
  else do
    calc params
    
calc params@(Parameters list range prec goal' tol twice
                        heap verbose deg eval depth num_threads) = do
  numProcessors <- getNumProcessors
  if num_threads <= fromIntegral numProcessors then do 
    flint_set_num_threads num_threads
  else do
    let msg = "number of threads (=" ++ show num_threads ++ ") greater than "
           ++ "number of processors (=" ++ show numProcessors ++ ").\n\n"
           ++ "choose [1.." ++ show numProcessors ++ "] threads."
    error msg
  let use_heap = if heap then 1 else 0
      Range (start, end) = range
      goal = if goal' == 0 then prec else goal'
  opts <- newAcbCalcIntegrateOpt_ deg eval depth use_heap verbose
  print opts
  withAcbCalcIntegrateOpt opts $ \opts -> do
    withMag tol $ \tol -> do
      flag <- mag_is_zero tol
      when (flag == 1) $ do mag_set_ui_2exp_si tol 1 (-prec)
      withNewAcb $ \a -> do
        withNewAcb $ \b -> do
          withNewAcb $ \s -> do
            withNewAcb $ \t -> do
              forM_ [start .. end] $ \j -> do
                let desc = description !! j
                startTime <- getCPUTime
                replicateM_ (if twice then 2 else 1) $ do
                  case j of
                    0 -> do
                      f <- makeFunPtr f_sin
                      acb_set_si a 0
                      acb_set_si b 100
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    1 -> do
                      f <- makeFunPtr f_atanderiv
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_mul_2exp_si s s 2
                      return ()
                    2 -> do
                      f <- makeFunPtr f_atanderiv
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_mul_2exp_si b b goal
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      arb_add_error_2exp_si (acb_realref s) (-goal)
                      acb_mul_2exp_si s s 1
                      return ()
                    3 -> do
                      f <- makeFunPtr f_circle
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_mul_2exp_si s s 2
                      return ()
                    4 -> do
                      f <- makeFunPtr f_rump
                      acb_set_si a 0
                      acb_set_si b 8
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    5 -> do
                      f <- makeFunPtr f_floor
                      acb_set_si a 1
                      acb_set_si b 101
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    6 -> do
                      f <- makeFunPtr f_helfgott
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    7 -> do
                      f <- makeFunPtr f_zeta
                      let points = [(-1, -1), (2, -1), (2, 1), (-1, 1)]
                          path = zip points $ tail $ cycle points
                      acb_zero s
                      forM_ path $ \((x1, y1), (x2, y2)) -> do 
                        acb_set_si_si a x1 y1
                        acb_set_si_si b x2 y2
                        acb_calc_integrate t f nullPtr a b goal tol opts prec
                        acb_add s s t prec
                      acb_const_pi t prec
                      acb_div s s t prec
                      acb_mul_2exp_si s s (-1)
                      acb_div_onei s s
                      return()
                    8 -> do
                      f <- makeFunPtr f_essing
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    9 -> do
                      f <- makeFunPtr f_essing2
                      acb_set_si a 0
                      acb_set_si b 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    10 -> do
                      f <- makeFunPtr f_factorial1000
                      acb_set_si a 0
                      acb_set_si b 10000
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    11 -> do
                      f <- makeFunPtr f_gamma
                      acb_set_si_si a 1 0
                      acb_set_si_si b 1 1000
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    12 -> do
                      f <- makeFunPtr f_sin_plus_small
                      acb_set_si a (-10)
                      acb_set_si b 10
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    13 -> do
                      f <- makeFunPtr f_exp
                      acb_set_si a (-1020)
                      acb_set_si b (-1010)
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    14 -> do
                      f <- makeFunPtr f_gaussian
                      acb_zero a
                      acb_set_si b $ fromIntegral
                                   $ ceiling 
                                   $ sqrt (fromIntegral goal * log 2) + 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_mul b b b prec
                      acb_neg b b
                      acb_exp b b prec
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    15 -> do
                      f <- makeFunPtr f_spike
                      acb_zero a
                      acb_one b 
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    16 -> do
                      f <- makeFunPtr f_monster
                      acb_zero a
                      acb_set_si b 8
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    17 -> do
                      f <- makeFunPtr f_sech
                      acb_set_d a 0
                      acb_set_d b $ fromIntegral
                                  $ ceiling 
                                  $ fromIntegral goal * log 2 + 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_neg b b
                      acb_exp b b prec
                      acb_mul_2exp_si b b 1
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    18 -> do
                      f <- makeFunPtr f_sech3
                      acb_zero a
                      acb_set_d b $ fromIntegral
                                  $ ceiling 
                                  $ fromIntegral goal * log 2  / 3 + 2
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_neg b b              
                      acb_mul_ui b b 3 prec
                      acb_exp b b prec
                      acb_mul_2exp_si b b 3
                      acb_div_ui b b 3 prec
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    19 -> do
                      when (goal < 0) $ do error "goal < 0"
                      -- error bound 2^-N (1+N) when truncated at 2^-N
                      let bitCount x = finiteBitSize x - countLeadingZeros x
                          n = goal + fromIntegral (bitCount goal)
                      acb_one a
                      acb_mul_2exp_si a  a (-n)
                      acb_one b
                      f <- makeFunPtr f_log_div1p
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_set_si b (n+1)
                      acb_mul_2exp_si b b (-n)
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    20 -> do 
                      f <- makeFunPtr f_log_div1p_transformed
                      when (goal < 0) $ do error "goal < 0"
                      -- error bound (N+1) exp(-N) when truncated at N
                      let bitCount x = finiteBitSize x - countLeadingZeros x
                          n = goal + fromIntegral (bitCount goal)
                      acb_zero a
                      acb_set_si b n
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_neg b b
                      acb_exp b b prec
                      acb_mul_si b b (n+1) prec
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    21 -> do
                      f <- makeFunPtr f_elliptic_p_laurent_n
                      let n = 10 ::CLong
                          points = [(1, -1), (1, 1), (-1, 1), (-1, -1)]
                          path = zip points $ tail $ cycle points
                      acb_zero s
                      with n $ \np -> do
                        let p = castPtr np :: Ptr ()
                        forM_ path $ \((x1, y1), (x2, y2)) -> do
                          acb_set_si_si a x1 y1
                          acb_set_si_si b x2 y2
                          acb_div_ui a a 2 prec
                          acb_div_ui b b 2 prec
                          acb_calc_integrate t f p a b goal tol opts prec
                          acb_add s s t prec
                      acb_const_pi t prec
                      acb_div s s t prec
                      acb_mul_2exp_si s s (-1)
                      acb_div_onei s s
                      return ()
                    22 -> do
                      f <- makeFunPtr f_zeta_frac
                      let n = 1000 :: CLong
                      acb_zero s

                      acb_set_si_si a 100 0
                      acb_set_si_si b 100 n
                      acb_calc_integrate t f nullPtr a b goal tol opts prec
                      acb_add s s t prec

                      acb_set_si_si a 100 n
                      acb_set_si_si b 1 (2*n)
                      acb_div_ui b b 2 prec
                      acb_calc_integrate t f nullPtr a b goal tol opts prec
                      acb_add s s t prec

                      acb_div_onei s s
                      arb_zero (acb_imagref s)

                      acb_set_si t n
                      acb_dirichlet_hardy_theta t t nullPtr nullPtr 1 prec
                      acb_add s s t prec

                      acb_const_pi t prec
                      acb_div s s t prec
                      acb_add_ui s s 1 prec
                      return ()
                    23 -> do
                      acb_set_si a 0
                      acb_set_si b 1000
                      f <- makeFunPtr f_lambertw 
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    24 -> do
                      acb_zero a
                      acb_const_pi b prec
                      f <- makeFunPtr f_max_sin_cos
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    25 -> do
                      acb_set_si a (-1)
                      acb_set_si b 1
                      f <- makeFunPtr f_erf_bent
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    26 -> do
                      acb_set_si a (-10)
                      acb_set_si b 10
                      f <- makeFunPtr f_airy_ai
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    27 -> do
                      acb_set_si a 0
                      acb_set_si b 10
                      f <- makeFunPtr f_horror
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    28 -> do
                      acb_set_si_si a (-1) (-1)
                      acb_set_si_si b (-1) 1
                      f <- makeFunPtr f_sqrt
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    29 -> do
                      f <- makeFunPtr f_gaussian_twist
                      acb_zero a
                      acb_set_si b $ ceiling
                                   $ sqrt (fromIntegral goal * log 2) + 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_mul b b b prec
                      acb_neg b b
                      acb_exp b b prec
                      arb_add_error (acb_realref s) (acb_realref b)
                      arb_add_error (acb_imagref s) (acb_realref b)
                      return ()
                    30 -> do
                      f <- makeFunPtr f_exp_airy
                      acb_zero a
                      acb_set_si b $ ceiling
                                   $ fromIntegral goal * log 2 + 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      acb_mul b b b prec
                      acb_neg b b
                      acb_exp b b prec
                      acb_mul_2exp_si b b 1
                      arb_add_error (acb_realref s) (acb_realref b)
                      return ()
                    31 -> do
                      f <- makeFunPtr f_sin_cos_frac
                      acb_zero a
                      acb_const_pi b prec
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    32 -> do
                      f <- makeFunPtr f_sin_near_essing
                      acb_zero a
                      acb_set_ui b 3
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    33 -> do
                      f <- makeFunPtr f_scaled_bessel
                      acb_zero a
                      acb_zero b
                      let k = 3
                      scaled_bessel_select_N (acb_realref b) k prec
                      with k $ \kp -> do
                        let p = castPtr kp :: Ptr ()
                        acb_calc_integrate s f p a b goal tol opts prec
                      scaled_bessel_tail_bound (acb_realref a) k
                                               (acb_realref b) prec
                      arb_add_error (acb_realref s) (acb_realref a)
                      return ()
                    34 -> do
                      f <- makeFunPtr f_scaled_bessel
                      acb_zero a
                      acb_zero b
                      let k = 15
                      scaled_bessel_select_N (acb_realref b) k prec
                      with k $ \kp -> do
                        let p = castPtr kp :: Ptr ()
                        acb_calc_integrate s f p a b goal tol opts prec
                      scaled_bessel_tail_bound (acb_realref a) k
                                               (acb_realref b) prec
                      arb_add_error (acb_realref s) (acb_realref a)
                      return ()
                    35 -> do
                      f <- makeFunPtr f_rsqrt
                      acb_set_si_si a (-1) (-1)
                      acb_set_si_si b (-1) 1
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    36 -> do
                      f <- makeFunPtr f_rgamma
                      when (goal < 0) $ do error "goal < 0."
                      acb_zero a
                      acb_set_si b (4 + (goal + 1) `div` 2)
                      acb_calc_integrate s f nullPtr a b goal tol opts prec
                      return ()
                    _ -> do
                      error "case does not exists."
                      return ()
                endTime <- getCPUTime
                putStrLn $ "I" ++ show j ++ " = " ++ desc
                let diff = fromIntegral (endTime  - startTime) / 10^12 :: Double
                printf "computation time: %0.3f sec\n" diff
                let digits = round (0.333 * fromIntegral prec) :: CLong         
                acb_printn s digits arb_str_none
                putStr "\n\n"
  return ()

-- auxiliary routines ----------------------------------------------------------

scaled_bessel_tail_bound b k n prec = do
  arb_const_pi b prec
  arb_mul b b n prec
  arb_ui_div b k b prec
  arb_sqrt b b prec
  arb_pow_ui b b k prec
  arb_mul b b n prec
  arb_mul_ui b b 2 prec
  arb_div_ui b b (k-2) prec

scaled_bessel_select_N n k prec = do
  let f = log (fromIntegral k / pi) / log 2 :: CDouble
      c e = fromIntegral e * fromIntegral k / 2 - f - fromIntegral e
          < fromIntegral prec + 5
      e = last $ takeWhile c [1, 2 ..]
  arb_one n
  arb_mul_2exp_si n n e

-- Parser ----------------------------------------------------------------------

data Parameters = Parameters {
    list        :: Bool
  , range       :: Range
  , prec        :: CLong
  , goal        :: CLong
  , tol         :: Mag
  , twice       :: Bool
  , heap        :: Bool
  , verbosity   :: CInt
  , deg         :: CLong
  , eval        :: CLong
  , depth       :: CLong
  , num_threads :: CInt
} deriving Show
    
parameters :: Parser Parameters
parameters = Parameters
  <$> switch (
      help "show list of implemented integrals."
   <> long "list"
   <> short 'l')
  <*> option rng (
      help "range (for example --range 2:6 or --range 7)"  
   <> long "range"
   <> short 'r'
   <> value (Range (0, length description - 1))
   <> metavar "range")
  <*> option pos (
      help "precision in bits (default p = 64)"
   <> long "prec"
   <> short 'p'
   <> value 64
   <> metavar "p")
  <*> option pos (
      help "approximate relative accuracy goal (p)"
   <> long "goal"
   <> value 0
   <> metavar "goal")
  <*> option mag (
      help "approximate absolute accuracy goal (default 2^-p)"
   <> long "tol"
   <> value (read "0")
   <> metavar "abstol")
  <*> switch (
      help "run twice (to see overhead of computing nodes)"
   <> long "twice")
  <*> switch (
      help "use heap for subinterval queue"
   <> long "heap")
  <*> option pos (
      help "verbosity level"
   <> short 'v'
   <> long "verbosity"
   <> value 0
   <> metavar "verbosity")
  <*> option pos (
      help "use quadrature degree up to n"
   <> long "deg"
   <> value 0
   <> metavar "degree")
  <*> option pos (
      help "limit number of function evaluations to n"
   <> long "eval"
   <> value 0
   <> metavar "eval")
  <*> option pos (
      help "limit subinterval queue size to n"
   <> long "depth"
   <> value 0
   <> metavar "depth")
  <*> option auto (
      help "number of threads"
   <> long "threads"
   <> value 1 
   <> metavar "threads")

-- ReadM parsers ---------------------------------------------------------------

rng :: ReadM Range
rng = eitherReader $ \s -> do
  let result@(Range (a, b)) = read s :: Range
  if 0 <= a && a <= b && b < length description then
    Right result
  else
    Left "could not parse range"
  
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"

mag = eitherReader $ \s -> do
  case readMaybe s of
    Just result -> Right result
    _           -> Left $ "parsing " ++ show s ++ " failed."

-- instances Mag ---------------------------------------------------------------

instance Read Mag where
  readsPrec _ = readP_to_S (mkMag 10 <$> parseArb)

-- instances Range -------------------------------------------------------------

newtype Range = Range (Int, Int)

instance Read Range where
  readsPrec _ = readP_to_S (parseRange <|> parseIndex)

instance Show Range where
  show (Range (a, b)) = "[" ++ show a ++ ":" ++ show b ++ "]"
  
-- parsers  --------------------------------------------------------------------

parseArb = do
  (res, _) <- gather $ choice
    [  char '[' *> parseFloat *> pm *> parseFloat <* char ']'
    ,  char '[' *> pm *> parseFloat <* char ']'
    ,  char '[' *> parseFloat <* char ']'
    , parseFloat
    ]
  return res
  where pm = skipSpaces *> string "+/-" <* many1 (char ' ')

mkMag prec s = unsafePerformIO $ do
  (result, flag) <- withNewMag $ \m -> do
    withNewArb $ \x -> do
      withCString s $ \s -> do
        arb_set_str x s prec
        arb_get_mag m x
  return result
     
parseFloat = do
  munch (\x -> x == '+' || x == '-')
  choice [nan, inf, num *> e, num]
  where
    nan = string "nan"
    inf = string "inf"
    num = munch1 isNumber *> munch (== '.') *> munch isNumber
    e = do
      char 'e'
      munch (\x -> x == '+' || x == '-')
      munch1 isNumber

parseRange :: ReadP Range 
parseRange = do
  a <- read <$> munch1 isNumber
  char ':'
  b <- read <$> munch1 isNumber
  return $ Range (a, b)

parseIndex :: ReadP Range
parseIndex = do
  a <- read <$> munch1 isNumber
  return $ Range (a, a)
  
--------------------------------------------------------------------------------

instance Show AcbCalcIntegrateOpt where
  show x = unsafePerformIO $ do
    (_, result) <- withAcbCalcIntegrateOpt x $ \x -> do
      CAcbCalcIntegrateOpt deg eval depth use_heap verbosity <- peek x
      return $ "options:"
             ++ " deg=" ++ show deg
             ++ " eval=" ++ show eval
             ++ " depth=" ++ show depth
             ++ " heap=" ++ show use_heap
             ++ " verbosity=" ++ show verbosity
    return result

