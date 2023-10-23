import System.IO.Unsafe

import Options.Applicative
import Control.Monad
import Control.Applicative ((<|>))

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc (free)
import Foreign.Storable

import Text.ParserCombinators.ReadP hiding (option)
import Text.Read (readMaybe)
import Text.Printf

import Data.Char
import Data.List (intercalate)
import Data.Number.Flint

import Integrands

main = run =<< execParser opts where
  hDesc = "Calculate integrals using acb_calculate."
  desc = "Calculate integrals from list in range 0:"
       ++ show (length integrands) ++ ". "
       ++ "For a list of implemented integrals use --list."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header hDesc)

run params = do
  if list params then do
    let desc = fst $ unzip integrands
    putStrLn "List of implemented integrals:\n"
    mapM_ (\(x, y) -> printf "  %2d  %s\n" x y) $ zip [0 :: Int ..] desc
  else do
    calc params
    
calc params@(Parameters list range prec goal' tol twice
                        heap verbose deg eval depth num_threads) = do
  when (verbose > 0) $ print params
  flint_set_num_threads num_threads
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
            forM_ [start .. end] $ \j -> do
              let (desc, h) = integrands !! j
              f <- makeFunPtr h
              flag <- case j of
                0 -> do
                  acb_set_d a 0
                  acb_set_d b 100
                  makeFunPtr f_sin
                  acb_calc_integrate s f nullPtr a b goal tol opts prec
                1 -> do
                  acb_set_d a 0
                  acb_set_d b 1
                  flag <- acb_calc_integrate s f nullPtr a b goal tol opts prec
                  acb_mul_2exp_si s s 2
                  return flag
                2 -> do
                  acb_set_d a 0
                  acb_set_d b 1
                  acb_mul_2exp_si b b goal
                  flag <- acb_calc_integrate s f nullPtr a b goal tol opts prec
                  arb_add_error_2exp_si (acb_realref s) (-goal)
                  acb_mul_2exp_si s s 1
                  return flag
                3 -> do
                  acb_set_d a 0
                  acb_set_d b 1
                  flag <- acb_calc_integrate s f nullPtr a b goal tol opts prec
                  acb_mul_2exp_si s s 2
                  return flag
                4 -> do
                  acb_set_d a 0
                  acb_set_d b 8
                  acb_calc_integrate s f nullPtr a b goal tol opts prec
                5 -> do
                  acb_set_d a 1
                  acb_set_d b 101
                  acb_calc_integrate s f nullPtr a b goal tol opts prec
                _ -> do
                  putStrLn "everything else"
                  return 1
              let digits = round (0.333 * fromIntegral prec) :: CLong
              putStrLn $ "I" ++ show j ++ " = " ++ desc
              acb_printn s digits arb_str_none
              putStr "\n\n"
  return ()
  
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
   <> value (Range (0, length integrands-1))
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
  if 0 <= a && a <= b && b < length integrands - 1 then
    Right result
  else
    Left $ "could not parse range"
  
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left $ "expected positive number"

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
  return $ result
     
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

--------------------------------------------------------------------------------

foreign import ccall "wrapper"
  makeFunPtr :: CAcbCalcFunc -> IO (FunPtr CAcbCalcFunc)
 