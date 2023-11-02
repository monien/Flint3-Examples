import System.TimeIt

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array

import Control.Monad
import Options.Applicative
import Text.ParserCombinators.ReadP hiding (option)

import Data.Char
import Data.Number.Flint

main = run =<< execParser opts where
  opts = info (options <**> helper) (
         fullDesc
      <> progDesc "Discrete Fourier Transform for functions [0..5]."
      <> header "Calculate Discrete Fourier Transform.")

run opts@(Options n verbosity range qqbar_limit nogb timing num_threads) = do
  let desc = [ "x_k = k + 2"
             , "x_k = sqrt(k + 2)"
             , "x_k = log(k + 2)"
             , "x_k = exp(2 pi i / (k + 2))"
             , "x_k = 1 / (1 + (k + 2) pi)"
             , "x_k = 1 / (1 + sqrt(k + 2) pi)"
             ]
      Range (start, end) = range
      gb = if nogb then 0 else 1
  ctx <- newCaCtx
  withNewCa ctx $ \t -> do
    withCaCtx ctx $ \ctx -> do
      forM_ [start .. end] $ \j -> do
        putStrLn $ desc !! j
        putStr "\n"
        if timing then do
          timeIt $ benchmark_DFT n j verbosity qqbar_limit gb t ctx
        else do
          benchmark_DFT n j verbosity qqbar_limit gb t ctx

data Options = Options {
    n :: CLong
  , verbosity :: Int
  , range :: Range
  , limit :: CLong
  , nogb :: Bool
  , doTiming :: Bool
  , num_threads :: CInt
} deriving Show

options :: Parser Options
options = Options
  <$> argument pos (
      help "n"
   <> value 2
   <> metavar "n")
  <*> option pos (
      help "verbosity 0 .. "
   <> long "verbosity"
   <> short 'v'
   <> value 0
   <> metavar "verbosity")
  <*> option (rng 0 5) (
      help "select function to be transformed [0..5]"
   <> long "range"
   <> short 'r'
   <> metavar "range")
  <*> option pos (
      help "limit"
   <> long "limit"
   <> value 0 
   <> metavar "limit")
  <*> switch (
      help "do not use Groebner bases"
   <> long "nogb")
  <*> switch (
      help "timing"
   <> long "timing"
   <> short 't')
  <*> option pos (
      help "number of threads"
   <> long "threads"
   <> value 1
   <> metavar "threads")
  
-- ReadM parsers ---------------------------------------------------------------

rng :: Int -> Int -> ReadM Range
rng start end = eitherReader $ \s -> do
  let result@(Range (a, b)) = read s :: Range
  if start <= a && a <= b && b <= end then
    Right result
  else
    Left $ "Could not parse range " ++ s
  
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"

between a b = eitherReader $ \s -> do
  let result = read s
  if a <= result && result <= b  then 
    Right result
  else
    Left $ "expected number in range [" ++ show a ++ " .. " ++ show b ++ "]."

-- instances Range -------------------------------------------------------------

newtype Range = Range (Int, Int)

instance Read Range where
  readsPrec _ = readP_to_S (parseRange <|> parseIndex)

instance Show Range where
  show (Range (a, b)) = "[" ++ show a ++ ":" ++ show b ++ "]"

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

benchmark_DFT n input verbose qqbar_limit gb t ctx = do

  let (.+.) x y =  x `advancePtr` (fromIntegral y)
  
  x  <- _ca_vec_init n ctx
  x' <- _ca_vec_init n ctx
  y  <- _ca_vec_init n ctx
  w  <- _ca_vec_init (2*n) ctx

  -- ca_ctx_set_option ctx ca_opt_print_flags ca_print_debug
  -- ca_ctx_set_option ctx ca_opt_verbose 1
  
  ca_ctx_set_option ctx ca_opt_use_groebner gb
  
  when (qqbar_limit /= 0) $ do
    ca_ctx_set_option ctx ca_opt_qqbar_deg_limit qqbar_limit

  --  Construct input vector

  when (verbose > 0) $ do putStr "[x] =\n"
    
  forM_ [0 .. n - 1] $ \i -> do
    let xi = x .+. i
    case input of
      0 -> do
        ca_set_si xi (i + 2) ctx
      1 -> do
        ca_set_si xi (i + 2) ctx
        ca_sqrt xi xi ctx
      2 -> do
        ca_set_si xi (i + 2) ctx
        ca_log xi xi ctx
      3 -> do
        ca_pi_i xi ctx
        ca_mul_ui xi xi 2 ctx
        ca_div_si xi xi (i + 2) ctx
        ca_exp xi xi ctx
      4 -> do
        ca_pi xi ctx
        ca_mul_si xi xi (i + 2) ctx
        ca_add_ui xi xi 1 ctx
        ca_inv xi xi ctx
      5 -> do
        ca_pi xi ctx
        ca_sqrt_ui w (fromIntegral i + 2) ctx
        ca_mul xi xi w ctx
        ca_add_ui xi xi 1 ctx
        ca_inv xi xi ctx 
    when (verbose > 0) $ do
      ca_print xi ctx
      putStr "\n"

  -- construct roots of unity

  when (verbose > 1) $ do putStrLn "\n[w] = "
  
  forM_ [0 .. 2 * n - 1] $ \i -> do
    let wi = w .+. i
    if i == 0 then do
      ca_one wi ctx
    else do
      if i == 1 then do
        ca_pi_i wi ctx
        ca_mul_ui wi wi 2 ctx
        ca_div_si wi wi n ctx
        ca_exp wi wi ctx
      else do
        ca_mul wi (w .+. (i - 1)) (w .+. 1) ctx
    when (verbose > 1) $ do
      ca_print wi ctx; putStr "\n"
      
  -- forward dft: x -> x'

  when (verbose > 2) $ do putStrLn "\nDFT([x]) = "

  forM_ [0 .. n - 1] $ \k -> do
    let x'k = x' .+. k
    ca_zero x'k ctx
    forM_ [0 .. n - 1] $ \j -> do
      let xj = x .+. j
          wj = w .+. (((2 * n - k) * j) `mod` (2 * n))
      ca_mul t xj wj ctx
      ca_add x'k x'k t ctx
    when (verbose > 2) $ do
      ca_print x'k ctx; putStr "\n"

  -- inverse dft

  when (verbose > 2) $ do putStrLn "\nIDFT(DFT([x])) ="

  forM_ [0 .. n - 1] $ \k -> do
    let yk = y `advancePtr` (fromIntegral k)
    ca_zero yk ctx
    forM_ [0 .. n - 1] $ \j -> do
      let x'j = x' .+. j
          wj = w .+.  ((k * j) `mod` (2 * n))
      ca_mul t x'j wj ctx
      ca_add yk yk t ctx
    ca_div_si yk yk n ctx
    when (verbose > 2) $ do
      ca_print yk ctx; putStr "\n"

  when (verbose > 0) $ do putStrLn "\n[x] - IDFT(DFT([x])) ="

  forM_ [0 .. n - 1] $ \k -> do
    let xk = x .+. k
        yk = y .+. k
    ca_sub t xk yk ctx
    is_zero <- ca_check_is_zero t ctx
    
    when (verbose > 0) $ do
      ca_print t ctx
      putStrLn $ "       (= 0   " ++  show is_zero ++ ")"
      when (is_zero /= t_true) $ do error "Failed to prove equality!"

  when (verbose > 0) $ do putStr "\n"

  _ca_vec_clear x  n ctx
  _ca_vec_clear x' n ctx
  _ca_vec_clear y  n ctx
  _ca_vec_clear w  (2 * n) ctx
  