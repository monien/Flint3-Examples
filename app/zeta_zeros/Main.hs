import System.IO.Unsafe
import Options.Applicative
import Control.Monad
import Control.Monad.State
import Foreign.C.Types
import Foreign.Marshal.Array 

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Reports the imaginary parts of consecutive nontrivial zeros \
         \of the Riemann zeta function starting with the nth zero."
  opts = info (options <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run opts@(Options n_start count accuracy platt verbosity num_threads) = do
  when (verbosity > 0) $ do print opts
  if platt && n_start < 10000 then do
    putStrLn "This implementation of the platt algorithm \
             \is not valid\n below the 10000th zero.\n"
  else do
    flint_set_num_threads num_threads
    let (prec, digs) = case accuracy of
          Just (Precision p) -> (p, d2p p)
          Just (Digits d)    -> (p2d d, d)
          Nothing  -> (pDefault, p2d pDefault)
          where pDefault = 64 + ceiling (logBase 2 (fromIntegral n_start))
        d2p p = round (fromIntegral p * log 2 + 1)
        p2d d = round (fromIntegral d / log 2 + 3)
        requested = fromIntegral $ min count 30000 
        usePlatt = platt || (requested > 100 && n_start > 10^11)
        workingPrecision = if platt then 2*prec else prec
        digits = p2d workingPrecision
    p <- _arb_vec_init requested
    let n = fromInteger n_start :: Fmpz
    -- _ <- runStateT (calculate p platt requested prec digits) (n, 0, 0, 0)
    -- return ()
    withFmpz n $ \n -> do
       if not platt then do
         acb_dirichlet_hardy_z_zeros p n requested prec
         print_zeros p n_start requested digits
       else do 
         found <- acb_dirichlet_platt_local_hardy_z_zeros p n requested prec
         if ( found > 0 ) then do
           print_zeros p n_start found digits
         else do
           putStrLn "Failed to find some zero.\nIncrease precision.\n"
    _arb_vec_clear p $ fromIntegral requested

-- calculate :: Ptr CArb -> Bool -> Integer -> CLong -> CLong
--           -> StateT (Fmpz, Integer, Integer, Integer) IO ()
-- calculate p platt requested prec digits = do
--   (n, iter, count, num_old) <- get
--   let num = if count + num_old > requested then requested-count-1 else 2*num_old
--   withFmpz n $ \n -> do 
--     if not platt then do
--       acb_dirichlet_hardy_z_zeros p n num prec
--       print_zeros p n_start num digits
--       fmpz_add_ui n n (fromIntegral num)
--       put (n, succ iter, count + num, num)
--     else do
--       found <- acb_dirichlet_platt_local_hardy_z_zeros p n num prec
--       print_zeros p n_start found digits
--       if ( found > 0 ) then do
--         print_zeros p n_start found digits
--       else do
--         error "Failed to find some zero.\nIncrease precision.\n"
--       fmpz_add_ui n n (fromIntegral found)
--       put (n, succ iter, count + found, num)
--   when (count < requested) $ do calculate p platt requested prec digits

 
data Options = Options {
  n_start :: Integer
, count :: Integer
, accuracy :: Maybe Accuracy
, platt :: Bool
, verbosity :: Int
, num_threads :: CInt
} deriving Show

data Accuracy = Precision CLong | Digits CLong deriving Show

-- option parser ---------------------------------------------------------------

options :: Parser Options
options = Options
  <$> option pos (
      help "integer n > 0. start from nth zero."
   <> short 'n'
   <> value 1
   <> metavar "n")
  <*> option pos (
      help "number of zeros to calculate (<30000)."
   <> long "count"
   <> short 'c'
   <> value 30000
   <> metavar "count")
  <*> optional optionAccuracy
  <*> switch (
      help "use platt algorithm."
   <> showDefault
   <> long "platt")
  <*> option pos (
      help "verbosity."
   <> long "verbosity"
   <> short 'v'
   <> value 0
   <> metavar "verbosity")
  <*> option pos (
      help "number of threads."
   <> long "threads"
   <> short 't'
   <> value 1
   <> metavar "threads")

optionAccuracy = optionPrecision <|> optionDigits

optionPrecision :: Parser Accuracy
optionPrecision =  Precision <$> option pos (
      help "precision."
   <> long "prec"
   <> short 'p'
   <> metavar "precision")

optionDigits :: Parser Accuracy
optionDigits = Digits <$> option pos (
      help "number of digits."
   <> long "digits"
   <> short 'd'
   <> metavar "digits")
   
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"

--------------------------------------------------------------------------------

print_zeros p n_start len digits = do
  forM_ [0 .. fromIntegral len - 1] $ \j -> do
    putStr $ show (fromIntegral n_start + j) ++ "\t"
    arb_printn (p `advancePtr` j) digits arb_str_no_radius
    putStr "\n"
-- print_zeros p n len digits = do
--   withNewFmpz $ \k -> do
--     fmpz_set k n
--     forM_ [0 .. len-1] $ \i -> do
--       fmpz_print k
--       putStr "\t"
--       arb_printn (p `advancePtr` i) digits arb_str_no_radius
--       putStr "\n"
--       fmpz_add_ui k k 1


