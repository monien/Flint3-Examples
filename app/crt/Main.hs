import System.Environment
import System.TimeIt

import Options.Applicative
import Control.Monad
import Control.Monad.State
import Text.Read (readMaybe)

import Foreign.C.Types
import Foreign.Ptr

import Data.Number.Flint

main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [arg0] -> case readMaybe arg0 :: Maybe Fmpz of
                Just x-> run x
                _ -> putStrLn $ "usage: " ++ prog ++ " <integer>."
    _ -> putStrLn $ "usage: " ++ prog ++ " <integer>."

run x = do
  print x
  withFmpz x $ \x -> do
    bits <- fmpz_bits x
    withNewFmpz $ \y -> do
      fmpz_zero y
      withNewFmpz $ \prod -> do
        fmpz_one prod
        _ <- runStateT (next x (bits + 2)) (0, y, prod, 0)
        return ()
  return ()

next :: Ptr CFmpz -> CFBitCnt
     -> StateT (Int, Ptr CFmpz, Ptr CFmpz, CULong) IO ()
next x bit_bound = do
  (i, y, prod, prime) <- get
  (prod', y', prime', bits) <- liftIO $ do
    prime' <- n_nextprime prime 0
    res <- fmpz_fdiv_ui x prime'
    fmpz_CRT_ui y y prod res prime' 1
    putStr $ "residue mod " ++ show prime' ++ " = " ++ show res
    putStr $ "; reconstruction = "
    fmpz_print y
    putStr "\n"
    fmpz_mul_ui prod prod prime'
    bits <- fmpz_bits prod
    return (prod, y, prime', bits)
  put (succ i, y', prod', prime')
  when (bits < bit_bound) $ next x bit_bound
    
  