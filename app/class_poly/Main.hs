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
    [arg0] -> case readMaybe arg0 :: Maybe CLong of
      Just d -> run d 1
    [arg0, "--threads", arg2] -> case readMaybe arg0 :: Maybe CLong of
      Just d -> case readMaybe arg2 :: Maybe CInt of
        Just num_threads -> run d num_threads
        _ -> putStrLn $ prog ++ ": usage d [--threads n]"
      _ -> putStrLn $ prog ++ ": usage d [--threads n]"
    _ -> putStrLn $ prog ++ ": usage d [--threads n]"

run d num_threads = do
  if d < 0 then do
    if d `mod` 4 == 1 || d `mod` 4 == 0 then do
      flint_set_num_threads num_threads
      res <- newFmpzPoly
      withFmpzPoly res $ \res -> acb_modular_hilbert_class_poly res d
      when (abs d <= 100) $ print res
      return ()
    else do
      putStrLn $ "D ( = " ++ show d ++ ") `mod` 4 /= 0, 1."
  else do
    putStrLn "D > 0."
