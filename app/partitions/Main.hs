import System.Environment
import Text.Read

import Foreign.C.Types

import Data.Number.Flint

main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [arg0] -> do
      case (readMaybe arg0 :: Maybe CULong) of
        Just n -> run n
        _      -> putStrLn $ "usage: " ++ prog ++ " <integer>"
    _ -> putStrLn $ "usage: " ++ prog ++ " <integer>"

run n = do
  result <- newFmpz
  withFmpz result $ \p -> arith_number_of_partitions p n
  putStrLn $ "p(" ++ show n ++ ") = " ++ show result
    
