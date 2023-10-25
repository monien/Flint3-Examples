import System.Environment
import Text.Read

import Foreign.C.Types

import Data.Number.Flint

main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [arg0] -> do
      case (readMaybe arg0 :: Maybe CLong) of
        Just n -> run n
        _      -> putStrLn $ "usage: " ++ prog ++ " <integer>"
    _ -> putStrLn $ "usage: " ++ prog ++ " <integer>"

run n = do
  s1 <- newFmpzMat n n
  s2 <- newFmpzMat n n
  withFmpzMat s1 $ \s1 -> arith_stirling_matrix_1 s1
  withFmpzMat s2 $ \s2 -> arith_stirling_matrix_2 s2
  let p = s1*s2
  putStrLn "S1 [Stirling numbers of 1st kind]:"
  print s1
  putStrLn "S2 [Stirling numbers of 1st kind]:"
  print s2
  putStrLn "S1 * S2:"
  print p
  
    