import Control.Monad
import Data.Char

main = do
  contents <- readFile "tmp"
  putStrLn "data FEXR_Builtiin ="
  forM_ (init $ lines contents) $ \line -> do
    putStrLn $ "  | " ++ line
  putStrLn "  deriving (Show, Eq)"

  let f j line = putStrLn $ "hash " ++ line ++ " = "  ++ show j
  zipWithM_ f [0..] (init $ lines contents)

