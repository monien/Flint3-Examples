main = do
  [a, b, c] <- mapM (fmap lines . readFile) ["funs", "desc", "lims"]
  let r = zipWith3 makeLine [0..] b c
  putStr $ unlines r

makeLine x y z = "  , ( f" ++ show x ++ ", " ++ y ++ ", " ++ z ++ ")"