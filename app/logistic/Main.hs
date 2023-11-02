import Options.Applicative

import Control.Monad
import Control.Monad.State

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Number.Flint

main = run =<< execParser opts where
  desc = "Compute nth iterate of the logistic map x_{n+1} = r x_n (1-x_n)."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n xs rs digits) = do
  print params
  let goal = round (fromIntegral digits / logBase 10 2 + 3) :: CLong
  withNewArb $ \x -> do
    withNewArb $ \r -> do
      withNewArb $ \s -> do
        withNewArb $ \t -> do
          _ <- runStateT (next n goal) (0, xs, rs, x, r, s, t, 64)
          putStr $ "x_" ++ show n ++ " = "
          arb_printn x digits arb_str_none
          putStr "\n"

next :: CLong -> CLong
     -> StateT (CLong, String, String, Ptr CArb, Ptr CArb, Ptr CArb, Ptr CArb, CLong) IO ()
next n goal = do
  (i, xs, rs, x, r, s, t, prec) <- get
  when (i == 0) $ liftIO $ do
    putStr $ "Trying precision " ++ show prec ++ " bits ... "
    getArb x xs prec
    getArb r rs prec
  if i < n then do 
    success <- liftIO $ do
      arb_sub_ui t x 1 prec
      arb_neg t t
      arb_mul x x t prec
      arb_mul x x r prec
      p <- arb_rel_accuracy_bits x
      return $ p >= goal
    if success then do
      put (i + 1, xs, rs, x, r, s, t, prec)
    else do
      liftIO $ putStrLn $ "ran out of precision at step " ++ show i
      put (0, xs, rs, x, r, s, t, 2 * prec)
    next n goal
  else do
    liftIO $ putStrLn "success!"
            
getArb x s prec = do
  withCString s $ \cs -> do
    flag <- arb_set_str x cs prec
    is_finite <- arb_is_finite x
    when (flag /= 0 || is_finite /= 1) $ do error $ "could no parse " ++ s
  
data Parameters = Parameters {
    n :: CLong
  , x0 :: String
  , r  :: String
  , digits :: CLong
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> argument pos (
      help "nth iterate of the logistic map."
   <> metavar "n")
  <*> strOption (
      help "starting point."
   <> long "x0"
   <> value "0.5"
   <> metavar "x0")
  <*> strOption (
      help "r parameter of logistic map."
   <> long "r"
   <> value "3.75"
   <> metavar "r")
  <*> option auto (
      help "number of digits."
   <> long "digits"
   <> short 'd'
   <> value 10
   <> metavar "digits")

pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"


