import System.TimeIt

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array (advancePtr)

import Options.Applicative
import Control.Monad

import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Print value of Dirichlet L-function at s = x + i y."
      <> header "Print value of Dirichlet L-function at s = x + i y.")

run p@(Parameters character n xs ys prec zfun deflate len) = do
  case gcd character n of
    1 -> do let deflate' = if deflate then 1 else 0
            lvalue character n xs ys prec zfun deflate' len
    _ -> putStrLn "need gcd(q, n) == 1 to define a character."
  
lvalue character n xs ys prec zfun deflate len = do
  z <- _acb_vec_init len
  p <- forM [0..len-1] $ \j -> return $ z `advancePtr` (fromIntegral j)
  g <- newDirichletGroup character
  withNewDirichletChar g $ \chi -> do
    withDirichletGroup g $ \g -> do
      putStr $ "character chi(" ++ show character ++ ", " ++ show n ++ ") is "
      dirichlet_char_log chi g n
      primitive <- dirichlet_char_is_primitive g chi
      if primitive == 1 then do
        putStrLn "primitive."
        withNewAcb $ \s -> do
          withNewArb $ \x -> do
            withNewArb $ \y -> do
              withCString xs $ \xs -> arb_set_str x xs prec
              withCString ys $ \ys -> arb_set_str y ys prec
              acb_set_arb_arb s x y
              if zfun then do
                acb_dirichlet_hardy_z z s g chi len prec
              else do
                acb_dirichlet_l_jet z s g chi deflate len prec
          forM_ (zip [0..] p) $ \(j, p) -> do
            let f = if zfun then "Z" else "L"
                prec' = fromIntegral $ floor $ fromIntegral prec / 3 + 1
            case j of
              0 -> putStr $ f ++ "(s) "
              1 -> putStr $ f ++ "'(s) "
              _ -> putStrLn $ "[x^" ++ show j ++ "] " ++ f
            acb_printn p prec' arb_str_none
            putStr "\n"
        return ()
      else
        putStrLn "not primitive."
  _acb_vec_clear z (fromIntegral len)

data Parameters = Parameters {
  character :: CULong,
  n :: CULong,
  xs :: String,
  ys :: String,
  prec :: CLong,
  zfun :: Bool,
  deflate :: Bool,
  len :: CLong
} deriving Show

-- default values give L(chi, 1/2)
-- 
-- see http://www.lmfdb.org/L/Character/Dirichlet/12/11/
parameters :: Parser Parameters
parameters = Parameters
  <$> option auto (
        long "character" <>
        value 12 <>
        help "Dirichlet character" <>
        metavar "character")
  <*> option auto (
        short 'n' <>
        value 11 <>
        metavar "n")
  <*> strOption (
        short 'x' <>
        value "0.5" <>
        metavar "x")
  <*> strOption (
        short 'y' <>
        value "0.0" <>
        metavar "y")
  <*> option auto (
        short 'p' <>
        long "precision" <>
        value 1024 <>
        metavar "prec")
  <*> switch (
        short 'z')
  <*> switch (
        short 'd' <>
        long "deflate")
  <*> option auto (
        long "len" <>
        short 'l' <>
        value 1 <>
        metavar "len")


