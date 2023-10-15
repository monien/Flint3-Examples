import System.TimeIt
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import Options.Applicative

import Control.Monad
import Control.Monad.Loops
import Data.IORef

import Data.Number.Arb

main = timeItNamed "time" $ run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Print central value of Dirichlet L-function."
      <> header "Print central value of Dirichlet L-function.")

run p@(Parameters quiet check prec qmin qmax) = do
  let digits = floor (fromIntegral prec * 0.3)
  withNewAcb $ \s -> do
    acb_one s
    acb_div_si s s 2 prec
    let qb = fromIntegral qmax * 0.5 * fromIntegral (qmax - qmin + 1) 
    pre <- newAcbDirichletHurwitzPrecompNum s 0 qb prec
    withAcbDirichletHurwitzPrecomp pre $ \pre ->
      forM_ [qmin..qmax] $ \q -> 
        when (q `mod` 4 /= 2) $ do
          withNewDirichletGroup q $ \g -> do
            n <- fromIntegral <$> dirichlet_group_size g 
            z <- _acb_vec_init n
            v <- _acb_vec_list_ptr z n
            acb_dirichlet_l_vec_hurwitz z s pre g prec
            withNewDirichletChar g $ \chi -> do
              dirichlet_char_one chi g
              forCharacters g chi (processCharacter quiet check q v digits)
            _acb_vec_clear z n 
          return ()

forCharacters g chi f = do
  let iterateDirichletChar k chi g f =
        when (next chi g) $ do
          f k g chi
          iterateDirichletChar (k+1) chi g f
      next chi g = unsafePerformIO $ do
        flag <- dirichlet_char_next chi g
        return $ flag >= 0
  dirichlet_char_one chi g
  iterateDirichletChar 1 chi g f

processCharacter quiet check q z digits k g chi = do
  let value = z !! k
  conductor <- dirichlet_conductor_char g chi
  when (conductor >= q) $ do
    m <- fromIntegral <$> dirichlet_char_exp g chi
    zero <- (==1) <$> acb_contains_zero value
    when (check && zero) $ putStrLn "value could be zero!"
    when (not quiet || (check && zero)) $ do
      putStr $ show q ++ ", " ++ show m ++ ": "
      acb_printn value digits arb_str_none
      putStr "\n"

data Parameters = Parameters {
  quiet :: Bool,
  check :: Bool,
  prec :: CLong,
  qmin :: CULong,
  qmax :: CULong
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> switch (
    long "quiet" <>
    short 'q')
  <*> switch (
    long "--check" <>
    short 'c')
  <*> option auto (
    long "prec" <>
    short 'p' <>
    value 100 <>
    metavar "precision")
  <*> argument auto (
    metavar "qmin")
  <*> argument auto (
    metavar "qmax")
    