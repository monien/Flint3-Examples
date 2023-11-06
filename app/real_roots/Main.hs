{-# language OverloadedStrings #-}

import Data.Typeable

import Options.Applicative
import Options.Applicative.Help.Pretty hiding (char)
import Text.ParserCombinators.ReadP hiding (option, between, optional)

import Control.Monad
import Control.Monad.State

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

import Data.Char
import Data.Number.Flint hiding (airy)

import Functions
import FFI
import Run

main :: IO ()
main = run =<< customExecParser (prefs showHelpOnEmpty) opts where
  opts = info (parameters <**> helper) 
       ( fullDesc
      <> header "\nRoot finding (real roots).\n"
      <> progDescDoc (Just desc))
      
desc = vcat funs
funs = 
  [ "Examples of root finding for some examples (0..7)."
  , "  0  Z(x), Z-function (Riemann zeta or Dirichlet L-function)"
  , "  1  sin(x)"
  , "  2  sin(x^2)"
  , "  3  sin(1/x)"
  , "  4  Ai(x), Airy function"
  , "  5  Ai'(x), Airy function"
  , "  6  Bi(x), Airy function"
  , "  7  Bi'(x), Airy function"
  , "With 0, specify optional Dirichlet character with \
    \[-character q n]"
  ]

-- Parser ----------------------------------------------------------------------

parameters :: Parser Parameters
parameters = Parameters
  <$> argument (between 0 7) (
      help "function"
   <> metavar "FUNCTION")
  <*> option auto (
      help "a"
   <> long "xa"
   <> metavar "A")
  <*> option auto (
      help "b"
   <> long "xb"
   <> metavar "B")
  <*> optional optionDC 
  <*> option auto (
      help "refine "
   <> long "refine"
   <> value 0
   <> metavar "DIGITS")
   <*> option pos (
      help "verbose"
   <> long "verbose"
   <> short 'v'
   <> value 0
   <> metavar "VERBOSE")
  <*> option auto (
      help "maximal depth"
   <> long "maxdepth"
   <> value 30
   <> metavar "MAX-DEPTH")
  <*> option auto (
      help "maximal number of evaluations"
   <> long "maxeval"
   <> value 100000
   <> metavar "MAX-EVAL")
  <*> option auto (
      help "maximal number of zeros to be found"
   <> long "maxfound"
   <> value 100000
   <> metavar "MAX-FOUND")
  <*> option auto (
      help "precision"
   <> long "precision"
   <> short 'p'
   <> value 64
   <> metavar "PRECISION")

optionDC = option dc (
     help "Dirichlet character for L-function. Specify in the form q.r, \
          \e.g. 12.7"
  <> long "character")
  
pos :: (Read a, Integral a) => ReadM a
pos = eitherReader $ \s -> do
  let result = read s
  if result >= 0 then 
    Right result
  else
    Left "expected positive number"

between a b = eitherReader $ \s -> do
  let result = read s
  if a <= result && result <= b  then 
    Right result
  else
    Left $ "expected number in range [" ++ show a ++ " .. " ++ show b ++ "]."

-- read Dirichlet Character ----------------------------------------------------

dc :: ReadM DC
dc = eitherReader $ \s -> do
  let chi@(DC p r) = read s
  if p > 0 && r >= 0 then
    Right chi
  else
    Left "expected Dirichlet character, specified as p.r"

instance Read DC where
  readsPrec _ = readP_to_S parseChar

parseChar = do
  p <- read <$> munch1 isNumber
  char '.'
  q <- read <$> munch1 isNumber
  return $ DC p q


-- testing ---------------------------------------------------------------------

testZParam = do

  g <- newDirichletGroup 12
  c <- newDirichletChar g

  withDirichletGroup g $ \g -> do
    withDirichletChar c $ \c -> do
      print g
      print c
      dirichlet_char_print g c
      putStr "\n"
      params <- malloc :: IO (Ptr CZParam)
      poke params (CZParam g c)
      putStrLn "alloc works."
      let ptr = castPtr params :: Ptr ()
      print ptr
      CZParam u v <- peek (castPtr ptr :: Ptr CZParam)
      putStrLn "new pointer."
      print u
      print v
      dirichlet_char_print u v
      putStr "\n"
      poke params (CZParam nullPtr nullPtr)
      CZParam u v <- peek (castPtr ptr :: Ptr CZParam)
      putStrLn "new pointer."
      print u
      print v
      putStr "\n"
      free params
  return ()