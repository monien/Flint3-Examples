module Run where

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

data DC = DC CULong CULong deriving Show

data Parameters = Parameters {
    function :: Int
  , a :: CDouble
  , b :: CDouble
  , character :: Maybe DC
  , refine :: CInt
  , verbose :: Int
  , maxDepth :: CLong
  , maxEval :: CLong
  , maxFound :: CLong
  , prec :: CLong
  } deriving Show

run params@(Parameters function a b character refine verbose
                       maxDepth maxEval maxFound prec) = do
                       
  let digits = fromIntegral (max 2 refine)
      low_prec = prec
      high_prec = round $ fromIntegral digits / logBase 10 2 + 10

  putStrLn $ "maxDepth = " ++ show maxDepth
  putStrLn $ "maxEval  = " ++ show maxEval
  putStrLn $ "low_prec = " ++ show low_prec

  -- here we don't use the wrapper to keep track of allocation
  
  [t, interval] <- replicateM 2 malloc; mapM_ arf_interval_init [t, interval]
  [v, w, z] <- replicateM 3 malloc; mapM_ arb_init [v, w, z]
  c <- malloc; arf_init c
  
  info   <- malloc
  blocks <- malloc
  params <- malloc :: IO (Ptr ())

  g <- newDirichletGroup (case character of
                            Just (DC p r) -> p
                            Nothing       -> 1)
  gx <- newDirichletChar g
  
  (_,(_, z_params)) <- withDirichletGroup g $ \g -> do
    withDirichletChar gx $ \gx -> do
      dirichlet_char_log gx g (case character of
                                 Just (DC p r) -> r
                                 Nothing       -> 1)
      return (CZParam g gx)
  
  fp <- makeFunPtr ([z_function, sin_x, sin_x2, sin_1x, airy] !! function)

  arf_interval_set_d_d interval a b

  -- parmeters
  
  case function of
    0 -> case character of
           Just _ -> poke (castPtr params) z_params
           _      -> poke (castPtr params) (CZParam nullPtr nullPtr)
    1 -> return ()
    2 -> return ()
    3 -> return ()
    4 -> poke (castPtr params) (0 :: CInt)
    5 -> poke (castPtr params) (1 :: CInt)
    6 -> poke (castPtr params) (2 :: CInt)
    7 -> poke (castPtr params) (3 :: CInt)

  num <- arb_calc_isolate_roots blocks info fp
                                (castPtr params) interval maxDepth
                                maxEval maxFound low_prec
  bp <- peek blocks
  ip <- peek info

  putStrLn $ "Isolated " ++ show num  ++ " roots in intervals"

  res <- forM [0.. fromIntegral num-1] $ \i -> do
    info_i <- peek (ip .+. i)
    when (info_i /= 1 && verbose > 0 ) $ do 
      arf_interval_printd (bp.+.i) 15; endl
    when (refine > 0) $ do
      -- bisection -------------------------------------------------------------

      res <- arb_calc_refine_root_bisect t fp params (bp.+.i) 5 low_prec
      
      when (res /= arb_calc_success) $ do
        putStrLn "warning: some bisection steps failed!"
      when (verbose > 0) $ do
        putStr "after bisection 1: "; arf_interval_printd t 15; endl
        
      res <- arb_calc_refine_root_bisect (bp.+.i) fp params t 5 low_prec
      when (res /= arb_calc_success) $ do
        putStrLn "warning: some bisection steps failed!"
      when (verbose > 0) $ do
        putStr "after bisection 2: "; arf_interval_printd (bp.+.i) 15; endl
        
      --- Newton iteration -----------------------------------------------------
      arf_interval_get_arb v t high_prec
      arb_calc_newton_conv_factor c fp params v low_prec
      
      arf_interval_get_arb w (bp.+.i) high_prec
      
      res <- arb_calc_refine_root_newton z fp params w v c 10 high_prec
      when (res /= arb_calc_success) $ do
        putStrLn "warning: some newton steps failed!"
        
      putStrLn $ "refined root (" ++ show i ++ "/" ++ show num ++ ")"
      arb_printn z (fromIntegral digits + 2) 0
      putStr "\n\n"
    -- count zeros
    return $ if info_i == 1 then (1, 0) else (0, 1)

  let (found, unknown) = foldr (\(x, y) (ax, ay) -> (x+ax, y+ay)) (0, 0) res
  putStr $ "Found " ++ show found ++ " roots"
  if unknown > 0 then do
     putStrLn $ ", where " ++ show unknown
              ++ "intervals contained undetected roots."
  else do
    putStr ".\n"

  mapM_ (\x -> do free x; arf_interval_clear x) [t, interval]
  mapM_ (\x -> do free x; arb_clear x) [v, w, z]
  arf_clear c; free c

  free params
  free info
  free blocks

-- utility functions -----------------------------------------------------------

arf_interval_set_d_d x a b = do
  arf_set_d (castPtr x) a
  arf_set_d (castPtr x .+. 1) b

(.+.) x y = x `advancePtr` fromIntegral y
endl = putStr "\n"