module Functions (
    z_function
  , sin_x
  , sin_x2
  , sin_1x
  , airy
  , makeFunPtr
  , F
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Storable

import Control.Monad

import Data.Number.Flint hiding (airy)

import FFI

foreign import ccall safe "wrapper"
  makeFunPtr :: F -> IO (FunPtr F)

type F = CArbCalcFunc
     
z_function, sin_x, sin_x2, sin_1x, airy :: F

z_function out inp params order prec = do

  CZParam g chi <- peek (castPtr params :: Ptr CZParam)

  when (g /= nullPtr) $ do
    putStr "char: "
    dirichlet_char_print g chi
    putStr "\n"
  -- putStrLn $ "g = " ++ show g
  -- putStrLn $ "chi = " ++ show chi
      
  if g == nullPtr then do
    x <- _arb_vec_init 2
    arb_set x inp
    arb_one (x .+. 1)
    _arb_poly_riemann_siegel_z_series out x (min 2 order) order prec
    _arb_vec_clear x 2
  else do
    tmp <- _acb_vec_init order
    acb_set_arb tmp inp
    acb_dirichlet_hardy_z tmp tmp g chi order prec
    forM_ [0 .. order - 1] $ \k -> do
      arb_set (out .+. k) (acb_realref (tmp .+. k))
    _acb_vec_clear tmp order

  return 0


sin_x out inp params order prec = do

  let xlen = min 2 order
  
  arb_set out inp
  when (xlen > 1) $ do arb_one (out .+. 1)
  _arb_poly_sin_series out out xlen order prec

  return 0
      
sin_x2 out inp params order prec = do

  let xlen = min 2 order
      ylen = min 3 order
      
  x <- _arb_vec_init xlen
  _arb_poly_mullow out x xlen x xlen ylen prec
  _arb_poly_sin_series out out ylen order prec
  _arb_vec_clear x xlen

  return 0


sin_1x out inp params order prec = do

  let xlen = min  2 order

  x <- _arb_vec_init xlen

  arb_set x inp
  when (xlen > 1) $ do arb_one (x .+. 1)

  _arb_poly_inv_series out x xlen order prec
  _arb_poly_sin_series out out order order prec

  _arb_vec_clear x xlen

  return 0

  
airy out inp params order prec = do

  let xlen = min 2 order

  which <- peek (castPtr params) :: IO CInt

  -- putStrLn $ "airy: which = " ++ show which

  withNewAcb $ \t -> do
    withNewAcb $ \u -> do
      acb_set_arb t inp

      if  xlen == 1 then do
        case which of 
          0 -> acb_hypgeom_airy t       nullPtr nullPtr nullPtr t prec
          1 -> acb_hypgeom_airy nullPtr t       nullPtr nullPtr t prec
          2 -> acb_hypgeom_airy nullPtr nullPtr t       nullPtr t prec
          _ -> acb_hypgeom_airy nullPtr nullPtr nullPtr t       t prec
        arb_set out (acb_realref t)
      else do 
        if  which == 0 || which == 1 then do
          acb_hypgeom_airy t u nullPtr nullPtr t prec
        else do 
          acb_hypgeom_airy nullPtr nullPtr t u t prec
        if  which == 0 || which == 2 then do
          arb_set (out .+. 0) (acb_realref t)
          arb_set (out .+. 1) (acb_realref u)
          -- f'' z = z f z 
          when (xlen == 3) $ do arb_mul (out .+. 2) out inp prec
        else do
          arb_set (out .+. 0) (acb_realref u)
          arb_mul (out .+. 1) (acb_realref t) inp prec
          -- f''' z = f z + z f' z 
          when (xlen == 3) $ do
            arb_mul (out .+. 2) out inp prec
            arb_add (out .+. 2) (out .+. 2) (acb_realref t) prec

  return 0

-- utility functions -----------------------------------------------------------

(.+.) x y = x `advancePtr` (fromIntegral y)

next x = do
  value <- peek x
  poke x (value + 1)