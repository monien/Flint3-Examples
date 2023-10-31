import Data.Number.Flint

import Control.Monad
import Foreign.C.Types
import Foreign.C.String

main = do
  fileName <- newCString "calcium.out"
  mode <- newCString "w"
  fp <- fopen fileName mode
  cs <- newCalciumStreamFile fp
  ctx <- newCaCtx
  x <- newCa ctx
  v <- newCaVec 6 ctx
  poly <- newCaPoly ctx
  prod <- newCaPoly ctx
  withCaCtx ctx $ \ctx -> do
    withCaPoly poly $ \poly -> do
      withCaPoly prod $ \prod -> do
        withCa x $ \x -> do
          ca_poly_one prod ctx
          ca_poly_x poly ctx
          forM_ [0..10] $ \j -> do
            ca_set_si x (-j) ctx
            ca_poly_x poly ctx
            ca_poly_set_coeff_ca poly 0 x ctx
            ca_poly_mul prod prod poly ctx
          ca_poly_print prod ctx; putStr "\n"
          ca_pi_i x ctx
          ca_print x ctx; putStr "\n"
          ca_euler x ctx
          ca_print x ctx; putStr "\n"
          withCaVec v $ \v -> ca_vec_print v ctx
  flag <- fclose fp
  return ()
  
foreign import ccall "stdio.h fopen"
  fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall "stdio.h fclose"
  fclose :: Ptr CFile -> IO CInt

foreign import ccall "stdio.h fputs"
  fputs :: CString -> Ptr CFile -> IO CInt
