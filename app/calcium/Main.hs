import Data.Number.Flint

import Foreign.C.Types
import Foreign.C.String

main = do
  fileName <- newCString "calcium.out"
  mode <- newCString "w"
  fp <- fopen fileName mode
  cs <- newCalciumStreamFile fp
  ctx <- newCaCtx
  x <- newCa ctx
  v <- newCaVec 16 ctx
  withCaCtx ctx $ \ctx -> do 
    withCa x $ \x -> do
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
