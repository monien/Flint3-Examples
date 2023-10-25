import Control.Monad
import Foreign.Ptr 
import Foreign.C.String
import Foreign.Marshal.Alloc

import Data.Number.Flint

main = do
  putStrLn "Output:\n"

  -- Case 1

  putStrLn "Positive integer:  x = 127 mod 7^10"

  forM_ [padic_terse, padic_series, padic_val_unit] $ \printMode -> do
    withNewPadicCtx 7 8 12 printMode $ \ctx -> do
      withNewPadic $ \x -> do
        padic_init2 x 10
        padic_set_ui x 127 ctx
        putStr "print:   "; padic_print x ctx; putStr "\n"
        cstr <- padic_get_str nullPtr x ctx
        str <- peekCString cstr
        free cstr
        putStrLn $ "getStr:  " ++ str

  -- Case 2

  putStrLn "Positive integer larger than p^N:  x = 1057 mod 2^10"

  forM_ [padic_terse, padic_series, padic_val_unit] $ \printMode -> do
    withNewPadicCtx 2 8 12 printMode $ \ctx -> do
      withNewPadic $ \x -> do
        padic_init2 x 10
        padic_set_ui x 1057 ctx
        putStr "print:   "; padic_print x ctx; putStr "\n"
        cstr <- padic_get_str nullPtr x ctx
        str <- peekCString cstr
        free cstr
        putStrLn $ "getStr:  " ++ str

  -- Case 3

  putStrLn "Negative integer:  x = -127 mod 3^10"

  forM_ [padic_terse, padic_val_unit] $ \printMode -> do
    withNewPadicCtx 2 8 12 printMode $ \ctx -> do
      withNewPadic $ \x -> do
        padic_init2 x 10
        padic_set_ui x (-127) ctx
        putStr "print:   "; padic_print x ctx; putStr "\n"
        cstr <- padic_get_str nullPtr x ctx
        str <- peekCString cstr
        free cstr
        putStrLn $ "getStr:  " ++ str

  -- Log

  putStrLn "Log of 7380996 mod 5^20"
  
  withNewPadicCtx 5 10 25 padic_series $ \ctx -> do
    withNewPadic $ \x -> do
      withNewPadic $ \y -> do
        padic_set_ui x 7380996 ctx
        padic_log y x ctx
        putStr "x = "; padic_print x ctx; putStr "\n"
        putStr "y = "; padic_print y ctx; putStr "\n"

      
      
      