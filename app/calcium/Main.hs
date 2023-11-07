import System.IO.Unsafe

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

import Data.Map (Map, (!), (!?))

import Data.Number.Flint

main = do
  let n = 12
  x <- toFexpr "x"
  f <- g
  poly <- hermiteH n
  heading $ "Hermite polynomial of degree " ++ show n
  print poly
  heading "expanded to normal form"
  print =<< normalForm poly
  heading "LaTeX representation "
  print =<< latex poly
  heading "another expression"
  print f
  heading "LaTeX representation"
  print =<< latex f

heading x = putStrLn $ "\n\ESC[1;34m" ++ x 
        ++ "\n" ++ (take (length x) $ repeat '‾') ++ "\ESC[0m"
  
--------------------------------------------------------------------------------

g = do
  f <- toFexpr FEXPR_Hypergeometric2F1
  a <- toFexpr (1//2 :: Fmpq)
  b <- toFexpr (1//3 :: Fmpq)
  c <- toFexpr (1//7 :: Fmpq)
  res <- newFexpr
  withFexpr f $ \f -> do
    withFexpr a $ \a -> do
      withFexpr b $ \b -> do
        withFexpr c $ \c -> do
          withFexpr res $ \res -> do
            fexpr_call3 res f a b c
  return res

-- horner :: Num a => [a] -> a -> a
horner x = foldr (\c sum -> c+x*sum) 0

hermiteH n = do
  coeffs <- mapM toFexpr $ reverse $ toList (hermitePolynomial n) 
  x <- toFexpr "x"
  return $ horner x coeffs

normalForm x = do
  withFexpr x $ \ x -> do
    fexpr_expanded_normal_form x x 0
  return x

latex x = do
  ptr <- malloc :: IO (Ptr CCalciumStream)
  calcium_stream_init_str ptr
  (_, result) <- withFexpr x $ \x -> do
    fexpr_write_latex ptr x 0
    CCalciumStream _ cs len _  <- peek ptr
    s <- peekCString cs
    free cs
    return s
  return result

--------------------------------------------------------------------------------

testCalcium = do
  ctx <- newCaCtx
  x <- newCa ctx
  withCa x $ \x -> do
    withCaCtx ctx $ \ctx -> do
      ca_one x ctx
      ca_div_ui x x 2 ctx
  v <- newCaVec 6 ctx
  poly <- newCaPoly ctx
  prod <- newCaPoly ctx
  ext <- newCaExtFx ca_Cos x ctx
  withCaCtx ctx $ \ctx -> do
    withCaExt ext $ \ext -> do
      ca_ext_print ext ctx; putStr "\n"
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
          
testCalciumStream = do
  h <- hermiteH 11
  w <- g
  fileName <- newCString "calcium.out"
  mode <- newCString "w"
  fp <- fopen fileName mode
  cs <- newCalciumStreamFile fp
  withCalciumStream cs $ \cs -> do
    withFexpr h $ \h -> do
      fexpr_write cs h
      fexpr_write_latex cs h 0
      fexpr_expanded_normal_form h h 0
    withFexpr w $ \w -> do
      fexpr_write cs w
      fexpr_write_latex cs w 0
  flag <- fclose fp
  return ()

foreign import ccall "stdio.h fopen"
  fopen :: CString -> CString -> IO (Ptr CFile)

foreign import ccall "stdio.h fclose"
  fclose :: Ptr CFile -> IO CInt

foreign import ccall "stdio.h fputs"
  fputs :: CString -> Ptr CFile -> IO CInt

