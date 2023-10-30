import Control.Monad
import Data.Word
import Data.Bits

import System.TimeIt
import Options.Applicative

import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array

import Data.Number.Flint

main = timeItNamed "time" $ run =<< execParser opts where
  desc = "Computes the coefficients of the Swinnerton-Dyer polynomial."
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc desc
      <> header desc)

run params@(Parameters n) = do
  print params
  let m = 1 `shiftL` (fromIntegral n)
  putStrLn $ "m = " ++ show m 
  ctx <- newCaCtx
  withCaCtx ctx $ \ctx -> do
    poly <- _ca_vec_init (m+1) ctx
    swinnerton_dyer_poly poly n (m+1) ctx
    forM_ [0 .. fromIntegral m - 1] $ \j -> do
      ca_print (poly `advancePtr` j) ctx; putStr "\n"
    putStr "\n"
    _ca_vec_clear poly (m+1) ctx

-- Parser Parameters -----------------------------------------------------------

data Parameters = Parameters {
  n :: CLong
  } deriving (Show, Eq)

parameters :: Parser Parameters
parameters = Parameters
  <$> argument (0 `between` 20) (
      help "n"
   <> metavar "n")

between a b = eitherReader $ \s -> do
  let result = read s
  if a <= result && result <= b  then 
    Right result
  else
    Left $ "expected number in range [" ++ show a ++ " .. " ++ show b ++ "]."

--------------------------------------------------------------------------------

_ca_poly_mullow res x xlen y ylen len ctx = do

  forM_ [0 .. fromIntegral len-1] $ \j -> do
    ca_zero (res `advancePtr` j) ctx

  t <- mallocForeignPtr
  withForeignPtr t $ \t -> do 
    ca_init t ctx

    forM_ [0 .. xlen - 1] $ \i -> do
      let xi = x `advancePtr` (fromIntegral i)
      forM_ [0 .. min ylen ( len - i) - 1] $ \j -> do
        let yj = y `advancePtr` (fromIntegral j)
            rij = res `advancePtr` (fromIntegral (i+j))
        ca_mul t xi yj ctx
        ca_add rij rij t ctx

    ca_clear t ctx

swinnerton_dyer_poly :: Ptr CCa -> CLong -> CLong -> Ptr CCaCtx -> IO ()
swinnerton_dyer_poly t n trunc ctx = do

  let m = 1 `shiftL` (fromIntegral n)
      k = min trunc (fromIntegral m + 1)

  square_roots <- _ca_vec_init n ctx    :: IO (Ptr CCa)
  tmp1 <- mallocArray (m `div` 2 + 1) :: IO (Ptr CCa)
  tmp2 <- mallocArray (m `div` 2 + 1) :: IO (Ptr CCa)
  tmp3 <- _ca_vec_init (fromIntegral m) ctx

  forM_ [0.. n - 1] $ \i -> do
    p <- n_nth_prime (fromIntegral i + 1)
    ca_sqrt_ui (square_roots `advancePtr` (fromIntegral i)) p ctx
    ca_print (square_roots `advancePtr` (fromIntegral i)) ctx; putStr "\n"
    
  forM_ [0 .. fromIntegral m - 1] $ \i -> do
    let ti = t `advancePtr` i
    ca_zero ti ctx
    forM_ [0 .. fromIntegral n - 1] $ \j -> do
      if (i `shiftR` j) .&. 1 == 1 then do
        ca_add ti ti (square_roots `advancePtr` j) ctx
      else do
        ca_sub ti ti (square_roots `advancePtr` j) ctx

  -- for each level ...

  forM_ [0 .. fromIntegral n - 1] $ \i -> do
    let stride = 1 `shiftL` i
    forM_ [0 .. fromIntegral m - 1] $ \j -> do
      ca_one (tmp1 `advancePtr` (fromIntegral stride)) ctx
      ca_one (tmp1 `advancePtr` (fromIntegral stride)) ctx
      _ca_poly_mullow tmp3 tmp1 (stride +1)
                           tmp2 (stride+21) (min (2*stride) trunc) ctx
      _ca_vec_set (t `advancePtr` j) tmp3   (min (2*stride) trunc) ctx        
        