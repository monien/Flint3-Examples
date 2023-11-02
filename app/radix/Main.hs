import System.CPUTime
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Storable
import Control.Monad
import Text.Printf

import Data.Number.Flint

main = do

  let n = 12376
      n_poly = (fromIntegral n) `div` 26
      m = 17^26 :: Fmpz

  state <- newFRandState
  
  ctx <- newFmpzModCtx m
  
  r <- newFmpzModPoly ctx
  t <- newFmpzModPoly ctx

  makePolynomials n m state r t ctx
  radixInfo n m r t ctx

  radix <- timeItNamed "precomputation" $ do
    newFmpzModPolyRadix r (n + 1) ctx

  b <- _fmpz_mod_poly_vec_init (fromIntegral n + 1) ctx

  timeItNamed "conversion" $ do
    withFmpzModPoly t $ \t -> do
      withFmpzModCtx ctx $ \ctx -> do
        withFmpzModPolyRadix radix $ \radix -> do
          fmpz_mod_poly_radix b t radix ctx
          -- forM_ [0..10] $ \j -> do
          --   p <- peek (b `advancePtr` j)
          --   fmpz_mod_poly_print p ctx
          --   putStr "\n"

  _fmpz_mod_poly_vec_clear b (n_poly + 1) ctx
      
radixInfo n m r t ctx = do
  withFmpzModPoly r $ \r -> do
    withFmpzModPoly t $ \t -> do
      withFmpzModCtx ctx $ \ctx -> do

        deg    <- fmpz_mod_poly_degree r ctx 
        polDeg <- fmpz_mod_poly_degree t ctx
        (_, bits)  <- withFmpz m fmpz_bits

        putStrLn "Radix conversion\n\
                 \-----------------"
        putStrLn $ "  Degree of the radix:     " ++ show deg
        putStrLn $ "  Bit size of the modulus: " ++ show bits
        putStrLn $ "  Degree of the input:     " ++ show polDeg
  putStr "\n"
  return ()

makePolynomials n m state r t ctx = do
  withFmpzModPoly r $ \r -> do
    withFmpzModPoly t $ \t -> do
      withNewFmpzModPoly ctx $ \u -> do
        withNewFmpzModPoly ctx $ \v -> do
          withFmpzModCtx ctx $ \ctx -> do
   
           fmpz_mod_poly_set_coeff_ui u 3 5 ctx
           fmpz_mod_poly_set_coeff_ui u 4 4 ctx

           fmpz_mod_poly_set_coeff_ui v 0 1 ctx
           fmpz_mod_poly_set_coeff_ui v 2 1 ctx
           fmpz_mod_poly_set_coeff_ui v 3 5 ctx
           fmpz_mod_poly_set_coeff_ui v 4 1 ctx
           fmpz_mod_poly_set_coeff_ui v 5 5 ctx
           fmpz_mod_poly_set_coeff_ui v 8 8 ctx
           fmpz_mod_poly_set_coeff_ui v 9 8 ctx
           fmpz_mod_poly_set_coeff_ui v 10 5 ctx
           fmpz_mod_poly_set_coeff_ui v 12 6 ctx
           fmpz_mod_poly_set_coeff_ui v 13 1 ctx

           fmpz_mod_poly_pow r u 3 ctx

           withNewFmpz $ \a -> do
             fmpz_set_ui a 4
             fmpz_mod_poly_scalar_mul_fmpz r r a ctx
             fmpz_set_ui a 27
             fmpz_mod_poly_scalar_mul_fmpz t t a ctx

           fmpz_mod_poly_add r r t ctx

           withFRandState state $ \state -> do
             fmpz_mod_poly_randtest t state (fromIntegral n + 1) ctx
  return ()

timeItNamed :: String -> IO a -> IO a
timeItNamed s f = do
  t0 <- getCPUTime
  result <- f 
  t1 <- getCPUTime
  let dt = fromIntegral (t1 - t0) / 10^9  :: Double
  printf "%s: %.6f ms\n" s dt
  return result

_fmpz_mod_poly_vec_init :: Int -> FmpzModCtx -> IO (Ptr (Ptr CFmpzModPoly))
_fmpz_mod_poly_vec_init n ctx = do
  vec <- mallocArray n :: IO (Ptr (Ptr CFmpzModPoly))
  result <- forM [0 .. fromIntegral n - 1] $ \j -> do
    poly <- malloc :: IO (Ptr CFmpzModPoly)
    withFmpzModCtx ctx $ \ctx -> do
      fmpz_mod_poly_init poly ctx
    return poly
  pokeArray vec result
  return vec

_fmpz_mod_poly_vec_clear :: (Ptr (Ptr CFmpzModPoly))
                        -> Int -> FmpzModCtx -> IO ()
_fmpz_mod_poly_vec_clear vec n ctx = do
  withFmpzModCtx ctx $ \ctx -> do
    forM [0 .. fromIntegral n - 1] $ \j -> do
      p <- peek (vec `advancePtr` j)
      fmpz_mod_poly_clear p ctx
  return ()
  

             

           

 