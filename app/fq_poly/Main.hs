import System.CPUTime
import Text.Printf
import Control.Monad
import Data.Number.Flint

main = do

  putStrLn  "Polynomial multiplication over GF(q)\n\
            \------------------------------------\n\
            \\ntimes measured per function application\n"

  state <- newFRandState

  withFRandState state $ \state -> do
  
    putStrLn "1)  Two length-10,000 polynomials over GF(3^2)"
    
    ctx <- newFqCtxConway 3 2 "X"
    fqPolyTest state ctx 10000 1 100 100 

    putStrLn "2)  Two length-500 polynomials over GF(3^263)"
    
    ctx <- newFqCtxConway 3 263 "X"
    fqPolyTest state ctx 500 1 1 100

    putStrLn "3)  Two length-5 polynomials over GF(109987^4)"
    
    ctx <- newFqCtxConway 109987 4 "X"
    fqPolyTest state ctx 5 10000 10000 10000

-- test and timing -------------------------------------------------------------

fqPolyTest state ctx len c r k = do
  putStr "\n"
  withNewFqPoly ctx $ \f -> do
    withNewFqPoly ctx $ \g -> do
      withNewFqPoly ctx $ \h -> do
        withFqCtx ctx $ \ctx -> do

          fq_poly_randtest g state len ctx
          fq_poly_randtest h state len ctx

          timeItNamed "Classical" (fq_poly_mul_classical f g h ctx) c
          timeItNamed "Reorder"   (fq_poly_mul_reorder   f g h ctx) r
          timeItNamed "KS"        (fq_poly_mul_KS        f g h ctx) k
  putStr "\n"
  
timeItNamed s f n = do
  t0 <- getCPUTime
  replicateM n f
  t1 <- getCPUTime
  let dt = fromIntegral (t1 - t0) / 10^9 / (fromIntegral n) :: Double
  printf "%s: %.6f ms\n" s dt




  