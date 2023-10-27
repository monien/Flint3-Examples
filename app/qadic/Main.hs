import Foreign.Storable
import Foreign.Marshal.Array (advancePtr)

import Data.Number.Flint

main = do

  let n = 5

  putStrLn "Compute a power and a sum"
          
  withNewQadicCtxConway 3 2 0 n "a" padic_series $ \ctx -> do
    CQadicCtx pctx _ _ _ _ <- peek ctx
    withNewQadicWithPrec n $ \a -> do
      withNewQadicWithPrec n $ \b -> do
        withNewQadicWithPrec n $ \c -> do

          withFmpzPoly (fromList [1, 2]) $ \poly -> do
            padic_poly_set_fmpz_poly a poly pctx
          qadic_print_pretty a ctx; putStr "\n"
          withFmpz 4 $ \four -> qadic_pow a a four ctx
          padic_poly_set_ui b 3249 pctx
          qadic_add c a b ctx
          
          qadic_print_pretty a ctx; putStr "\n"
          qadic_print_pretty b ctx; putStr "\n"
          qadic_print_pretty c ctx; putStr "\n"
          putStr "\n"
          
          putStrLn "Compute a Teichmuller lift"
          
          withFmpzPoly (fromList [1, 2]) $ \poly -> do
            padic_poly_set_fmpz_poly a poly pctx
          qadic_teichmuller b a ctx
          withFmpz 9 $ \nine -> qadic_pow c b nine ctx
          
          qadic_print_pretty a ctx; putStr "\n"
          qadic_print_pretty b ctx; putStr "\n"
          qadic_print_pretty c ctx; putStr "\n"
          putStr "\n"
          
          putStrLn "Compute an inverse"

          qadic_set a b
          qadic_inv b a ctx
          qadic_mul c a b ctx

          qadic_print_pretty a ctx; putStr "\n"
          qadic_print_pretty b ctx; putStr "\n"
          qadic_print_pretty c ctx; putStr "\n"
          putStr "\n"

  ------------------------------------------------------------------------------

  putStrLn "Compute a Frobenius image"

  withNewQadicCtxConway 3 2 0 n "X" padic_terse $ \ctx -> do
    CQadicCtx pctx _ _ _ _ <- peek ctx
    withNewQadicWithPrec n $ \a -> do
      withNewQadicWithPrec n $ \b -> do
      
        withFmpzPoly (fromList [78, 45]) $ \poly -> do
          padic_poly_set_fmpz_poly a poly pctx

        qadic_frobenius b a 1 ctx
        
        putStr "a = "; qadic_print_pretty a ctx; putStr "\n"
        putStr "b = "; qadic_print_pretty b ctx; putStr "\n"
        putStrLn "Context:"; qadic_ctx_print ctx; putStr "\n"
        putStr "\n"
        
  ------------------------------------------------------------------------------

  putStrLn "Compute a square root"

  withNewQadicCtxConway 2 3 0 n "X" padic_series $ \ctx -> do
    CQadicCtx pctx _ _ _ _ <- peek ctx
    withNewQadicWithPrec n $ \a -> do
      withNewQadicWithPrec n $ \b -> do

        withFmpzPoly (fromList [1, 3, 1]) $ \poly -> do
          padic_poly_set_fmpz_poly a poly pctx

        ans <- qadic_sqrt b a ctx

        putStr "a = "; qadic_print_pretty a ctx; putStr "\n"
        putStr "b = "; qadic_print_pretty b ctx; putStr "\n"
        putStrLn $ "ans = " ++ show ans
        putStrLn "Context:"; qadic_ctx_print ctx; putStr "\n"
        putStr "\n"
    

          
                