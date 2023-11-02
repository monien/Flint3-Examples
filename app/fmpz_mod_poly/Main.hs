import Data.Number.Flint

main = do

  ctx <- newFmpzModCtx 7

  withNewFmpzModPoly ctx $ \x -> do
    withNewFmpzModPoly ctx $ \y -> do
      withFmpzModCtx ctx $ \ctx -> do
        fmpz_mod_poly_set_coeff_ui x 3 5 ctx
        fmpz_mod_poly_set_coeff_ui x 0 6 ctx
        fmpz_mod_poly_sqr y x ctx
        fmpz_mod_poly_print x ctx; putStr "\n"
        fmpz_mod_poly_print y ctx; putStr "\n"