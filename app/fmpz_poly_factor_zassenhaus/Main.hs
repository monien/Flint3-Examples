import Foreign.C.String

import Data.Number.Flint

main = do
  f <- newFmpzPoly
  facs <- newFmpzPolyFactor
  withFmpzPoly f $ \f -> do
    withCString s $ \s -> do
      fmpz_poly_set_str f s
    withFmpzPolyFactor facs $ \facs -> do
      fmpz_poly_factor_zassenhaus facs f
      fmpz_poly_print f
      putStr "\n"
      fmpz_poly_factor_print facs
      putStr "\n"

s = "63  1 1 1 -4 -7 -2 -6 -3 -7 18 7 25 -11 95 36 21 16 69 56 35 36 32 33 26 -26 -15 -14 -53 -96 67 72 -67 40 -79 -116 -452 -312 -260 -29 -1393 327 69 -28 -241 230 -54 -309 -125 -74 -450 -69 -3 66 -27 73 68 50 -63 -1290 372 31 -16 2"