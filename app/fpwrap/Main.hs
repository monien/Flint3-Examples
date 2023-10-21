{-# language FlexibleInstances #-}

import System.IO.Unsafe

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Text.Printf

import Data.Complex
import Data.Number.Flint hiding (magnitude, phase, realPart, imagPart)

main = do
  let x = 2.0 :: Double
      fx = zeta x
      z = 0.5 :+ 123.0 :: Complex Double
      fz = zeta z
  printf "zeta(%g) = %.16g\n" x fx
  printf "zeta(%g + %gi) = %.16g + %.16gi\n"
    (realPart z ) (imagPart z )
    (realPart fz) (imagPart fz)

-- implement instances for zeta for Double and Complex Double ------------------

instance Special Double where
  zeta x = unsafePerformIO $ do
    alloca $ \f -> do
      flag <- arb_fpwrap_double_zeta f (realToFrac x) 0
      result <- peek f
      return $ realToFrac $ result

instance Special (Complex Double) where
  zeta z = unsafePerformIO $ do
    alloca $ \f -> do
      alloca $ \zp -> do
        poke zp ((realToFrac.realPart) z :+ (realToFrac.imagPart) z)
        flag <- arb_fpwrap_cdouble_zeta f zp 0
        result <- peek f
        return $ ((realToFrac.realPart) result :+ (realToFrac.imagPart) result)