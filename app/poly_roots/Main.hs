import System.TimeIt

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array

import Options.Applicative
import Options.Applicative.Help.Pretty

import Control.Monad
import Data.IORef

import Data.Number.Flint

-- main = putStrLn "done"

main = timeItNamed "time"
     $ run =<< customExecParser (prefs showHelpOnEmpty) opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Isolates all the complex roots of a polynomial with \
                  \integer coefficients.."
      <> header "Roots of polynomials")

run params@(Parameters digitsRefine digitsPrint polys) = do
  let prec = case digitsRefine of
               Just digits -> round $ fromIntegral digits / logBase 10 2 + 2
               Nothing     -> 16
  print params
  if not (null polys) then do
    pols <- product <$> mapM getPolynomial polys
    forM_ (factor pols) $ \(poly, e) -> do
      putStrLn $ "polynomial expression " ++ show poly 
               ++ " with multiplicity " ++ show e 
      withFmpzPoly poly $ \p -> do
        n <- fmpz_poly_degree p
        putStrLn $ "polynomial degree: " ++ show n
        z <- _acb_vec_init n
        arb_fmpz_poly_complex_roots z p arb_fmpz_poly_roots_verbose prec
        case digitsPrint of
          Just digits -> do
            forM_ [0..fromIntegral n-1] $ \j -> do
              acb_printn (z.+.j) digits arb_str_no_radius
              putStr "\n"
          Nothing -> return ()
        _acb_vec_clear z n
      return ()
  else do
    putStrLn $ "no polynomial expression given."

(.+.) x y = x `advancePtr` (fromIntegral y)

getPolynomial :: Polynomial -> IO FmpzPoly
getPolynomial x =
  case x of
    A n -> easyA n
    T n -> chebyshevT n
    U n -> chebyshevT n
    P n -> legendreP' n
    C n -> cyclotomicC n
    S n -> swinnertonDyerS n
    B n -> bernoulliB' n

data Polynomial
  = A CULong
  | T CULong
  | U CULong
  | P CULong
  | C CULong
  | S CULong
  | B CULong
  | W CULong
  | E CULong
  | M CULong
  | Coeffs [Integer]
  deriving Show

-- Parser ----------------------------------------------------------------------

data Parameters = Parameters {
  digitsRefine :: Maybe CLong
, digitsPrint :: Maybe CLong
, poly :: [Polynomial]
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> optional ( option auto (
      help "the roots are refined to a relative tolerance\
           \better than 10^(-d). By default, the roots are \
           \only computed to sufficient\
           \accuracy to isolate them. The refinement is not\
           \currently done efficiently"
   <> long "refine"
   <> short 'r'
   <> metavar "RELATIVE-TOLERANCE"))
  <*> optional ( option auto (
      help "computed roots are printed to d decimals. \
           \By default, the roots are not printed."
   <> short 'd'
   <> long "print"
   <> metavar "DIGITS") )
  <*> many polynomial

polynomial :: Parser Polynomial
polynomial = subparser cmds where
  cmds = mconcat $ zipWith mkCmd desc [a, t, u, p, phi, s, b, w, e, m, coeffs]
  mkCmd x y = command (takeWhile (/=' ') x) (info y (progDesc x))

desc =
  [ "a <n>      Easy polynomial 1 + 2x + ... + (n+1)x^n"
  , "t <n>      Chebyshev polynomial T_n"
  , "u <n>      Chebyshev polynomial U_n"
  , "p <n>      Legendre polynomial P_n"
  , "c <n>      Cyclotomic polynomial Phi_n"
  , "s <n>      Swinnerton-Dyer polynomial S_n"
  , "b <n>      Bernoulli polynomial B_n"
  , "w <n>      Wilkinson polynomial W_n"
  , "e <n>      Taylor series of exp(x) truncated to degree n"
  , "m <n> <m>  The Mignotte-like polynomial x^n + (100x+1)^m, n > m"
  , "coeffs <c0 c1 ... cn>        c0 + c1 x + ... + cn x^n"
  ]

a, t, u, p, phi, s, b, w, e, m, coeffs :: Parser Polynomial

a   = A   <$> argument auto (metavar "n")
t   = T   <$> argument auto (metavar "n")
u   = U   <$> argument auto (metavar "n")
p   = P   <$> argument auto (metavar "n")
phi = C   <$> argument auto (metavar "n")
s   = S   <$> argument auto (metavar "n")
b   = B   <$> argument auto (metavar "n")
w   = W   <$> argument auto (metavar "n")
e   = E   <$> argument auto (metavar "n")
m   = M   <$> argument auto (metavar "n")
coeffs = Coeffs <$> many (argument auto (metavar "coeffs"))

-- polynomials -----------------------------------------------------------------

-- Legendre polynomial (denominator removed)
legendreP' n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    withNewFmpqPoly $ \h -> do
      fmpq_poly_legendre_p h n
      fmpq_poly_get_numerator p h
  return poly

chebyshevT n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    fmpz_poly_chebyshev_t p n
  return poly

chebyshevU n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    fmpz_poly_chebyshev_u p n
  return poly

cyclotomicC n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    fmpz_poly_cyclotomic  p n
  return poly

swinnertonDyerS n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    fmpz_poly_swinnerton_dyer p n
  return poly

-- Bernoulli polynomial (denominator removed)
bernoulliB' n = do
  (poly, _) <- withNewFmpzPoly $ \p -> do
    withNewFmpqPoly $ \h -> do
      arith_bernoulli_polynomial h n
      fmpq_poly_get_numerator p h
  return poly

easyA n = do
  let poly = fromList (map fromIntegral [1..n]) :: FmpzPoly
  return poly
