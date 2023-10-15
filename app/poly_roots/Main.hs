import System.TimeIt

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import Options.Applicative
import Options.Applicative.Help.Pretty

import Control.Monad
import Control.Monad.Loops
import Data.IORef

import Data.Number.Arb

main = timeItNamed "time" $ run =<< execParser opts where
  opts = info (parameters <**> helper) (
         fullDesc
      <> progDesc "Isolates all the complex roots of a polynomial with integer coefficients.."
      <> header "Roots of polynomials")

run p@(Parameters digitsRefine digitsPrint poly) = do
  let prec = fromIntegral $ floor (fromIntegral digitsRefine * 3.32193 + 2)
      digits = fromIntegral digitsPrint
  print p
  p <- getPolynomial (head poly)
  withFmpzPoly p $ \p -> do
    n <- fmpz_poly_degree p
    putStrLn $ "polynomial degree: " ++ show n
    z <- _acb_vec_init n
    x <- _acb_vec_list_ptr z n
    _arb_fmpz_poly_complex_roots z p arb_fmpz_poly_roots_verbose prec
    forM_ x $ \x -> do
      acb_printn x digits arb_str_no_radius
      putStr "\n"
    _acb_vec_clear z n
    
getPolynomial x = do
  p <- newFmpzPoly
  withFmpzPoly p $ \p -> do
    case x of 
      T   n -> arb_fmpz_poly_chebyshev_t_polynomial p (fromIntegral n)
      U   n -> arb_fmpz_poly_chebyshev_t_polynomial p (fromIntegral n)
      P   n -> arb_fmpz_poly_legendre_polynomial p (fromIntegral n)
      Phi n -> arb_fmpz_poly_cyclotomic_polynomial p (fromIntegral n)
      S   n -> arb_fmpz_poly_swinnerton_dyer_polynomial p (fromIntegral n)
      B   n -> arb_fmpz_poly_bernoulli_polynomial p (fromIntegral n)
      _ -> error "polynomial not known"
  return p



data Polynomial = A Int
                | T Int
                | U Int
                | P Int
                | Phi Int
                | S Int
                | B Int
                | W Int
                | E Int
                | M Int
                | Coeffs [Integer]
  deriving Show
  
data Parameters = Parameters {
  digitsRefine :: Int,
  digitsPrint :: Int,
  poly :: [Polynomial]
} deriving Show

parameters :: Parser Parameters
parameters = Parameters
  <$> option auto (
     long "refine" <>
     help "the roots are refined to a relative tolerance\
          \better than 10^(-d). By default, the roots are \
          \only computed to sufficient\
          \accuracy to isolate them. The refinement is not\
          \currently done efficiently" <>
     value 32 <>
     metavar "d")
  <*> option auto (
    long "print" <>
    help "computed roots are printed to d decimals.\n\
         \By default, the roots are not printed." <>
    value 32 <>
    metavar "d")
  <*> many polynomial
  
polynomial :: Parser Polynomial
polynomial = subparser cmds where
  cmds = mconcat $ zipWith mkCmd desc [a, t, u, p, phi, s, b, w, e, m, coeffs]
  mkCmd x y = command (takeWhile (/=' ') x) (info y (progDesc x))

desc = [
  "a <n>      Easy polynomial 1 + 2x + ... + (n+1)x^n", 
  "t <n>      Chebyshev polynomial T_n",
  "u <n>      Chebyshev polynomial U_n",
  "p <n>      Legendre polynomial P_n",
  "c <n>      Cyclotomic polynomial Phi_n",
  "s <n>      Swinnerton-Dyer polynomial S_n",
  "b <n>      Bernoulli polynomial B_n",
  "w <n>      Wilkinson polynomial W_n",
  "e <n>      Taylor series of exp(x) truncated to degree n",
  "m <n> <m>  The Mignotte-like polynomial x^n + (100x+1)^m, n > m",
  "coeffs <c0 c1 ... cn>        c0 + c1 x + ... + cn x^n"]

a, t, u, p, phi, s, b, w, e, m, coeffs :: Parser Polynomial

a   = A   <$> (argument auto (metavar "n"))
t   = T   <$> (argument auto (metavar "n"))
u   = U   <$> (argument auto (metavar "n"))
p   = P   <$> (argument auto (metavar "n"))
phi = Phi <$> (argument auto (metavar "n"))
s   = S   <$> (argument auto (metavar "n"))
b   = B   <$> (argument auto (metavar "n"))
w   = W   <$> (argument auto (metavar "n"))
e   = E   <$> (argument auto (metavar "n"))
m   = M   <$> (argument auto (metavar "n"))
coeffs = Coeffs <$> many (argument auto (metavar "coeffs"))
