import System.TimeIt

import Control.Monad
import Data.List (intercalate)
import Foreign.ForeignPtr
import Foreign.C.Types

import Data.Number.Flint

-- This program checks several variations of Machin’s formula ------------------

main = timeItNamed "machin" $ do
  checkFormula m_formulas simple_ca_atan_p_q  "atan"
  checkFormula h_formulas simple_ca_atanh_p_q "atanh"
    
checkFormula formulas f fName = do
  ctx <- newCaCtx
  withNewCa ctx $ \x -> do
    withNewCa ctx $ \y -> do
      withCaCtx ctx $ \ctx -> do
        withNewFmpq $ \arg -> do
          forM_ formulas $ \(rhs, coeffs) -> do
            -- evaluate formula -> x
            ca_zero x ctx
            s <- forM coeffs $ \(c, q) -> do
              f y 1 q ctx
              ca_mul_si y y c ctx
              ca_add x x y ctx
              return $ "(" ++ show c ++ ")*" ++ fName ++ "(1/" ++ show q ++ ")"
            putStr $ "[" ++ intercalate "+" s ++ "] - "
            -- evaluate result -> y
            let (p, q) = rhs
            fmpq_set_si arg p (fromIntegral q)
            putStr $ fName ++ "("; fmpq_print arg; putStr ") = "
            f y p q ctx
            -- formula - result
            ca_sub x x y ctx
            ca_print x ctx; putStr "\n"

simple_ca_atan_p_q res p q ctx = do
  ca_set_si res p ctx
  ca_div_si res res q ctx
  ca_atan res res ctx

simple_ca_atanh_p_q res p q ctx = do
  ca_set_si res p ctx
  ca_div_si res res q ctx
  simple_ca_atanh res res ctx

-- valid for -1 < x < 1
simple_ca_atanh res x ctx = do
  t <- mallocForeignPtr
  u <- mallocForeignPtr
  withForeignPtr t $ \t -> do
    withForeignPtr u $ \u -> do
      ca_init t ctx
      ca_init u ctx
      ca_add_si t x 1 ctx
      ca_sub_si u x 1 ctx
      ca_neg u u ctx
      ca_div res t u ctx
      ca_log res res ctx
      ca_div_si res res 2 ctx
      ca_clear t ctx
      ca_clear u ctx

m_formulas :: [((CLong, CLong), [(CLong, CLong)])]
m_formulas = 
  [ ((1, 1), [(1, 1)])
  , ((1, 1), [(1, 2), (1, 3)])
  , ((1, 1), [(2, 2), (-1, 7)])
  , ((1, 1), [(2, 3), (1, 7)])
  , ((1, 1), [(4, 5), (-1, 239)])
  , ((1, 1), [(1, 2), (1, 5), (1, 8)])
  , ((1, 1), [(1, 3), (1, 4), (1, 7), (1, 13)])
  , ((1, 1), [(12, 49), (32, 57), (-5, 239), (12, 110443)])
  ]

h_formulas :: [((CLong, CLong), [(CLong, CLong)])]
h_formulas =
  [ ((3, 5), [(14, 31), (10, 49), (6, 161)])
  , ((4, 5), [(22, 31), (16, 49), (10, 161)])
  , ((12, 13), [(32, 31), (24, 49), (14, 161)])
  , ((3, 5), [(144, 251), (54, 449), (-38, 4801), (62, 8749)])
  , ((4, 5), [(228, 251), (86, 449), (-60, 4801), (98, 8749)])
  , ((12, 13), [(334, 251), (126, 449), (-88, 4801), (144, 8749)])
  , ((24, 25), [(404, 251), (152, 449), (-106, 4801), (174, 8749)])
  ]