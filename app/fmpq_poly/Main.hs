import System.IO.Unsafe
import Data.Char
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc 
import GHC.Read
-- import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Number.Flint
import Debug.Trace

main = do
  let f, g :: FmpqPoly
      f = read "2  1/2 3/5"
      g = read "4  1/3 2 3/2 -1/2"
  print f
  print g
  print $ f*g

