module FFI where

import Data.Number.Flint

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Storable

#include "z_param.h"

data ZParam = ZParam {-# UNPACK #-} !(ForeignPtr CZParam)
data CZParam = CZParam (Ptr CDirichletGroup) (Ptr CDirichletChar)

instance Storable CZParam where
  {-# INLINE sizeOf #-}
  sizeOf    _ = #{size      z_param_t}
  {-# INLINE alignment #-}
  alignment _ = #{alignment z_param_t}
  peek ptr = CZParam
    <$> #{peek z_param_struct, G  } ptr 
    <*> #{peek z_param_struct, chi} ptr 
  poke ptr (CZParam g c) = do
    #{poke z_param_struct, G  } ptr g
    #{poke z_param_struct, chi} ptr c

