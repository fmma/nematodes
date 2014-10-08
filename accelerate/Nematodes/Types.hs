module Nematodes.Types (

  Matrix, Frames, VarArgIn(..), Tform(..),

) where

import Data.Array.Accelerate

type Matrix e = Array DIM2 e
type Frames e = Array DIM3 e

data VarArgIn e = VarArgIn { thresh :: e, bThreash :: e, circle :: e }

data Tform e = 
    Affine (Matrix e)
  | Projective (Matrix e)
  | Custom 
      Int -- NDIMS_IN 
      Int -- NDIMS_OUT
      (Acc (Matrix e) -> Acc (Matrix e)) -- FORWARD_FCN
      (Acc (Matrix e) -> Acc (Matrix e)) -- INVERSE_FCN
      [String] -- TDATA
