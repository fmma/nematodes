module Nematodes.Types (

  Prec, Matrix, Frames, VarArgIn(..), Tform(..), Ratio, Yfp, Cfp

) where

import Data.Array.Accelerate

type Prec = Float

type Matrix a = Array DIM2 a
type Frames = Array DIM3 Prec

data VarArgIn = VarArgIn { thresh :: Prec, bThreash :: Prec, circle :: Prec }

data Tform a = 
    Affine (Matrix a)
  | Projective (Matrix a)
  | Custom 
      Int -- NDIMS_IN 
      Int -- NDIMS_OUT
      (Acc (Matrix a) -> Acc (Matrix a)) -- FORWARD_FCN
      (Acc (Matrix a) -> Acc (Matrix a)) -- INVERSE_FCN
      [String] -- TDATA

type Ratio = Vector Prec
type Yfp = Vector Prec
type Cfp = Vector Prec
