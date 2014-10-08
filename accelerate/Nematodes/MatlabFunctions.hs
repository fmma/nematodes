{-# language RankNTypes, ScopedTypeVariables #-}

module Nematodes.MatlabFunctions (

  bwconnectcomp, centroid,

) where

import Prelude hiding (map, length, fromIntegral, round)

import Nematodes.Types
import Data.Array.Accelerate

-- Compute connected component of tresholded regions.
bwconnectcomp :: Acc (Matrix Bool)       -- Black and white image.
              -> Acc (Vector (Int, Int)) -- Vector of points in cc.
bwconnectcomp _ = use (fromList (Z :. 0) []) -- TODO

centroid :: Acc (Vector (Int, Int))
         -> Acc (Scalar (Int, Int))
centroid c = map (`div` length c) (foldAll plus p0 c)
  where
    plus :: Exp (Int, Int) -> Exp (Int, Int) -> Exp (Int, Int)
    plus p1 p2 =
      let (x1,y1) = unlift p1 :: (Exp Int, Exp Int)
          (x2,y2) = unlift p2 :: (Exp Int, Exp Int)
      in lift (x1+x2,y1+y2)

    p0 :: Exp (Int, Int)
    p0 = lift (0 :: Int, 0 :: Int)

    div :: Exp (Int, Int) -> Exp Int -> Exp (Int, Int)
    div p1 z =
      let (x,y) = unlift p1 :: (Exp Int, Exp Int)
          xf :: Exp Float
          xf = fromIntegral x
          yf :: Exp Float
          yf = fromIntegral y
          zf :: Exp Float
          zf = fromIntegral z
      in lift (round $ xf / zf, round $ yf / zf)
