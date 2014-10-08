{-# language ViewPatterns #-}

module Nematodes.Calcium.CalciumProcess (

  calcium_process,

) where

import Prelude hiding (map, zipWith, length, fromIntegral, (++), unzip, round, sum)

import Nematodes.Types
import Nematodes.MatlabFunctions
import Nematodes.Calcium.ThreshEstimate ( thresh_estimate )
import Nematodes.Splitter.Splitter2 ( splitter2 )

import Data.Array.Accelerate

{-
 % try to make a mask based on the largest connected
 % component.
    BWmax = double(BWmax);
    BWmax(CCmax.PixelIdxList{largest_max}) = 2;
    lhs_mask = double(BWmax == 2);
    strel_size = 15;
    se = strel('ball',strel_size,strel_size);
    lhs_mask = imdilate(lhs_mask,se) > strel_size;
    rhs_mask = lhs_mask;
-}
stencilStuff :: Acc (Matrix Prec)       -- [lhs, rhs]
             -> Acc (Scalar (Int, Int)) -- Smax
             -> Acc (Matrix Bool, Matrix Bool) -- (lsh_mask, rhs_mask)
stencilStuff _ _ = lift (fromList (Z :. 0 :. 0) [], fromList (Z :. 0 :. 0) []) -- TODO

calcium_process :: Frames -> VarArgIn -> Acc (Ratio, Yfp, Cfp)
calcium_process frames varargin =
  let 
    tform :: Tform Prec
    tform = undefined -- TODO find_good_reference_frame
  in collect $ calcium_process_seq tform (toSeqInner (use frames))

  -- TODO handle_args
  -- The following assumes the following simplifications from the matlab version
  --   1. Only one connected component
  --   2. use_circle == 0
  --   3. handle_backround == 0
calcium_process_seq :: Tform Prec -> Seq [Matrix Prec] -> Seq (Ratio, Yfp, Cfp)
calcium_process_seq tform im =
  let split :: Seq [(Matrix Prec, Matrix Prec)]
      split = mapSeq (splitter2 tform) im

      thresh :: Seq [(Scalar Prec, Scalar Prec)]
      thresh = mapSeq (thresh_estimate . afst) split

      bWmax :: Seq [Matrix Bool]
      bWmax =
        zipWithSeq
          (\ x y -> map (>* the (afst y)) (afst x))
          split
          thresh

      cCmax :: Seq [Vector (Int, Int)]  -- Simplification (1)
      cCmax = mapSeq bwconnectcomp bWmax

      smax :: Seq [Scalar (Int, Int)]   -- Simplification (1)
      smax = mapSeq centroid cCmax

      mask :: Seq [(Matrix Bool, Matrix Bool)]  -- Simplification (2)
      mask =
        zipWithSeq
          (\ x y -> stencilStuff (afst x) y)
          split
          smax

      masked :: Seq [(Matrix Prec, Matrix Prec)]
      masked =
        zipWithSeq
          (\ mask split -> 
            let (lhs_mask, rhs_mask) = unlift mask
                (lhs, rhs) = unlift split
            in lift (zipWith (*) (map boolToFloat lhs_mask) lhs, zipWith (*) (map boolToFloat rhs_mask) rhs))
          mask
          split

      lhs_nnz :: Seq [Scalar Int]
      lhs_nnz = mapSeq (countPositive . afst) masked

      rhs_nnz :: Seq [Scalar Int]
      rhs_nnz = mapSeq (countPositive . asnd) masked

      yfp :: Seq [Scalar Prec]
      yfp =
        zipWithSeq
          (\ mask lhs_nnz -> zipWith (/) (foldAll (+) 0 (afst mask)) (map fromIntegral lhs_nnz))
          masked
          lhs_nnz

      cfp :: Seq [Scalar Prec]
      cfp =
        zipWithSeq
          (\ mask rhs_nnz -> zipWith (/) (foldAll (+) 0.0 (asnd mask)) (map fromIntegral rhs_nnz))
          masked
          rhs_nnz

      ratio :: Seq [Scalar Prec]
      ratio =
        zipWithSeq
          (zipWith (\ x y -> x / y - 0.6))
          yfp
          cfp

  in lift (fromSeqElems yfp, fromSeqElems cfp, fromSeqElems ratio)

countPositive :: Shape sh => Acc (Array sh Prec) -> Acc (Scalar Int)
countPositive acc = sum (map f acc)
  where
    f :: Exp Prec -> Exp Int
    f x = boolToInt (x >* constant 0.0)

boolToFloat :: Exp Bool -> Exp Float
boolToFloat = fromIntegral . boolToInt
