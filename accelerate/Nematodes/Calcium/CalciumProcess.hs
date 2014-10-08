{-# language ScopedTypeVariables #-}

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
stencilStuff :: (Elt e, IsFloating e) 
             => Acc (Matrix e)          -- [lhs, rhs]
             -> Acc (Scalar (Int, Int)) -- Smax
             -> Acc (Matrix Bool, Matrix Bool) -- (lsh_mask, rhs_mask)
stencilStuff _ _ = lift (fromList (Z :. 0 :. 0) [], fromList (Z :. 0 :. 0) []) -- TODO

calcium_process :: forall e. (Elt e, IsFloating e)
                => Frames e
                -> VarArgIn e
                -> Acc (Vector e, Vector e, Vector e)
calcium_process frames varargin =
  let 
    tform :: Tform e
    tform = undefined -- TODO find_good_reference_frame
  in collect $ calcium_process_seq tform (toSeqInner (use frames))

  -- TODO handle_args
  -- The following assumes the following simplifications from the matlab version
  --   1. Only one connected component
  --   2. use_circle == 0
  --   3. handle_backround == 0
calcium_process_seq :: forall e. (Elt e, IsFloating e)
                    => Tform e
                    -> Seq [Matrix e]
                    -> Seq (Vector e, Vector e, Vector e)
calcium_process_seq tform im =
  let split :: Seq [(Matrix e, Matrix e)]
      split = mapSeq (splitter2 tform) im

      thresh :: Seq [(Scalar e, Scalar e)]
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

      masked :: Seq [(Matrix e, Matrix e)]
      masked =
        zipWithSeq
          (\ mask split -> 
            let (lhs_mask, rhs_mask) = unlift mask
                (lhs, rhs) = unlift split
            in lift (zipWith (*) (map boolToPrec lhs_mask) lhs, zipWith (*) (map boolToPrec rhs_mask) rhs))
          mask
          split

      lhs_nnz :: Seq [Scalar Int]
      lhs_nnz = mapSeq (countPositive . afst) masked

      rhs_nnz :: Seq [Scalar Int]
      rhs_nnz = mapSeq (countPositive . asnd) masked

      yfp :: Seq [Scalar e]
      yfp =
        zipWithSeq
          (\ mask lhs_nnz -> zipWith (/) (foldAll (+) 0 (afst mask)) (map fromIntegral lhs_nnz))
          masked
          lhs_nnz

      cfp :: Seq [Scalar e]
      cfp =
        zipWithSeq
          (\ mask rhs_nnz -> zipWith (/) (foldAll (+) 0.0 (asnd mask)) (map fromIntegral rhs_nnz))
          masked
          rhs_nnz

      ratio :: Seq [Scalar e]
      ratio =
        zipWithSeq
          (zipWith (\ x y -> x / y - 0.6))
          yfp
          cfp

  in lift (fromSeqElems yfp, fromSeqElems cfp, fromSeqElems ratio)

countPositive :: (Elt e, IsFloating e, Shape sh) => Acc (Array sh e) -> Acc (Scalar Int)
countPositive acc = sum (map (\ x -> boolToInt (x >* fromIntegral (constant (0 :: Int)))) acc)

boolToPrec :: (Elt e, IsFloating e) => Exp Bool -> Exp e
boolToPrec = fromIntegral . boolToInt
