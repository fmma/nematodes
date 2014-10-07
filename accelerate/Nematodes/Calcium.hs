module Nematodes.Calcium where

import Prelude hiding (map, zipWith)

import Data.Array.Accelerate

type Prec = Float

type Frames = Array DIM3 Prec

data VarArgIn = VarArgIn { tresh :: Prec, bThreash :: Prec, circle :: Prec }

type Ratio = Vector Prec
type Yfp = Vector Prec
type Cfp = Vector Prec

type Matrix = Array Dim2

splitter2 :: a -- Transform?
         -> Acc (Matrix Prec)
         -> Acc (Katrix Prec, Matrix Prec)
splitter2 = undefined -- TODO

tresh_estimate :: Acc (Matrix Prec) -> Acc (Scalar Prec, Scalar Prec)
tresh_estimate = undefined -- TODO

bwconnectcomp :: Acc (Matrix Bool)       -- Black and white image.
              -> Acc (Vector (Int, Int)) -- Vector of points in cc.
bwconnectcomp = undefined -- TODO

centroid :: Acc (Vector (Int, Int))
         -> Acc (Scalar (Int, Int))
centroid = undefined -- TODO

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
stencilStuff :: Acc (Matrix Bool)       -- BWmax
             -> Acc (Vector (Int, Int)) -- CCmax
             -> Acc (Matrix Bool, Matrix Bool) -- (lsh_mask, rhs_mask)
stencilStuff = undefined -- TODO

calcium_process :: Frames -> VarArgIn -> (Ratio, Yfp, Cfp)
calcium_process frames varargin =
  -- TODO handle_args
  -- The following assumes 
  --   use_circle == 0
  --   handle_backround == 0
  let
    tform = undefined -- TODO find_good_reference_frame
    (yfp, cfp, ratio) = run $ collect $
      let im :: Seq [Matrix Prec]
          im = toSeqInner (use frames)

          split :: Seq [(Matrix Prec, Matrix Prec)]
          split = mapSeq (splitter2 tform) im

          tresh :: Seq [(Scalar Prec, Scalar Prec)]
          tresh = mapSeq (thresh_estimate . afst) split

          bWmax :: Seq [Matrix Bool]
          bWmax =
            zipWithSeq
              (\ x y -> zipWith (>) (afst x) (afst y))
              split
              tresh

          cCmax :: Seq [Vector (Int, Int)]
          cCmax = mapSeq bwconnectcomp bWmax -- Note: This is a simplification from matlab.

          smax :: Seq [Scalar (Int, Int)]
          smax = mapSeq centroid cCmax

          mask :: Seq [(Matrix Bool, Matrix Bool)]
          mask =
            zipWithSeq
              (\ x y -> stencilStuff (afst x) y)
              split
              smax

          masked :: Seq [(Matrix Prec, Matrix Prec)]
          masked =
            zipWithSeq
              (\ (lhs_mask, rhs_mask) (lhs, rhs) -> (lhs_mask (map (*)) lhs, rhs_mask (map (*)) rhs) x y)
              mask
              split

          lhs_nnz :: Seq [Scalar Int]
          lhs_nnz = mapSeq (length . find (>0) . afst) masked

          rhs_nnz :: Seq [Scalar Int]
          rhs_nnz = mapSeq (length . find (>0) . asnd) masked

          yfp :: Seq [Scalar Prec]
          yfp =
            zipWithSeq
              (\ mask lhs_nnz -> zipWith (/) (fold (+) 0 (afst mask)) lhs_nnz)
              masked
              lhs_nnz

          cfp :: Seq [Scalar Prec]
          cfp =
            zipWithSeq
              (\ mask rhs_nnz -> zipWith (/) (fold (+) 0 (asnd mask)) rhs_nnz)
              masked
              rhs_nnz

          ratio :: Seq [Scalar Prec]
          ratio =
            zipWithSeq
              (\ x y -> zipWith (/) x y - 0.6)
              yfp
              cfp

      in lift (fromSeq yfp, fromSeq cfp, fromSeq ratio)
  in (ratio, yfp, cfp)
