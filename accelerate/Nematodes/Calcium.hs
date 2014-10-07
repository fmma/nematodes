module Nematodes.Calcium where

import Prelude hiding (map, zipWith, length, fromIntegral, (++))

import Data.Array.Accelerate

type Prec = Float

type Frames = Array DIM3 Prec

data VarArgIn = VarArgIn { thresh :: Prec, bThreash :: Prec, circle :: Prec }

type Ratio = Vector Prec
type Yfp = Vector Prec
type Cfp = Vector Prec

type Matrix = Array DIM2

splitter2 :: a -- Transform?
         -> Acc (Matrix Prec)
         -> Acc (Matrix Prec, Matrix Prec)
splitter2 = undefined -- TODO

thresh_estimate :: Acc (Matrix Prec) -> Acc (Scalar Prec, Scalar Prec)
thresh_estimate = undefined -- TODO

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
stencilStuff :: Acc (Matrix Prec)       -- [lhs, rhs]
             -> Acc (Scalar (Int, Int)) -- Smax
             -> Acc (Matrix Bool, Matrix Bool) -- (lsh_mask, rhs_mask)
stencilStuff = undefined -- TODO

calcium_process :: Frames -> VarArgIn -> Acc (Ratio, Yfp, Cfp)
calcium_process frames varargin =
  let tform = undefined -- TODO find_good_reference_frame
  in collect $ calcium_process_seq tform (toSeqInner (use frames))

  -- TODO handle_args
  -- The following assumes the following simplifications from the matlab version
  --   * Only one connected component
  --   * use_circle == 0
  --   * handle_backround == 0
calcium_process_seq :: Acc (Matrix Prec) -> Seq [Matrix Prec] -> Seq (Ratio, Yfp, Cfp)
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

      cCmax :: Seq [Vector (Int, Int)]  -- Simplification 1
      cCmax = mapSeq bwconnectcomp bWmax

      smax :: Seq [Scalar (Int, Int)]   -- Simplification 1
      smax = mapSeq centroid cCmax

      mask :: Seq [(Matrix Bool, Matrix Bool)]  -- Simplification 2
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

countPositive :: Acc (Array sh Prec) -> Acc (Scalar Int)
countPositive = undefined

boolToFloat :: Exp Bool -> Exp Float
boolToFloat = fromIntegral . boolToInt
