module Nematodes.Calcium where

import Data.Array.Accelerate

type Prec = Float

type Frames = Arra DIM3 Prec

data VarArgIn = VarArgIn { Tresh :: Prec, BThreash :: Prec, Circle :: Prec }

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
    ((((), (yfp, _)), (cfp, _)), (ratio, _)) = run $ runSequence
      -- im(i) :: Matrix Prec = 
        $ useLazy frames -- Todo slice

      -- split :: (Matrix Prec, Matrix Prec)
        $ mapSeq (splitter2 tform) ZeroIdx{-im(i)-}

      -- tresh = (tresh :: Scalar Prec, btresh :: Scalar Prec)
        $ mapSeq (thresh_estimate . afst) ZeroIdx{-split-}

      -- BWmax :: Matrix Bool =
        $ zipWithSeq 
            (\ x y -> zipWith (>) (afst x) (afst y)) 
            (SuccIdx ZeroIdx){-split-} 
            ZeroIdx{-tresh-}

      -- CCmax :: Vector (Int, Int) =      Note: This is a simplification from matlab.
        $ mapSeq bwconnectcomp ZeroIdx{-BWmax-}

      -- Smax :: (Int, Int) =
        $ mapSeq centroid ZeroIdx{-CCmax-}

      -- mask :: (Matrix Bool, Matrix Bool) = 
        $ zipWithSeq
            (\ x y -> stencilStuff (afst x) y) 
            (SuccIdx (SuccIdx (SuccIdx (SuccIdx ZeroIdx)))){-split-} 
            ZeroIdx{-Smax-}

      -- masked :: (Matrix Prec, Matrix Prec) =
        $ zipWithSeq 
            (\ (lhs_mask, rhs_mask) (lhs, rhs) -> (lhs_mask (map (*)) lhs, rhs_mask (map (*)) rhs)) x y) 
            ZeroIdx{-mask-}
            (SuccIdx (SuccIdx (SuccIdx (SuccIdx (SuccIdx ZeroIdx))))){-split-}

      -- lhs_nnz :: Int =
        $ mapSeq (length . find (>0) . afst) ZeroIdx{-masked-}

      -- rhs_nnz :: Int =
        $ mapSeq (length . find (>0) . asnd) (SuccIdx ZeroIdx){-masked-}

      -- yfp(i) :: Prec =
        $ zipWithSeq 
            (\ mask lhs_nnz -> zipWith (/) (fold (+) 0 (afst mask)) lhs_nnz) 
            (SuccIdx (SuccIdx ZeroIdx)){-masked-}
            (SuccIdx ZeroIdx){-lhs_nnz-}
        $ fromSeq ZeroIdx

      -- cfp(i) :: Prec =
        $ zipWithSeq
            (\ mask rhs_nnz -> zipWith (/) (fold (+) 0 (asnd mask)) rhs_nnz)
            (Succ (SuccIdx (SuccIdx ZeroIdx))){-masked-}
            (SuccIdx ZeroIdx){-rhs_nnz-}
        $ fromSeq ZeroIdx

      -- ratio :: Prec =
        $ zipWithSeq
            (\ x y -> zipWith (/) x y - 0.6)
            (SuccIdx ZeroIdx){-yfp(i)-}
            ZeroIdx{-cfp(i)-}
        $ fromSeq ZeroIdx

        $ emptySeq
  in (ratio, yfp, cfp)
