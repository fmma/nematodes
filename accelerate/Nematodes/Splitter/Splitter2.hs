module Nematodes.Splitter.Splitter2 (

  splitter2,

) where

import Nematodes.Types
import Data.Array.Accelerate

-- %
-- % code to split an image down the center and return the two halves
-- % with the RHS registered with the LHS
-- %
splitter2 :: Tform Prec
          -> Acc (Matrix Prec)
          -> Acc (Matrix Prec, Matrix Prec)
splitter2 tform acc = lift (acc, acc) -- TODO
