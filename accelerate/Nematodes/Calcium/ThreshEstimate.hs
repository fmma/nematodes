module Nematodes.Calcium.ThreshEstimate (

  thresh_estimate,

) where

import Nematodes.Types
import Data.Array.Accelerate

-- % Gives a threshold and background threshold for a single frame. 
-- % kmeans input of 3 clusters, 10 iterations is typical. The user can input
-- % hard-coded fractions of the top and bottom cluster intensities to be
-- % returned in the t, bt variables.
thresh_estimate :: (Elt e, IsFloating e) => Acc (Matrix e) -> Acc (Scalar e, Scalar e)
thresh_estimate _ = lift (unit (constant 0.0), unit (constant 0.0)) -- TODO
