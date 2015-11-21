{-# LANGUAGE BangPatterns #-}

module FeatureDetection.HarrisCorners
(
    cornerResponse
  , selectCorners
) where

import Utility.ImageRep
import Utility.Zippers as Z
import Stencils.Blur
import Stencils.Gradient

import Data.Array.Repa.Repr.Unboxed             as U
import qualified Data.Vector.Unboxed as V
import Data.Array.Repa as R
import Data.Int

cornerResponse :: Float -> Image Float -> Image Float -> Image Float -> IO (Image Float)
cornerResponse !k !dx !dy !dxy = computeP $ Z.zipWith3 getM dx dy dxy
  where getM :: Float -> Float -> Float -> Float
        getM !x !y !xy = let
                        x2 = square x
                        y2 = square y
                     in (det x2 y2 xy) - (k * (square $ trace x2 y2))
        {-# INLINE getM #-}

        det :: Float -> Float -> Float -> Float
        det !x !y !xy = x * y - square xy
        {-# INLINE det #-}

        square :: Float -> Float
        square !x = x * x
        {-# INLINE square #-}

        trace :: Float -> Float -> Float
        trace !x !y = x + y
        {-# INLINE trace #-}

{-# NOINLINE cornerResponse #-}

selectCorners :: Float -> Image Float -> IO (Array U DIM1 Int)
selectCorners !k !img
  = let
      vec = toUnboxed img
      match ix = vec `V.unsafeIndex` ix > k
      process' ix = ix
   in selectP match process' (size $ extent img)
