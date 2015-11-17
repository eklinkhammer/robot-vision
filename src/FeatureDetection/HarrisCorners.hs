{-# LANGUAGE BangPatterns #-}

module FeatureDetection.HarrisCorners
(
    cornerResponse
  , selectCorners
  , cornerResponseZ
) where

import Utility.ImageRep
import Stencils.Blur
import Stencils.Gradient

import Data.Array.Repa.Repr.Unboxed             as U
import qualified Data.Vector.Unboxed as V
import Data.Array.Repa as R
import Data.Int

cornerResponseZ :: Float -> Image Float -> Image Float -> IO (Image Float)
cornerResponseZ !k !dx !dy = computeP $ R.zipWith getM dx dy
  where getM :: Float -> Float -> Float
        getM !x !y = let
                        x2 = square x
                        y2 = square y
                        xy = x * y
                     in (det x2 y2 xy)- k * (square $ trace x2 y2)
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

{-# NOINLINE cornerResponseZ #-}

cornerResponse :: Float -> Image Float -> Image Float -> IO (Image Float)
cornerResponse !k !dX !dY = computeP $ fromFunction (extent dX) f
  where
    f (Z :. y :. x) = det' - k * (square trace')
      where
        dx = dX ! (Z :. y :. x)
        dy = dY ! (Z :. y :. x)
        square :: Float -> Float
        square x = x * x
        {-# INLINE square #-}
        trace :: Float -> Float -> Float
        trace x y = x + y
        {-# INLINE trace #-}
        det :: Float -> Float -> Float -> Float
        det a b c = a * b - (square c)
        {-# INLINE det #-}
        dx2 = square dx
        dy2 = square dy
        dxy = dx * dy
        trace' = trace dx2 dy2
        det' = det dx2 dy2 dxy

selectCorners :: Float -> Image Float -> IO (Array U DIM1 Int)
selectCorners !k !img
  = let
      vec = toUnboxed img
      match ix = vec `V.unsafeIndex` ix > k
      process' ix = ix
   in selectP match process' (size $ extent img)
