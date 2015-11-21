{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Stencils.Blur
(
    blurSepY
  , blurSepX
  , blur
  , boxBlur
) where

import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

-- | Separable Gaussian blur in the Y direction. Taken from Repa Examples.
blurSepY :: Array U DIM2 Float -> IO (Array U DIM2 Float)
blurSepY arr
        = computeP
        $ smap (/ 256)
        $ forStencil2  BoundClamp arr
          [stencil2|    1
                        4
                        6
                        4
                        1 |]
{-# NOINLINE blurSepY #-}

-- | Separable Gaussian blur in the X direction. Taken from Repa Examples.
blurSepX :: Array U DIM2 Float -> IO (Array U DIM2 Float)
blurSepX arr
  = computeP
  $ forStencil2 BoundClamp arr
    [stencil2|  1 4 6 4 1|]
{-# NOINLINE blurSepX #-}

-- | A full gaussian blur. It seems slightly (just barely) faster to run them individually in a pipeline.
blur :: Array U DIM2 Float -> IO (Array U DIM2 Float)
blur input = do blurX <- blurSepX input
                blurSepY blurX

-- | A box blur.
boxBlur :: Array U DIM2 Float -> IO (Array U DIM2 Float)
boxBlur arr
  = computeP $ smap (/25)
  $ forStencil2 BoundClamp arr
    [stencil2|  1 1 1 1 1
                1 1 1 1 1
                1 1 1 1 1
                1 1 1 1 1
                1 1 1 1 1|]
{-# NOINLINE boxBlur #-}
