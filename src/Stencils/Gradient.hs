{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards,
             MagicHash, ScopedTypeVariables, TypeFamilies #-}
module Stencils.Gradient
(
    gradientX
  , gradientY
  , gradientMagOrient
) where

import Data.Word
import Control.Monad

import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Specialised.Dim2
import GHC.Exts

import Prelude hiding (zipWith)
import Utility.ImageRep

orientUndef     = 0     :: Word8
orientPosDiag   = 64    :: Word8
orientVert      = 128   :: Word8
orientNegDiag   = 192   :: Word8
orientHoriz     = 255   :: Word8

data Edge       = None | Weak | Strong
edge None       = 0     :: Word8
edge Weak       = 128   :: Word8
edge Strong     = 255   :: Word8

-- | Compute gradient in the X direction. Repa example
gradientX :: Array U DIM2 Float -> IO (Array U DIM2 Float)
gradientX img
  = computeP
  $ forStencil2 BoundClamp img
    [stencil2|    -1  0  1
                  -2  0  2
                  -1  0  1 |]
{-# NOINLINE gradientX #-}

-- | Compute gradient in the Y direction. Repa example
gradientY :: Array U DIM2 Float -> IO (Array U DIM2 Float)
gradientY img
  = computeP
  $ forStencil2 BoundClamp img
    [stencil2|     1  2  1
                   0  0  0
                  -1 -2 -1 |]
{-# NOINLINE gradientY #-}

gradientMagOrient :: Float -> Array U DIM2 Float -> Array U DIM2 Float -> IO (Array U DIM2 (Float, Word8))
gradientMagOrient !threshLow dX dY
        = computeP
        $ zipWith magOrient dX dY

 where  magOrient :: Float -> Float -> (Float, Word8)
        magOrient !x !y
                = (magnitude x y, orientation x y)
        {-# INLINE magOrient #-}

        magnitude :: Float -> Float -> Float
        magnitude !x !y
                = sqrt (x * x + y * y)
        {-# INLINE magnitude #-}

        {-# INLINE orientation #-}
        orientation :: Float -> Float -> Word8
        orientation !x !y

         -- Don't bother computing orientation if vector is below threshold.
         | x >= negate threshLow, x < threshLow
         , y >= negate threshLow, y < threshLow
         = orientUndef

         | otherwise
         = let  -- Determine the angle of the vector and rotate it around a bit
                -- to make the segments easier to classify.
                !d      = atan2 y x
                !dRot   = (d - (pi/8)) * (4/pi)

                -- Normalise angle to beween 0..8
                !dNorm  = if dRot < 0 then dRot + 8 else dRot

                -- Doing explicit tests seems to be faster than using the FP floor function.
           in fromIntegral
               $ I# (if dNorm >= 4
                     then if dNorm >= 6
                          then if dNorm >= 7
                                then 255#               -- 7
                                else 192#               -- 6

                          else if dNorm >= 5
                                then 128#               -- 5
                                else 64#                -- 4

                     else if dNorm >= 2
                        then if dNorm >= 3
                                then 255#               -- 3
                                else 192#               -- 2

                        else if dNorm >= 1
                                then 128#               -- 1
                                else 64#)               -- 0
{-# NOINLINE gradientMagOrient #-}
