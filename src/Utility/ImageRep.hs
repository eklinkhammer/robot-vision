module Utility.ImageRep
(
    toGreyScale
  , toOutput
  , Image (..)
) where

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as U
import Data.Array.Repa.Algorithms.Pixel

import Data.Word
type Image a = Array U DIM2 a

toGreyScale :: Image (Word8, Word8, Word8) -> IO (Image Float)
toGreyScale arr = computeP $ R.map (*255) $ R.map floatLuminanceOfRGB8 arr
{-# NOINLINE toGreyScale #-}

toOutput :: Image Float -> IO (Image Word8)
toOutput arr = computeP $ R.map toWord arr
{-# NOINLINE toOutput #-}

toWord :: Float -> Word8
toWord x = fromIntegral (round x :: Integer)
{-# INLINE toWord #-}
