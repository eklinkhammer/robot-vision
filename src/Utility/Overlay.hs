{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards,
             MagicHash, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Utility.Overlay
(
  overlayPoints
) where

import Utility.ImageRep

import Data.Word
import Data.Int
import Control.Monad
import GHC.Exts
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed.Mutable    as VM
import qualified Data.Vector.Unboxed            as V
import qualified Prelude                        as P
import Prelude                                  hiding (compare)



overlayPoints :: Word8 -> Image Float -> Array U DIM1 Int -> IO (Image Word8)
overlayPoints val img arrPoints
  = do (sh, vec) <- overlayPointsIO
       return $ sh `seq` vec `seq` fromUnboxed sh vec

  where lenImg    = R.size $ R.extent img
        listPoints = R.toList arrPoints

        overlayPointsIO
          = do
                vImg <- VM.unsafeNew lenImg
                VM.set vImg 0
                burn vImg listPoints val
                vImg' <- V.unsafeFreeze vImg
                return (extent img, vImg')

        burn :: VM.IOVector Word8 -> [Int] -> Word8 -> IO ()
        burn _ [] _ = return ()
        burn !vImg (pt:pts) !color = do
          VM.write vImg pt color
          >> burn vImg pts color
{-# NOINLINE overlayPoints #-}
