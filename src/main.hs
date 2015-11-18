{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards, MagicHash, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import Control.Monad
import System.Environment
import Data.Word
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as U
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing

import Prelude hiding (compare)

import Utility.Time
import Utility.ImageRep
import Utility.Overlay
import Stencils.Blur
import Stencils.Gradient
import FeatureDetection.HarrisCorners

main :: IO ()
main = do
  putStrLn "Hello World!"
  args <- getArgs
  case args of
    [fileIn, fileOut] -> run fileIn fileOut
    _ -> putStrLn "Insufficient Arguments"

run fileIn fileOut
 = do arrInput <- liftM (either (error . show) id) $ readImageFromBMP fileIn
      (arrResult, tTotal) <- time $ process 1 arrInput
      putStrLn $ prettyTime tTotal
      writeImageToBMP fileOut (U.zip3 arrResult arrResult arrResult)

process loops arrInput
 = do arrGrey     <- timeStage loops "toGreyScale" $ toGreyScale arrInput
      arrBlur    <- timeStage loops "blur" $ blur arrGrey
      arrGradX   <- timeStage loops "gradX" $ gradientX arrBlur
      arrGradY  <- timeStage loops "gradY" $ gradientY arrBlur

      cornerImage <- timeStage loops "Harris" $ cornerResponse 0.04 arrGradX arrGradY
      cornerImageZ <- timeStage loops "HarrisZ" $ cornerResponseZ 0.04 arrGradX arrGradY
      corners <- timeStage loops "Corner Selection" $ selectCorners 0 cornerImage
      cornersZ <- timeStage loops "Corner Selection Z" $ selectCorners 0 cornerImageZ
      --putStrLn "Image values"
      --putStrLn $ show $ take 1000 $ toList arrGrey
      --putStrLn "Blurred"
      --putStrLn $ show $ take 1000 $ toList arrBlur
      --putStrLn "GradX"
      --putStrLn $ show $ take 1000 $ toList arrGradX
      --putStrLn "GradY"
      --putStrLn $ show $ take 1000 $ toList arrGradY
      --putStrLn "Corner"
      --putStrLn $ show $ take 1000 $ toList cornerImage
      --putStrLn "CornerZ"
      --putStrLn $ show $ take 1000 $ toList cornerImageZ
      putStrLn $ show $ valuePoint cornersZ cornerImageZ
      overlay <- timeStage loops "With corners" $ overlayPoints 128 arrGrey cornersZ


      --putStrLn $ show $ toList cornersZ
      --arrMagOrient <- timeStage loops "magOrident" $ gradientMagOrient 50 arrGradX arrGradY
      --arrMag <- timeStage loops "just mag" $ gradMagToImage arrMagOrient
      --arrOutput   <- timeStage loops "toOutput" $ toOutput overlay
      return overlay

gradMagToImage :: Image (Float, Word8) -> IO (Image Float)
gradMagToImage arr = computeP $ R.map fst arr
{-# NOINLINE gradMagToImage #-}

valuePoint :: (Unbox a) => Array U DIM1 Int -> Image a -> [(Int,a)]
valuePoint pts img = valuePoints (R.toList pts) img
  where valuePoints [] _ = []
        valuePoints (pt:pts') img' = let val = img' ! (fromIndex (R.extent img) pt)
                                      in (pt, val) : (valuePoints pts' img)
