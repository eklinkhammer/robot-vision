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

-- Display all elements of list with their index
displayList :: (Ord a, Unbox a) => Image a -> a -> [(a,Int)]
displayList img k = filter ((<) k . fst) $ Prelude.zip (toList img) [1..]

displayListShape :: (Ord a, Unbox a) => Image a -> [(a,Int)] -> [(a,DIM2)]
displayListShape img vals = Prelude.map (\(x,ix) -> (x, fromIndex (extent img) ix)) vals

displayMagList :: (Ord a, Unbox a, Num a) => Image a -> a -> [(a,Int)]
displayMagList img k = filter ((<) k . abs . fst) $ Prelude.zip (toList img) [1..]

toIndexList :: (Ord a, Unbox a, Num a) => Image a -> [(a,Int)]
toIndexList img = Prelude.zip (toList img) [1..]

toShapeList :: (Ord a, Unbox a, Num a) => Image a -> [(a,DIM2)]
toShapeList img = Prelude.map (\(x,ix) -> (x, fromIndex (extent img) ix)) $ toIndexList img

toOnlyPos :: (Ord a, Unbox a, Num a) => [(a,b)] -> [(a,b)]
toOnlyPos = Prelude.map (\(x,y) -> (max x 0, y))

printShapeList :: (Show a) => [(a,DIM2)] -> IO ()
printShapeList [x] = putStrLn $ show $ fst x
printShapeList list@((x,(Z :. xj :. _)):(_,(Z :. yj :. _)):_) = do
  if (xj == yj) then do
                      putStr $ (show x) Prelude.++ " "
                      printShapeList $ tail list
                else do
                      putStrLn $ show x
                      printShapeList $ tail list


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
      arrGradXY <- timeStage loops "gradXY" $ gradientY arrGradX
      cornerImage <- timeStage loops "Harris" $ cornerResponse 0.15 arrGradX arrGradY arrGradXY
      corners <- timeStage loops "Corner Selection" $ selectCorners 0 cornerImage
      --putStrLn $ show $ displayList arrGradX 0
      --putStrLn $ show $ displayList cornerImage 0
      --putStrLn $ show $ toShapeList arrGradX
      --printShapeList $ toShapeList arrGradY
      --printShapeList $ toOnlyPos $ toShapeList cornerImage
      overlay <- timeStage loops "With corners" $ overlayPoints 128 arrGrey corners

      --putStrLn $ show $ toList corners
     --arrMagOrient <- timeStage loops "magOrident" $ gradientMagOrient 50 arrGradX arrGradY
      --arrMag <- timeStage loops "just mag" $ gradMagToImage arrMagOrient
      return overlay

gradMagToImage :: Image (Float, Word8) -> IO (Image Float)
gradMagToImage arr = computeP $ R.map fst arr
{-# NOINLINE gradMagToImage #-}

valuePoint :: (Unbox a) => Array U DIM1 Int -> Image a -> [(Int,a)]
valuePoint pts img = valuePoints (R.toList pts) img
  where valuePoints [] _ = []
        valuePoints (pt:pts') img' = let val = img' ! (fromIndex (R.extent img) pt)
                                      in (pt, val) : (valuePoints pts' img)
