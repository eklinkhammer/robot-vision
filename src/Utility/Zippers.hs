module Utility.Zippers
(
  zipWith3
) where

import Data.Array.Repa
import Prelude hiding (map, zipWith3)
import Foreign.Storable

zipWith3 :: (Shape sh, Source r1 a, Source r2 b, Source r3 c)
        => (a -> b -> c -> d)
        -> Array r1 sh a -> Array r2 sh b -> Array r3 sh c
        -> Array D sh d
zipWith3 f arr1 arr2 arr3
 = let  get ix  = f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix) (arr3 `unsafeIndex` ix)
        {-# INLINE get #-}

   in   fromFunction
                (intersectDim (intersectDim (extent arr1) (extent arr2)) (extent arr3))
                get
{-# INLINE [2] zipWith3 #-}
