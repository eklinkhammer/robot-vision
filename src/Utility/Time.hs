{-# LANGUAGE BangPatterns #-}

module Utility.Time
(
  timeStage
) where

import Data.Array.Repa
import Data.Array.Repa.IO.Timing
import qualified Prelude as P
import Prelude hiding (compare)
import Data.Array.Repa.Repr.Unboxed as U
import Debug.Trace
import Control.Monad

timeStage
          :: (Shape sh, Unbox a)
          => Int
          -> String
          -> IO (Array U sh a)
          -> IO (Array U sh a)
timeStage loops name fn
 = do
    let burn !n
         = do !arr <- fn
              if n <= 1 then return arr
                        else burn (n - 1)
    traceEventIO $ "**** Stage " P.++ name P.++ " begin."
    (arrResult, t) <- time $ do !arrResult' <- burn loops
                                return arrResult'
    traceEventIO $ "**** Stage " P.++ name P.++ " ends."
    when (loops >= 1)
      $ putStr $ name P.++ "\n" P.++ unlines [" " P.++ l | l <- lines $ prettyTime t]
    return arrResult
