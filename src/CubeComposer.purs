module CubeComposer where

import Data.Array
import Data.Foldable
import Debug.Trace

import Types
import Transformer

ts :: [Transformer]
ts = [
       tStackEqual
     , tReplace
     , tTail
     , tClearEmpty
     ]

initial :: Wall
initial = [[Red], [Red], [Red, Blue], [Red, Blue], [Red, Blue], [Blue]]

main = do
    trace $ "Initial: " ++ (show initial)
    trace "Steps:"
    sequence_ $ map (trace <<< show) $ allSteps ts initial
    trace "---"
    trace ""
