module Solver (
      Solution()
    , solve
    ) where

import Data.Array
import Data.Function
import Data.Maybe

import Transformer
import Types

type Solution = [TransformerRecord]

-- | Remove the given transformer from the list
remove :: TransformerRecord -> [TransformerRecord] -> [TransformerRecord]
remove x = filter (\y -> y.id /= x.id)

-- | Helper function for the solver
solve' :: Wall -> Wall -> [TransformerRecord] -> [TransformerRecord] -> Maybe Solution
solve' initial target chain ts =
        if final == target
        then return chain
        else head $ sortBy (compare `on` length)
                  $ mapMaybe (\t -> solve' initial target (chain `snoc` t) (remove t ts)) ts
    where final = transformed (map _.function chain) initial

-- | Brute force solver
solve :: [TransformerRecord] -> Wall -> Wall -> Maybe Solution
solve transformers initial target = solve' initial target [] transformers
