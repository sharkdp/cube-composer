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

functions = map (\x -> x.function)

remove :: TransformerRecord -> [TransformerRecord] -> [TransformerRecord]
remove x = filter (\y -> y.id /= x.id)

-- | Brute force solver
solve :: Wall -> Wall -> Maybe Solution
solve initial target = solve' initial target [] transformers

solve' :: Wall -> Wall -> [TransformerRecord] -> [TransformerRecord] -> Maybe Solution
solve' initial target chain ts =
        if final == target
        then return chain
        else head $ sortBy (compare `on` length)
                  $ mapMaybe (\t -> solve' initial target (chain `snoc` t) (remove t ts)) ts
    where final = transformed (functions chain) initial
