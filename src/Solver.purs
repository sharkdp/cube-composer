module Solver where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe -- TODO

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
solve' initial target chain ts = if final == target
                                 then (Just chain)
                                 else head $ sortBy (\a b -> length a `compare` length b) $ mapMaybe (\t -> solve' initial target (chain `snoc` t) (remove t ts)) ts
    where steps = allSteps (functions chain) initial
          final = fromJust $ last steps

-- TODO: try to do this with a lazy list?
