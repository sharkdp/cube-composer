module Solver (
      Solution()
    , solve
    ) where

import qualified Data.StrMap as SM
import Data.Array
import Data.Function

import Level
import Transformer
import Types

type Solution = [TransformerId]

-- | Takes from a list of lists until the length increases
takeShortest :: forall a. [[a]] -> [[a]]
takeShortest [] = []
takeShortest (x:xs) = x : takeWhile (\y -> length y == length x) xs

-- | Helper function for the solver
solve' :: Chapter -> Wall -> Wall -> [TransformerId] -> [TransformerId] -> [Solution]
solve' ch initial target chain ts =
        if final == target
        then return chain
        else takeShortest $ sortBy (compare `on` length)
                          $ concatMap (\t -> solve' ch initial target (chain `snoc` t) (filter (/= t) ts)) ts
    where final = transformed (map (getTransformer ch) chain) initial

-- | Brute force solver. Returns a list of all shortest solutions, if any exist
solve :: LevelId -> [Solution]
solve lid = solve' chapter level.initial level.target [] (SM.keys chapter.transformers)
    where level = getLevel lid
          chapter = getChapter lid

