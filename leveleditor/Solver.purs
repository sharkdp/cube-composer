module Solver (
      Solution()
    , solve
    ) where

import qualified Data.StrMap as SM
import Data.Array
import Data.Function
import Data.Maybe

import Level
import Transformer
import Types

type Solution = [TransformerId]

-- | Helper function for the solver
solve' :: Chapter -> Wall -> Wall -> [TransformerId] -> [TransformerId] -> Maybe Solution
solve' ch initial target chain ts =
        if final == target
        then return chain
        else head $ sortBy (compare `on` length)
                  $ mapMaybe (\t -> solve' ch initial target (chain `snoc` t) (filter (/= t) ts)) ts
    where final = transformed (map (getTransformer ch) chain) initial

-- | Brute force solver
solve :: LevelId -> Maybe Solution
solve lid = solve' chapter level.initial level.target [] (SM.keys chapter.transformers)
    where level = getLevel lid
          chapter = getChapter lid

