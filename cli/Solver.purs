module Solver (
      Solution()
    , solve
    ) where

import Prelude
import Data.Function (on)
import Data.List (List(..), fromFoldable, filter, snoc, concatMap, length, sortBy,
                  mapMaybe, takeWhile, (:))
import Data.StrMap as SM

import Levels (getChapter, getLevel, getTransformer)
import Transformer (transformed)
import Types (LevelId, TransformerId, Wall, Chapter)

type Solution = List TransformerId

-- | Takes from a list of lists until the length increases
takeShortest :: forall a. List (List a) -> List (List a)
takeShortest Nil = Nil
takeShortest (Cons x xs) = x : takeWhile (\y -> length y == length x) xs

-- | Helper function for the solver
solve' :: Chapter -> Wall -> Wall -> List TransformerId -> List TransformerId -> List Solution
solve' ch initial target chain ts =
        if final == target
        then pure chain
        else takeShortest $ sortBy (compare `on` length)
                          $ concatMap (\t -> solve' ch initial target (chain `snoc` t) (filter (_ /= t) ts)) ts
    where final = transformed (mapMaybe (getTransformer ch) chain) initial

-- | Brute force solver. Returns a list of all shortest solutions, if any exist
solve :: LevelId -> List Solution
solve lid = solve' chapter level.initial level.target Nil (fromFoldable $ SM.keys chapter.transformers)
    where level = getLevel lid
          chapter = getChapter lid

