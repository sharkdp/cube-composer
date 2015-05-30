module Main (main) where

import Data.Array
import Data.Foldable
import Data.Traversable
import Debug.Trace

import Types
import Transformer
import Solver
import Level
import Levels.Chapter1

ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Blue = 4

main =
    for allLevelIds $ \lid -> do
        let chapter = getChapter lid
            level = getLevel lid
            solutions = solve lid

        trace $ levelTitle lid level
        trace $ "  Initial: " ++ show (map (map ttyColor) level.initial)
        trace $ "  Target:  " ++ show (map (map ttyColor) level.target)
        trace $ "  Solutions: "
        for solutions $ \sol ->
            trace $ "    " ++ show sol
        trace ""
