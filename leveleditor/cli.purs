module Main (main) where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
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

levelid :: LevelId
levelid = "1_4"

main = do
    let chapter = getChapter levelid
        level = getLevel levelid
        solution = solve levelid

    trace $ "Initial: " ++ show (map (map ttyColor) level.initial)
    trace $ "Target:  " ++ show (map (map ttyColor) level.target)
    trace $ "Solution: " ++ show solution
