module Main (main) where

import Prelude
import Control.Monad.Eff.Console
import Data.List
import Data.Foldable
import Data.Traversable

import Types
import Transformer
import Solver
import Levels

ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Cyan = 4

showList :: forall a. (Show a) => List a -> String
showList xs = show (fromList xs :: Array a)

showList2 :: forall a. (Show a) => List (List a) -> String
showList2 xss = show ((fromList <<< map fromList) xss :: Array (Array a))

main =
    for allLevelIds $ \lid -> do
        let chapter = getChapter lid
            level = getLevel lid
            solutions = solve lid

        log $ levelTitle lid level
        log $ "  Initial: " ++ showList2 (map (map ttyColor) level.initial)
        log $ "  Target:  " ++ showList2 (map (map ttyColor) level.target)
        log $ "  Solutions: "
        for solutions $ \sol ->
            log $ "    " ++ showList sol
        log ""
