module Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List, fromList)
import Data.Traversable (for)

import Types
import Solver (solve)
import Levels

ttyColor :: Cube -> Int
ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Cyan = 4

showList :: forall a. (Show a) => List a -> String
showList xs = show (fromList xs :: Array a)

showList2 :: forall a. (Show a) => List (List a) -> String
showList2 xss = show ((fromList <<< map fromList) xss :: Array (Array a))

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
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
    return unit
