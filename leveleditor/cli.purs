module Main (main) where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Debug.Trace

import Types
import Transformer
import Solver
import Levels.Chapter1

chapter :: Chapter
chapter = chapter1

initial :: Wall
initial = [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]]

ids = ["stackEqual", "rejectO", "replaceBbyBBB"]

chain :: [Transformer]
chain = catMaybes $ map (getTransformerById chapter) ids

final = fromJust $ last $ allSteps chain initial
{-- final = [[Red, Red], [Red], [Red]] --}
{-- final = [[Red, Red], [Red, Red]] --}
{-- final = [[Red, Red, Red], [Red]] --}
{-- final = [[Red, Red, Red], [Blue, Red]] --}
{-- final = [[Red], [Red, Red], [Red]] --}

ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Blue = 4

main = do
    trace $ "Initial: " ++ show (map (map ttyColor) initial)
    trace $ "after applying " ++ show (length chain) ++ " transformers: " ++ show ids
    trace $ "Final: " ++ show (map (map ttyColor) final)
    let solution = solve chapter.transformers initial final
    let solIds = map (\x -> x.id) <$> solution
    trace $ "Solution: " ++ show solIds
    {-- let ll = defer (\x -> 11) :: Lazy Number --}
    {-- print $ force ll --}
