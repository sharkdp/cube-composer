module Main (main) where

import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Debug.Trace

import Types
import Transformer
import Solver

initial :: Wall
initial = [[Red, Red], [Red, Yellow], [Blue, Yellow], [Blue, Blue]]

ids = ["stackEqual", "pushY", "replaceYbyBY"]

chain :: [Transformer]
chain = catMaybes $ map getTransformerById ids

{-- final = fromJust $ last $ allSteps chain initial --}
{-- final = [[Red, Red], [Red], [Red]] --}
{-- final = [[Red, Red], [Red, Red]] --}
{-- final = [[Red, Red, Red], [Red]] --}
{-- final = [[Red, Red, Red], [Blue, Red]] --}
final = [[Red], [Red, Red], [Red]]

ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Blue = 4

main = do
    trace $ "Initial: " ++ show (map (map ttyColor) initial)
    trace $ "after applying " ++ show (length chain) ++ " transformers: " ++ show ids
    trace $ "Final: " ++ show (map (map ttyColor) final)
    let solution = solve initial final
    let solIds = map (\x -> x.id) <$> solution
    trace $ "Solution: " ++ show solIds
    {-- let ll = defer (\x -> 11) :: Lazy Number --}
    {-- print $ force ll --}
