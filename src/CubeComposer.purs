module CubeComposer where

import Data.Array
import Data.Foldable
import Data.Maybe
import Debug.Trace

data Cube = Red | Blue

instance showCube :: Show Cube where
    show Red = "Red"
    show Blue = "Blue"

instance eqCube :: Eq Cube where
    (==) Red Red = true
    (==) Blue Blue = true
    (==) Red Blue = false
    (==) Blue Red = false
    (/=) x y = not (x == y)

type Stack = [Cube]
type Wall = [Stack]

type Transformer = Wall -> Wall


replace :: Cube -> Cube -> Cube -> Cube
replace a b x  = if x == a then b else x

map2d :: (Cube -> Cube) -> Wall -> Wall
map2d = map >>> map

-- | Adaption of Haskells scanl
scanl :: forall a b . (b -> a -> b) -> b -> [a] -> [b]
scanl f bi as = bi : (case as of
                        [] -> []
                        a:as' -> scanl f (f bi a) as')

reject :: forall a. (a -> Boolean) -> [a] -> [a]
reject f = filter (not <<< f)

-- | Successively apply all transformers to the initial wall and return
-- | all intermediate transformation steps
allSteps :: [Transformer] -> Wall -> [Wall]
allSteps ts wi = scanl (flip ($)) wi ts

-------- TRANSFORMERS ----------

tTail :: Transformer
tTail = map ((fromMaybe []) <<< tail)

tReplace :: Transformer
tReplace = map2d (replace Red Blue)

tStackEqual :: Transformer
tStackEqual [] = []
tStackEqual (s:ss) = (concat (s:split.init)) : tStackEqual split.rest
    where split = span (== s) ss

-- | Remove emtpy stacks
tClearEmpty :: Transformer
tClearEmpty = reject null

-------- TESTING -------

ts :: [Transformer]
ts = [
       tStackEqual
     , tReplace
     , tTail
     , tClearEmpty
     ]

initial :: Wall
initial = [[Red], [Red], [Red, Blue], [Red, Blue], [Red, Blue], [Blue]]

main = do
    trace $ "Initial: " ++ (show initial)
    trace "Steps:"
    sequence_ $ map (trace <<< show) $ allSteps ts initial
    trace "---"
    trace ""
