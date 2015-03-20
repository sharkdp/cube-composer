module Transformer where

import Types
import Data.Array
import Data.Maybe

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
