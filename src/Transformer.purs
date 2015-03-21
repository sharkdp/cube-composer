module Transformer (
      Transformer()
    , tTail
    , tReplace
    , tStackEqual
    , allSteps
    ) where

import Types
import Data.Array
import Data.Maybe
import Data.Traversable

type Transformer = Wall -> Wall

-- | replace a by b, leave unchanged otherwise
replace :: Cube -> Cube -> Cube -> Cube
replace a b x  = if x == a then b else x

-- | Map a function over the two dimensional array
map2d :: (Cube -> Cube) -> Wall -> Wall
map2d = map <<< map

-- | Opposite of filter
reject :: forall a. (a -> Boolean) -> [a] -> [a]
reject f = filter (f >>> not)

-- | Successively apply all transformers to the initial wall and return
-- | all (intermediate) transformation steps
allSteps :: [Transformer] -> Wall -> [Wall]
allSteps ts wi = wi : scanl (flip ($)) wi ts

-- | Remove emtpy stacks
tClearEmpty :: Transformer
tClearEmpty = reject null

-- | Drop lowest cube
tTail :: Transformer
tTail = tClearEmpty <<< map ((fromMaybe []) <<< tail)

-- | Replace all occurences of a by b
tReplace :: Cube -> Cube -> Transformer
tReplace a b = map2d $ replace a b

-- | concat adjacent lists if they are equal
tStackEqual :: Transformer
tStackEqual [] = []
tStackEqual (s:ss) = (concat (s:split.init)) : tStackEqual split.rest
    where split = span (== s) ss
