module Transformer where

import Types
import Data.Array
import Data.Maybe
import Data.Traversable
import Data.Foldable

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

-- | Return the final step of the transformation chain
transformed :: [Transformer] -> Wall -> Wall
transformed ts wi = foldl (flip ($)) wi ts

-- | Remove emtpy stacks
tClearEmpty :: Transformer
tClearEmpty = reject null

-- | Drop lowest cube
tTail :: Transformer
tTail =  map (tail >>> fromMaybe []) >>> tClearEmpty

-- | Replace all occurences of a by b
tReplace :: Cube -> Cube -> Transformer
tReplace a b = map2d replace
    where replace x = if x == a then b else x

-- | Replace all occurences of a by bs and flattens
tReplaceMultiple :: Cube -> [Cube] -> Transformer
tReplaceMultiple a bs = map (concatMap replace)
   where replace x = if x == a then bs else [x]

-- | concat adjacent lists if they are equal
tStackEqual :: Transformer
tStackEqual [] = []
tStackEqual (s:ss) = concat (s:split.init) : tStackEqual split.rest
    where split = span (== s) ss

-- | Flatten the whole wall to single cubes
tFlatten :: Transformer
tFlatten = concat >>> map singleton

-- | Find a specific transformer by its id
getTransformerById :: Chapter -> TransformerId -> Maybe Transformer
getTransformerById chapter id = _.function <$> (head $ filter (\t -> t.id == id) chapter.transformers)
