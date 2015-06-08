module Transformer where

import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Types
import qualified Data.StrMap as SM

-- | Map a function over the two dimensional array (= wall)
map2d :: (Cube -> Cube) -> Wall -> Wall
map2d = map <<< map

-- | Opposite of filter, reject all values which satisfy the pattern
reject :: forall a. (a -> Boolean) -> [a] -> [a]
reject f = filter (not <<< f)

-- | Successively apply all transformers to the initial wall and return
-- | all (intermediate) transformation steps
allSteps :: [Transformer] -> Wall -> [Wall]
allSteps ts initial = initial : scanl (#) initial ts

-- | Return the final step of the transformation chain
transformed :: [Transformer] -> Wall -> Wall
transformed ts initial = foldl (#) initial ts

-- | Remove emtpy stacks
clearEmpty :: Transformer
clearEmpty = reject null

-- | Replace all occurences of a certain cube with another
replaceSingle :: Cube -> Cube -> Transformer
replaceSingle a b = map2d replace
    where replace x = if x == a then b else x

-- | Replace all occurences of a certain cube with a list of new cubes
replaceMultiple :: Cube -> [Cube] -> Transformer
replaceMultiple a bs = map (concatMap replace)
   where replace x = if x == a then bs else [x]
