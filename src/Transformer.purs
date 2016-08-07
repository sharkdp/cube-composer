module Transformer where

import Prelude
import Data.List (List, concatMap, singleton, snoc, null, (:), filter)
import Data.Foldable (foldl)
import Data.Traversable (scanl)
import Types (Transformer, Cube, Wall)

-- | Map a function over the two dimensional array (= wall)
map2d :: (Cube -> Cube) -> Wall -> Wall
map2d = map <<< map

-- | Opposite of filter, reject all values which satisfy the pattern
reject :: forall a. (a -> Boolean) -> List a -> List a
reject f = filter (not <<< f)

-- | Successively apply all transformers to the initial wall and return
-- | all (intermediate) transformation steps
allSteps :: List Transformer -> Wall -> List Wall
allSteps ts initial = initial : scanl (#) initial ts

-- | Return the final step of the transformation chain
transformed :: List Transformer -> Wall -> Wall
transformed ts initial = foldl (#) initial ts

-- | Remove emtpy stacks
clearEmpty :: Transformer
clearEmpty = reject null

-- | Reject all cubes of a certain color
mapReject :: Cube -> Transformer
mapReject c = map (reject (_ == c)) >>> clearEmpty

-- | Stack a single cube on top of each column
mapStack :: Cube -> Transformer
mapStack c = map (_ `snoc` c)

-- | Replace all occurences of a certain cube with another
replaceSingle :: Cube -> Cube -> Transformer
replaceSingle a b = map2d replace
    where replace x = if x == a then b else x

-- | Replace all occurences of a certain cube with a list of new cubes
replaceMultiple :: Cube -> List Cube -> Transformer
replaceMultiple a bs = map (concatMap replace)
    where replace x = if x == a then bs else singleton x
