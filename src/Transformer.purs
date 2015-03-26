module Transformer where

import Types
import Data.Array
import Data.Maybe
import Data.Traversable

type Transformer = Wall -> Wall

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

tFlatten :: Transformer
tFlatten = concat >>> map singleton


type TransformerRecord = {
    id :: String,
    name :: String,
    function :: Transformer
}

transformers :: [TransformerRecord]
transformers = [
    {
        id: "stackEqual",
        name: "stackEqual",
        function: tStackEqual
    {-- }, { --}
    {--     id: "mapClone", --}
    {--     name: "map({X} ↦ {X}{X})", --}
    {--     function: map $ concatMap (\x -> [x, x]) --}
    {-- }, { --}
    {--     id: "flatten", --}
    {--     name: "flatten", --}
    {--     function: tFlatten --}
    {-- }, { --}
    {--     id: "replaceYbyB", --}
    {--     name: "map({Yellow} ↦ {Brown})", --}
    {--     function: tReplace Yellow Brown --}
    {-- }, { --}
    {--     id: "replaceYbyBY", --}
    {--     name: "map({Yellow} ↦ {Brown}{Yellow})", --}
    {--     function: tReplaceMultiple Yellow [Brown, Yellow] --}
    {-- }, { --}
    {--     id: "replaceBbyBBB", --}
    {--     name: "map({Brown} ↦ {Brown}{Brown}{Brown})", --}
    {--     function: tReplaceMultiple Brown [Brown, Brown, Brown] --}
    {-- }, { --}
    {--     id: "replaceBbyOO", --}
    {--     name: "map({Brown} ↦ {Orange}{Orange})", --}
    {--     function: tReplaceMultiple Brown [Orange, Orange] --}
    }, {
        id: "rejectO",
        name: "reject({Orange})",
        function: map (reject (== Orange)) >>> tClearEmpty
    {-- }, { --}
    {--     id: "pushY", --}
    {--     name: "map(push({Yellow}))", --}
    {--     function: map (flip snoc Yellow) --}
    {-- }, { --}
    {--     id: "tail", --}
    {--     name: "map(tail)", --}
    {--     function: tTail --}
    }
]

getTransformerById :: String -> Maybe Transformer
getTransformerById id = (\x -> x.function) <$> (head $ filter (\t -> t.id == id) transformers)
