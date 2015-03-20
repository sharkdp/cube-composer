module CubeComposer where

import Data.Array
import Data.Enum
import Data.Foldable
import Data.Maybe
import Debug.Trace

data Cube = Blue | Brown | Red | Orange | Yellow

instance showCube :: Show Cube where
    show Blue = "Blue"
    show Brown = "Brown"
    show Red = "Red"
    show Orange = "Orange"
    show Yellow = "Yellow"

instance eqCube :: Eq Cube where
    (==) a b = fromEnum a == fromEnum b
    (/=) a b = not (a == b)

instance ordCube :: Ord Cube where
    compare a b = fromEnum a `compare` fromEnum b

instance enumCube :: Enum Cube where
    cardinality = Cardinality 5
    firstEnum = cubeFirst
    lastEnum = cubeLast
    succ = defaultSucc cubeToEnum cubeFromEnum
    pred = defaultPred cubeToEnum cubeFromEnum
    toEnum = cubeToEnum
    fromEnum = cubeFromEnum

cubeFirst = Blue
cubeLast = Yellow

cubeFromEnum Blue = 0
cubeFromEnum Brown = 1
cubeFromEnum Red = 2
cubeFromEnum Orange = 3
cubeFromEnum Yellow = 4

cubeToEnum 0 = Just Blue
cubeToEnum 1 = Just Brown
cubeToEnum 2 = Just Red
cubeToEnum 3 = Just Orange
cubeToEnum 4 = Just Yellow
cubeToEnum _ = Nothing

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
