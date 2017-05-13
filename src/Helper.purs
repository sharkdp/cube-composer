module Helper where

import Prelude
import Data.List (fromFoldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.StrMap as SM

import Types (Level, LevelId, Difficulty, Wall, Cube)

-- | Create a StrMap from an Array of (key, value) pairs
fromArray :: forall a. Array (Tuple String a) -> SM.StrMap a
fromArray = SM.fromFoldable

-- | Operator to create tuples, especially for creating maps with
-- | `Map.fromList ["key1" :> "value1", "key2" :> "value2"]`
tuple :: forall a b. a -> b -> Tuple a b
tuple a b = Tuple a b

infixl 6 tuple as :>

-- | Array analogs of the Stack and Wall types
type AStack = Array Cube
type AWall = Array AStack

-- | Convert 2D Array to List
convert :: AWall -> Wall
convert = fromFoldable <<< map fromFoldable

-- | Helper type to create levels from arrays
type LevelEntry = {
    name       :: String,
    help       :: Maybe String,
    difficulty :: Difficulty,
    initial    :: AWall,
    target     :: AWall
}

-- | Helper function to create levels from arrays of cubes (instead of lists)
level :: LevelId
      -> LevelEntry
      -> Tuple LevelId Level
level lid entry =
    lid :> {
        name: entry.name,
        help: entry.help,
        difficulty: entry.difficulty,
        initial: convert entry.initial,
        target: convert entry.target
    }

infixl 6 level as :->
