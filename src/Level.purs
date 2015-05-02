module Level where

import Data.Array
import Data.Foldable
import Data.Maybe

import Levels.Chapter1
import Types

-- | A simple list of all available chapters
allChapters :: [Chapter]
allChapters = [chapter1]

-- | A simple list of all available levels across the chapters
allLevels :: [Level]
allLevels = concatMap _.levels allChapters

-- | Find a given level by its id
getLevelById :: String -> Maybe Level
getLevelById lid = find (\l -> l.id == lid) allLevels
