module Level where

import Data.Array
import Data.Foldable
import Data.Maybe

import Levels.Chapter1
import Types

allChapters :: [Chapter]
allChapters = [chapter1]

allLevels :: [Level]
allLevels = concatMap _.levels allChapters

getLevelById :: LevelId -> Maybe Level
getLevelById lid = find (\l -> l.id == lid) allLevels
