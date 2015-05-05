module Level where

import Data.Array
import Data.Foldable
import Data.Maybe
import qualified Data.StrMap as SM

import Types
import Levels.Chapter1
import Levels.Chapter2

-- | A simple list of all available chapters
allChapters :: [Chapter]
allChapters = [chapter1, chapter2]

-- | A dictionary of all available levels across the chapters
allLevels :: SM.StrMap Level
allLevels = SM.unions (map _.levels allChapters)

-- | Find a given level by its id
getLevelById :: LevelId -> Maybe Level
getLevelById = flip SM.lookup allLevels

-- | Get the chapter to which a level belongs
getChapter :: LevelId -> Maybe Chapter
getChapter lid = find hasLevel allChapters
    where hasLevel ch = SM.member lid ch.levels
