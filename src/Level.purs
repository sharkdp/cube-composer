module Level where

import Data.Array
import Data.Foldable
import Data.Maybe
import qualified Data.StrMap as SM

import Levels.Chapter1
import Types

-- | A simple list of all available chapters
allChapters :: [Chapter]
allChapters = [chapter1]

-- | A dictionary of all available levels across the chapters
allLevels :: SM.StrMap Level
allLevels = SM.unions (map _.levels allChapters)

-- | Find a given level by its id
getLevelById :: String -> Maybe Level
getLevelById = flip SM.lookup allLevels
