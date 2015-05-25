module Level where

import Data.Array
import Data.Foldable
import Data.Maybe
import qualified Data.StrMap as SM

import Types
import Unsafe

import Levels.Chapter1
import Levels.Chapter2

-- | A simple list of all available chapters
allChapters :: [Chapter]
allChapters = [chapter1, chapter2]

-- | A dictionary of all available levels across the chapters
allLevels :: SM.StrMap Level
allLevels = SM.unions (map _.levels allChapters)

-- | Find a given level by its id
getLevel :: LevelId -> Level
getLevel lid =
    case (SM.lookup lid allLevels) of
         Just level -> level
         Nothing -> unsafeError $ "Could not find level " ++ show lid

-- | Get the chapter to which a level belongs
getChapter :: LevelId -> Chapter
getChapter lid =
    case (find hasLevel allChapters) of
         Just chapter -> chapter
         Nothing -> unsafeError $ "Could not find chapter " ++ show lid
    where hasLevel ch = SM.member lid ch.levels

getTransformerRecord :: Chapter -> TransformerId -> TransformerRecord
getTransformerRecord chapter tid =
    case (SM.lookup tid chapter.transformers) of
         Just t -> t
         Nothing -> unsafeError $ "Could not find transformer " ++ show tid

-- | Find a specific transformer by its id
getTransformer :: Chapter -> TransformerId -> Transformer
getTransformer ch tid = _.function $ getTransformerRecord ch tid
