module Storage where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Data.Array as A
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))

import Data.StrMap as SM

import Types (GameState, TransformerId, LevelId)

foreign import data STORAGE :: Effect

type SaveableGameState = {
    currentLevel :: LevelId,
    levelState :: SM.StrMap (Array TransformerId)
}

toSaveable :: GameState -> SaveableGameState
toSaveable gs = {
        currentLevel: gs.currentLevel,
        levelState: A.fromFoldable <$> gs.levelState
    }

fromSaveable :: SaveableGameState -> GameState
fromSaveable sgs = {
        currentLevel: sgs.currentLevel,
        levelState: fromFoldable <$> sgs.levelState
    }


-- | Retrieve the current game state from local storage (FFI, needs 'Just' and 'Nothing' as input)
foreign import unsafeLoadGameState :: forall a eff. (a -> Maybe a)
                                   -> (Maybe a)
                                   -> Eff (storage :: STORAGE | eff) (Maybe SaveableGameState)

-- | Retrieve game state from local storage
loadGameState :: forall eff. Eff (storage :: STORAGE | eff) (Maybe GameState)
loadGameState = map fromSaveable <$> unsafeLoadGameState Just Nothing

-- | Store a game state in local storage (unsafe)
foreign import unsafeSaveGameState :: forall eff. SaveableGameState
                                   -> Eff (storage :: STORAGE | eff) Unit

-- | Store a game state in local storage
saveGameState :: forall eff. GameState
              -> Eff (storage :: STORAGE | eff) Unit
saveGameState = toSaveable >>> unsafeSaveGameState
