module Storage where

import Control.Monad.Eff
import DOM
import Data.Maybe

import Types

-- | Retrieve the current game state from local storage (FFI, needs 'Just' and 'Nothing' as input)
foreign import helpLoadGameState """
    function helpLoadGameState(just) {
        return function(nothing) {
            return function() {
                var data = localStorage.getItem('gameState');
                if (!data) {
                    return nothing;
                }
                return just(JSON.parse(data));
            };
        };
    } """ :: forall a eff. (a -> Maybe a) -> (Maybe a) -> Eff (dom :: DOM | eff) (Maybe GameState)

-- | Retrieve game state from local storage
loadGameState :: forall eff. Eff (dom :: DOM | eff) (Maybe GameState)
loadGameState = helpLoadGameState Just Nothing

-- | Store a game state in local storage
foreign import saveGameState """
    function saveGameState(gs) {
        return function() {
            localStorage.setItem('gameState', JSON.stringify(gs));
            return {};
        };
    } """ :: forall eff. GameState -> Eff (dom :: DOM | eff) Unit
