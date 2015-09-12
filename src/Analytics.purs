module Analytics where

import Prelude
import Control.Monad.Eff
import DOM

import Types

foreign import analyticsEvent :: forall eff. String
                              -> String
                              -> String
                              -> Eff (dom :: DOM | eff) Unit

analyticsLevelChanged :: forall eff. LevelId -> Eff (dom :: DOM | eff) Unit
analyticsLevelChanged = analyticsEvent "level" "changed"
