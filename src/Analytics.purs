module Analytics where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import DOM (DOM)

import Types (LevelId)

foreign import analyticsEvent :: forall eff. String
                              -> String
                              -> String
                              -> Eff (dom :: DOM | eff) Unit

analyticsLevelChanged :: forall eff. LevelId -> Eff (dom :: DOM | eff) Unit
analyticsLevelChanged = analyticsEvent "level" "changed"
