module Sortable where

import Prelude
import Control.Monad.Eff
import DOM
import DOM.Node.Types

-- | Install 'Sortable' on the given DOM element. The second argument is
-- | an event handler that is called if the list is modified.
foreign import installSortable :: forall eff. Element
                               -> (Eff (dom :: DOM | eff) Unit)
                               -> Eff (dom :: DOM | eff) Unit
