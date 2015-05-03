module Sortable where

import Control.Monad.Eff
import Data.DOM.Simple.Types
import DOM

-- | Install 'Sortable' on the given DOM element. The second argument is
-- | an event handler that is called if the list is modified.
foreign import installSortable
  """
  function installSortable(el) {
    return function(sortHandler) {
      return function() {
        new Sortable(el, {
          group: 'function-lists',
          ghostClass: 'sortable-ghost',
          animation: 150,
          onSort: sortHandler
        });
      };
    };
  }
  """ :: forall eff. HTMLElement
      -> (Eff (dom :: DOM | eff) Unit)
      -> Eff (dom :: DOM | eff) Unit
