module Sortable where

import Control.Monad.Eff
import Data.DOM.Simple.Window
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Types
import DOM

foreign import installSortable """
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
  } """ :: forall eff. HTMLElement -> (Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
