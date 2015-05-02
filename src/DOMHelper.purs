module DOMHelper where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types

{-- import Types --}

-- | Get the parent element in the DOM tree, this should be in purescript-simple-dom
foreign import parentElement """
    function parentElement(child) {
        return function() {
            return child.parentElement;
        };
    } """ :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) HTMLElement

-- | Read the URL hash from a given DOM location
foreign import getLocationHash """
    function getLocationHash(loc) {
        return function() {
            if (loc.hash) {
                return loc.hash.substring(1);
            } else {
                return "";
            }
        };
    } """ :: forall eff. DOMLocation -> Eff (dom :: DOM | eff) String
