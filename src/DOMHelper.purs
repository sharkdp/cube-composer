module DOMHelper where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.Maybe

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

-- | Perform a DOM action with a single element which can be accessed by ID
withElementById :: forall eff. String
                            -> HTMLDocument
                            -> (HTMLElement -> Eff (dom :: DOM | eff) Unit)
                            -> Eff (dom :: DOM | eff) Unit
withElementById id doc f = getElementById id doc >>= maybe (return unit) f
