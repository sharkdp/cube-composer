module DOMHelper where

import Prelude
import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.Maybe

-- | Get the parent element in the DOM tree, this should be in purescript-simple-dom
foreign import parentElement :: forall eff. HTMLElement
                             -> Eff (dom :: DOM | eff) HTMLElement

-- | Perform a DOM action with a single element which can be accessed by ID
withElementById :: forall eff. String
                -> HTMLDocument
                -> (HTMLElement -> Eff (dom :: DOM | eff) Unit)
                -> Eff (dom :: DOM | eff) Unit
withElementById id doc f = getElementById id doc >>= maybe (return unit) f

-- | Add a 'change' event handler to an element
foreign import addChangeEventListener :: forall eff. (HTMLElement -> Eff (dom :: DOM | eff) Unit)
                                      -> HTMLElement
                                      -> Eff (dom :: DOM | eff) Unit

foreign import getSelectedValue :: forall eff. HTMLElement
                                -> Eff (dom :: DOM | eff) String
