module DOMHelper where

import Prelude
import Control.Monad.Eff (Eff())
import DOM (DOM())
import DOM.Event.Types (Event(), EventType(), EventTarget(), KeyboardEvent(), readKeyboardEvent)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement(), htmlDocumentToDocument, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.Element (getAttribute)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.ParentNode (children)
import DOM.Node.Types (HTMLCollection(), Element(), Document(), ElementId(..),
                       documentToNonElementParentNode, elementToParentNode)
import Data.Either (fromRight)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(), maybe, fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)

getDocument :: forall eff. Eff (dom :: DOM | eff) Document
getDocument = window >>= document <#> htmlDocumentToDocument

getElementById' :: forall eff. String
                -> Document
                -> Eff (dom :: DOM | eff) (Maybe Element)
getElementById' id doc = do
  let docNode = documentToNonElementParentNode doc
  nullableEl <- getElementById (ElementId id) docNode
  pure $ toMaybe nullableEl

-- | Perform a DOM action with a single element which can be accessed by ID
withElementById :: forall eff. String
                -> Document
                -> (Element -> Eff (dom :: DOM | eff) Unit)
                -> Eff (dom :: DOM | eff) Unit
withElementById id doc action = getElementById' id doc >>= maybe (pure unit) action

children' :: forall eff. Element -> Eff (dom :: DOM | eff) (Array HTMLElement)
children' el = htmlCollectionToArray <$> children (elementToParentNode el)

addEventListener' :: forall eff. EventType -> (Event -> Eff (dom :: DOM | eff) Unit) -> EventTarget -> Eff (dom :: DOM | eff) Unit
addEventListener' etype listener target =
  addEventListener etype (eventListener listener) true target

unsafeElementToHTMLElement :: Element -> HTMLElement
unsafeElementToHTMLElement = unsafePartial (fromRight <<< readHTMLElement <<< toForeign)

unsafeEventToKeyboardEvent :: Event -> KeyboardEvent
unsafeEventToKeyboardEvent = unsafePartial (fromRight <<< readKeyboardEvent <<< toForeign)

unsafeGetAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) String
unsafeGetAttribute key el = unsafePartial (fromJust <<< toMaybe) <$> getAttribute key el

foreign import getSelectedValue :: forall eff. Element
                                -> Eff (dom :: DOM | eff) String

foreign import setInnerHTML :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit

foreign import htmlCollectionToArray :: HTMLCollection -> Array HTMLElement

foreign import keyCode :: KeyboardEvent -> Int

foreign import ctrlKey :: KeyboardEvent -> Boolean

foreign import setStyleAttribute :: forall eff. String -> String -> HTMLElement -> Eff (dom :: DOM | eff) Unit

foreign import classAdd :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit

foreign import classRemove :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit
