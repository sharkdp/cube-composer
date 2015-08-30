## Module DOMHelper

#### `getDocument`

``` purescript
getDocument :: forall eff. Eff (dom :: DOM | eff) Document
```

#### `getElementById'`

``` purescript
getElementById' :: forall eff. String -> Document -> Eff (dom :: DOM | eff) (Maybe Element)
```

#### `withElementById`

``` purescript
withElementById :: forall eff. String -> Document -> (Element -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
```

Perform a DOM action with a single element which can be accessed by ID

#### `children'`

``` purescript
children' :: forall eff. Element -> Eff (dom :: DOM | eff) (Array HTMLElement)
```

#### `addEventListener'`

``` purescript
addEventListener' :: forall eff. EventType -> (Event -> Eff (dom :: DOM | eff) Unit) -> EventTarget -> Eff (dom :: DOM | eff) Unit
```

#### `unsafeElementToHTMLElement`

``` purescript
unsafeElementToHTMLElement :: Element -> HTMLElement
```

#### `unsafeEventToKeyboardEvent`

``` purescript
unsafeEventToKeyboardEvent :: Event -> KeyboardEvent
```

#### `unsafeGetAttribute`

``` purescript
unsafeGetAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) String
```

#### `getSelectedValue`

``` purescript
getSelectedValue :: forall eff. Element -> Eff (dom :: DOM | eff) String
```

#### `setInnerHTML`

``` purescript
setInnerHTML :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit
```

#### `htmlCollectionToArray`

``` purescript
htmlCollectionToArray :: HTMLCollection -> Array HTMLElement
```

#### `keyCode`

``` purescript
keyCode :: KeyboardEvent -> Int
```

#### `ctrlKey`

``` purescript
ctrlKey :: KeyboardEvent -> Boolean
```

#### `setStyleAttribute`

``` purescript
setStyleAttribute :: forall eff. String -> String -> HTMLElement -> Eff (dom :: DOM | eff) Unit
```

#### `classAdd`

``` purescript
classAdd :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit
```

#### `classRemove`

``` purescript
classRemove :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit
```


