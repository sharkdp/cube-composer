## Module DOMHelper

#### `parentElement`

``` purescript
parentElement :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) HTMLElement
```

Get the parent element in the DOM tree, this should be in purescript-simple-dom

#### `withElementById`

``` purescript
withElementById :: forall eff. String -> HTMLDocument -> (HTMLElement -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
```

Perform a DOM action with a single element which can be accessed by ID

#### `addChangeEventListener`

``` purescript
addChangeEventListener :: forall eff. (HTMLElement -> Eff (dom :: DOM | eff) Unit) -> HTMLElement -> Eff (dom :: DOM | eff) Unit
```

Add a 'change' event handler to an element

#### `getSelectedValue`

``` purescript
getSelectedValue :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) String
```


