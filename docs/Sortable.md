## Module Sortable

#### `installSortable`

``` purescript
installSortable :: forall eff. Element -> (Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
```

Install 'Sortable' on the given DOM element. The second argument is
an event handler that is called if the list is modified.


