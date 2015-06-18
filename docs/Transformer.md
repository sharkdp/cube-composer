## Module Transformer

#### `map2d`

``` purescript
map2d :: (Cube -> Cube) -> Wall -> Wall
```

Map a function over the two dimensional array (= wall)

#### `reject`

``` purescript
reject :: forall a. (a -> Boolean) -> List a -> List a
```

Opposite of filter, reject all values which satisfy the pattern

#### `allSteps`

``` purescript
allSteps :: List Transformer -> Wall -> List Wall
```

Successively apply all transformers to the initial wall and return
all (intermediate) transformation steps

#### `transformed`

``` purescript
transformed :: List Transformer -> Wall -> Wall
```

Return the final step of the transformation chain

#### `clearEmpty`

``` purescript
clearEmpty :: Transformer
```

Remove emtpy stacks

#### `mapReject`

``` purescript
mapReject :: Cube -> Transformer
```

Reject all cubes of a certain color

#### `mapStack`

``` purescript
mapStack :: Cube -> Transformer
```

Stack a single cube on top of each column

#### `replaceSingle`

``` purescript
replaceSingle :: Cube -> Cube -> Transformer
```

Replace all occurences of a certain cube with another

#### `replaceMultiple`

``` purescript
replaceMultiple :: Cube -> List Cube -> Transformer
```

Replace all occurences of a certain cube with a list of new cubes


