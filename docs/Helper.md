## Module Helper

#### `fromArray`

``` purescript
fromArray :: forall a. Array (Tuple String a) -> StrMap a
```

Create a StrMap from an Array of (key, value) pairs

#### `tuple`

``` purescript
tuple :: forall a b. a -> b -> Tuple a b
```

Operator to create tuples, especially for creating maps with
`Map.fromList ["key1" :> "value1", "key2" :> "value2"]`

#### `(:>)`

``` purescript
infixl 6 tuple as :>
```

#### `AStack`

``` purescript
type AStack = Array Cube
```

Array analogs of the Stack and Wall types

#### `AWall`

``` purescript
type AWall = Array AStack
```

#### `convert`

``` purescript
convert :: AWall -> Wall
```

Convert 2D Array to List

#### `LevelEntry`

``` purescript
type LevelEntry = { name :: String, help :: Maybe String, difficulty :: Difficulty, initial :: AWall, target :: AWall }
```

Helper type to create levels from arrays

#### `level`

``` purescript
level :: LevelId -> LevelEntry -> Tuple LevelId Level
```

Helper function to create levels from arrays of cubes (instead of lists)

#### `(:->)`

``` purescript
infixl 6 level as :->
```


