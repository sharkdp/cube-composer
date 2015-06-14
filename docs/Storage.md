## Module Storage

#### `Storage`

``` purescript
data Storage :: !
```

#### `SaveableGameState`

``` purescript
type SaveableGameState = { currentLevel :: LevelId, levelState :: StrMap (Array TransformerId) }
```

#### `toSaveable`

``` purescript
toSaveable :: GameState -> SaveableGameState
```

#### `fromSaveable`

``` purescript
fromSaveable :: SaveableGameState -> GameState
```

#### `unsafeLoadGameState`

``` purescript
unsafeLoadGameState :: forall a eff. (a -> Maybe a) -> Maybe a -> Eff (storage :: Storage | eff) (Maybe SaveableGameState)
```

Retrieve the current game state from local storage (FFI, needs 'Just' and 'Nothing' as input)

#### `loadGameState`

``` purescript
loadGameState :: forall eff. Eff (storage :: Storage | eff) (Maybe GameState)
```

Retrieve game state from local storage

#### `unsafeSaveGameState`

``` purescript
unsafeSaveGameState :: forall eff. SaveableGameState -> Eff (storage :: Storage | eff) Unit
```

Store a game state in local storage (unsafe)

#### `saveGameState`

``` purescript
saveGameState :: forall eff. GameState -> Eff (storage :: Storage | eff) Unit
```

Store a game state in local storage


