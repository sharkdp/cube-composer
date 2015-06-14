## Module Level

#### `allChapters`

``` purescript
allChapters :: List Chapter
```

A simple list of all available chapters

#### `allLevels`

``` purescript
allLevels :: StrMap Level
```

A dictionary of all available levels across the chapters

#### `allLevelIds`

``` purescript
allLevelIds :: List LevelId
```

A list of all level ids across the chapters

#### `getLevel`

``` purescript
getLevel :: LevelId -> Level
```

Find a given level by its id

#### `levelTitle`

``` purescript
levelTitle :: LevelId -> Level -> String
```

Level id, name and difficulty as a single string

#### `getChapter`

``` purescript
getChapter :: LevelId -> Chapter
```

Get the chapter to which a level belongs

#### `getTransformerRecord`

``` purescript
getTransformerRecord :: Chapter -> TransformerId -> TransformerRecord
```

#### `getTransformer`

``` purescript
getTransformer :: Chapter -> TransformerId -> Transformer
```

Find a specific transformer by its id


