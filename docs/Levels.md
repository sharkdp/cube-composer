## Module Levels

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

#### `firstLevel`

``` purescript
firstLevel :: LevelId
```

ID of the first level

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
getTransformerRecord :: Chapter -> TransformerId -> Maybe TransformerRecord
```

Find a specific transformer + metadata by its id

#### `getTransformer`

``` purescript
getTransformer :: Chapter -> TransformerId -> Maybe Transformer
```

Find a specific transformer by its id


