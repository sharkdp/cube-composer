![cube composer](https://raw.githubusercontent.com/sharkdp/cube-composer/master/img/cube-composer.png)

A puzzle game inspired by functional programming,
written in [PureScript](https://github.com/purescript/purescript).

[**Play it online!**](https://david-peter.de/cube-composer)

## Local install

```sh
git clone https://github.com/sharkdp/cube-composer.git
cd cube-composer
npm install
bower install
gulp
```
If no errors occur, the game can now be played on a browser by opening `index.html`.

## Creating new levels

Levels are grouped into chapters. Each chapter has a dedicated file in the folder [`src/Levels/`](src/Levels/). To create a new chapter, you can copy one of the existing files, bump the chapter number and add it to the `allChapters` list in [`Levels.purs`](src/Levels.purs). Each chapter comes with a specific list of `transformers` (functions). As an example, we look at `map (Yellow ↦ Red)` in `Chapter0.purs`:
``` purescript
"replaceYbyR" :> {
    name: "map {Yellow}↦{Red}",
    function: replaceSingle Yellow Red
}
```
Here, `replaceYbyR` is an internal ID which is used to identify the transformer, `map {Yellow}↦{Red}` is the displayed name of the transformer (`{x}` will be replaced by a small cube of color `x`) and `replaceSingle Yellow Red` is the implementation of the transformer. The `function` field in the record has to be of type `Transformer`, where
``` purescript
type Stack = List Cube
type Wall = List Stack
type Transformer = Wall -> Wall
```
Consequently, a `Transformer` is a function that transforms a 2D array of cubes (`Wall`). Some basic transformers are given in [`Transformer.purs`](src/Transformer.purs).

Each level is given by a record like
``` purescript
"0.2" :-> {
  name: "Level title",
  help: Just "...",
  difficulty: Easy,
  initial: [[Yellow, Yellow, Red], [Yellow, Red], ...],
  target: [[Red], [Red], [Red], [Red], [Red], [Red]]
}
```
where `0.2` is the `Chapter.Level` ID of the level, `name` is the title of the puzzle, `help` is the help text shown in the right panel, difficulty is `Easy`, `Medium` or `Hard` and `initial :: Wall` and `target :: Wall` are the inital and target 2D arrays of cubes.

To view all levels with all shortest solutions for each (can be useful, among other things, to verify newly introduced levels can be solved as planned – and cannot be solved in another way which undermines their point):
```sh
gulp prod
node dist/cli.js
```

Please send a pull request if you would like to add your puzzles to the game.

## CI status
[![Build Status](https://img.shields.io/travis/sharkdp/cube-composer.svg?style=flat)](https://travis-ci.org/sharkdp/cube-composer)
