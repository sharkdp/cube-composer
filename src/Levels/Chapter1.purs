module Levels.Chapter1 where

import Prelude
import Data.List (List(..), reverse, snoc, filter, (:))
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import ListHelper (contains)
import Transformer (clearEmpty, mapReject, replaceMultiple)
import Types (Chapter, Cube(..), Difficulty(..))

chapter1 :: Chapter
chapter1 = {
    name: "Chapter 1",

    transformers: fromArray [
        "mapYtoYR" :> {
            name: "map {Yellow}↦[{Red}{Yellow}]",
            function: replaceMultiple Yellow (Yellow : Red : Nil)
        },
        "mapCtoRC" :> {
            name: "map {Cyan}↦[{Cyan}{Red}]",
            function: replaceMultiple Cyan (Red : Cyan : Nil)
        },
        "rejectY" :> {
            name: "map (reject {Yellow})",
            function: mapReject Yellow
        },
        "rejectC" :> {
            name: "map (reject {Cyan})",
            function: mapReject Cyan
        },
        "filterContainsR" :> {
            name: "filter (contains {Red})",
            function: filter (contains Red) >>> clearEmpty
        },
        "stackR" :> {
            name: "map (stack {Red})",
            function: map (_ `snoc` Red)
        },
        "mapReverse" :> {
            name: "map reverse",
            function: map reverse
        }
    ],

    levels: fromArray [
        "1.1" :-> {
            name: "Mercury",
            help: Just """There are some new types of functions in this chapter. We will
                          introduce them when they are needed. Note that you can always
                          skip levels and come back later.""",
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Red, Red]]
        },
        "1.2" :-> {
            name: "Venus",
            help: Just """The function `filterContainsR` removes columns without a red cube.""",
            difficulty: Medium,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red]]
        },
        "1.3" :-> {
            name: "Earth",
            help: Just """You can flip each column vertically with `mapReverse`.""",
            difficulty: Easy,
            initial: [[Cyan, Cyan, Yellow], [Cyan, Red], [Cyan, Red], [Cyan, Cyan, Yellow]],
            target: [[Red, Cyan, Cyan], [Red, Cyan], [Red, Cyan], [Red, Cyan, Cyan]]
        },
        "1.4" :-> {
            name: "Mars",
            help: Just """In case you were wondering: the level names <s>have a rather deep
                          philosophical meaning</s> are chosen randomly.""",
            difficulty: Medium,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red], [Red, Red], [Red, Red]]
        }
    ]
}
