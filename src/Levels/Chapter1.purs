module Levels.Chapter1 where

import Prelude
import Data.List
import Data.Maybe

import Helper
import Transformer
import Types

contains :: forall a. (Eq a) => a -> List a -> Boolean
contains x xs = isJust $ elemIndex x xs

chapter1 :: Chapter
chapter1 = {
    name: "Chapter 1",

    transformers: fromArray [
        "mapYtoYR" :> {
            name: "map {Yellow}↦{Yellow}{Red}",
            function: replaceMultiple Yellow (Yellow : Red : Nil)
        },
        "mapCtoRC" :> {
            name: "map {Cyan}↦{Red}{Cyan}",
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
            function: map (`snoc` Red)
        },
        "mapReverse" :> {
            name: "map reverse",
            function: map reverse
        }
    ],

    levels: fromArray [
        "1.1" :-> {
            name: "Mercury",
            help: Just """There are a few new functions in this chapter. You will be able to
                          figure out what they do by playing around.""",
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Red, Red]]
        },
        "1.2" :-> {
            name: "Venus",
            help: Nothing,
            difficulty: Medium,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red]]
        },
        "1.3" :-> {
            name: "Earth",
            help: Nothing,
            difficulty: Easy,
            initial: [[Cyan, Cyan, Yellow], [Cyan, Red], [Cyan, Red], [Cyan, Cyan, Yellow]],
            target: [[Red, Cyan, Cyan], [Red, Cyan], [Red, Cyan], [Red, Cyan, Cyan]]
        },
        "1.4" :-> {
            name: "Mars",
            help: Just """In case you were wondering: The level names are pretty arbitrary.""",
            difficulty: Medium,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red], [Red, Red], [Red, Red]]
        }
    ]
}
