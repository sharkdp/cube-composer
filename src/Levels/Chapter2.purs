module Levels.Chapter2 where

import Prelude
import Data.List
import Data.Maybe

import Helper
import Transformer
import Types

contains :: forall a. (Eq a) => a -> List a -> Boolean
contains x xs = isJust $ elemIndex x xs

chapter2 :: Chapter
chapter2 = {
    name: "Chapter 2",

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
        "mapPushR" :> {
            name: "map (stack {Red})",
            function: map (`snoc` Red)
        },
        "mapReverse" :> {
            name: "map reverse",
            function: map reverse
        }
    ],

    levels: fromArray [
        "2.1" :-> {
            name: "Mercury",
            difficulty: Medium,
            help: Nothing,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red], [Red, Red], [Red, Red]]
        },
        "2.2" :-> {
            name: "Venus",
            help: Nothing,
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red]]
        },
        "2.3" :-> {
            name: "Earth",
            help: Nothing,
            difficulty: Easy,
            initial: [[Cyan, Cyan, Yellow], [Cyan, Red], [Cyan, Red], [Cyan, Cyan, Yellow]],
            target: [[Red, Cyan, Cyan], [Red, Cyan], [Red, Cyan], [Red, Cyan, Cyan]]
        },
        "2.4" :-> {
            name: "Mars",
            help: Nothing,
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Red, Red]]
        }
    ]
}
