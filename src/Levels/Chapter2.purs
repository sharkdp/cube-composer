module Levels.Chapter2 where

import Data.Array
import qualified Data.StrMap as SM

import Helper
import Transformer
import Types

chapter2 :: Chapter
chapter2 = {
    name: "Chapter 2",

    transformers: SM.fromList [
        "mapYtoYR" :> {
            name: "map {Yellow}↦{Yellow}{Red}",
            function: tReplaceMultiple Yellow [Yellow, Red]
        },
        "mapCtoRC" :> {
            name: "map {Cyan}↦{Red}{Cyan}",
            function: tReplaceMultiple Cyan [Red, Cyan]
        },
        "rejectY" :> {
            name: "map (reject {Yellow})",
            function: map (reject (== Yellow)) >>> tClearEmpty
        },
        "rejectC" :> {
            name: "map (reject {Cyan})",
            function: map (reject (== Cyan)) >>> tClearEmpty
        },
        "filterContainsR" :> {
            name: "filter (contains {Red})",
            function: filter (\stack -> Red `elemIndex` stack /= -1) >>> tClearEmpty
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

    levels: SM.fromList [
        "2.1" :> {
            name: "Mercury",
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red], [Red, Red], [Red, Red]]
        },
        "2.2" :> {
            name: "Venus",
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red], [Red, Red]]
        },
        "2.3" :> {
            name: "Earth",
            difficulty: Easy,
            initial: [[Cyan, Cyan, Yellow], [Cyan, Red], [Cyan, Red], [Cyan, Cyan, Yellow]],
            target: [[Red, Cyan, Cyan], [Red, Cyan], [Red, Cyan], [Red, Cyan, Cyan]]
        },
        "2.4" :> {
            name: "Mars",
            difficulty: Easy,
            initial: [[Red, Red], [Red, Yellow], [Cyan, Yellow], [Cyan, Cyan]],
            target: [[Red, Red, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Red, Red]]
        }
    ]
}
