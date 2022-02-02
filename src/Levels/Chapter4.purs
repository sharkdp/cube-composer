module Levels.Chapter4 where

import Data.List (List(..), (:), partition, concat)
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import ListHelper (contains)
import Transformer (replaceSingle)
import Types (Chapter, Transformer, Cube(..), Difficulty(..))

partitionContains :: Cube -> Transformer
partitionContains cube wall =
    let parts = partition (contains cube) wall
    in concat (parts.no : parts.yes : Nil)

chapter4 :: Chapter
chapter4 = {
    name: "Chapter 4",

    transformers: fromArray [
        "replaceYbyR" :> {
            name: "map {Yellow}↦{Red}",
            function: replaceSingle Yellow Red
        },
        "replaceRbyC" :> {
            name: "map {Red}↦{Cyan}",
            function: replaceSingle Red Cyan
        },
        "replaceCbyY" :> {
            name: "map {Cyan}↦{Yellow}",
            function: replaceSingle Cyan Yellow
        },
        "partitionContainsC" :> {
            name: "partition (contains {Cyan})",
            function: partitionContains Cyan
        },
        "partitionContainsR" :> {
            name: "partition (contains {Red})",
            function: partitionContains Red
        }
    ],

    levels: fromArray [
        "4.1" :-> {
            name: "Take sides!",
            help: Just """This chapter introduces partitioning. The function `partitionContainsR` reorders the columns
                          so that the columns which do not contain a red cube are grouped on the left, and the columns
                          which do are gouped on the right.""",
            difficulty: Easy,
            initial: [[Cyan, Red], [Cyan, Cyan], [Red, Red], [Cyan, Cyan], [Cyan, Red]],
            target: [[Cyan, Cyan], [Cyan, Cyan], [Cyan, Red], [Red, Red], [Cyan, Red]]
        },
        "4.2" :-> {
            name: "Take sides – again!",
            help: Just """Note that within each partition – the columns which don't satisfy the condition and the
                          columns which do – the order remains the same as it was prior to partitioning.""",
            difficulty: Medium,
            initial: [[Cyan, Red], [Cyan, Cyan], [Red, Red], [Cyan, Cyan], [Cyan, Red]],
            target: [[Cyan, Cyan], [Cyan, Cyan], [Red, Red], [Cyan, Red], [Cyan, Red]]
        },
        "4.3" :-> {
            name: "Shift",
            help: Just "Can you partition this?",
            difficulty: Medium,
            initial: [[Cyan, Red], [Red, Cyan], [Cyan, Red], [Red, Cyan], [Cyan, Red]],
            target: [[Red, Cyan], [Cyan, Red], [Red, Cyan], [Cyan, Red], [Red, Cyan]]
        },
        "4.4" :-> {
            name: "Robot eyes",
            help: Nothing,
            difficulty: Medium,
            initial: [[Brown, Brown, Brown], [Brown, Yellow, Brown], [Brown, Brown, Brown], [Brown, Yellow, Brown], [Brown, Brown, Brown]],
            target: [[Brown, Brown, Brown], [Brown, Brown, Brown], [Brown, Brown, Brown], [Brown, Yellow, Brown], [Brown, Yellow, Brown]]
        },
        "4.5" :-> {
            name: "Mountains",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Cyan, Cyan, Cyan], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan], [Brown, Brown, Brown, Cyan], [Brown, Brown, Cyan, Cyan]]
        }
    ]
}
