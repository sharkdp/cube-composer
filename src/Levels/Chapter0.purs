module Levels.Chapter0 where

import Prelude
import Data.List (List(..), (:), snoc)
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import Transformer (mapReject, replaceMultiple, replaceSingle)
import Types (Chapter, Cube(..), Difficulty(..))

chapter0 :: Chapter
chapter0 = {
    name: "Introduction",

    transformers: fromArray [
        "replaceYbyR" :> {
            name: "map {Yellow}↦{Red}",
            function: replaceSingle Yellow Red
        },
        "stackY" :> {
            name: "map (stack {Yellow})",
            function: map (_ `snoc` Yellow)
        },
        "replaceYbyYR" :> {
            name: "map {Yellow}↦[{Red}{Yellow}]",
            function: replaceMultiple Yellow (Yellow : Red : Nil)
        },
        "rejectY" :> {
            name: "map (reject {Yellow})",
            function: mapReject Yellow
        }
    ],

    levels: fromArray [
        "0.1" :-> {
            name: "Transformation",
            help: Just """In this game, your goal is to create a sequence of functions which
                          transforms the colored cubes into the desired pattern (shown above).
                          To change yellow cubes to red cubes, add the function `replaceYbyR` to your program.
                          You can do so by clicking on the function or by dragging it to the
                          program on the right.""",
            difficulty: Easy,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Red, Red], [Red, Red], [Red], [Red], [Red, Red], [Red, Red, Red]]
        },
        "0.2" :-> {
            name: "Rejection",
            help: Just """To remove all cubes of a specified color, use the <code>reject</code>
                          function.""",
            difficulty: Easy,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red], [Red], [Red], [Red], [Red], [Red]]
        },
        "0.3" :-> {
            name: "Composition",
            help: Just """Most levels require a combination of two or more functions. Try to
                          add the functions `stackY` and `rejectY` to your program. Note that
                          you can change the order of the functions by drag and drop. Try to
                          understand the effect of `stackY` by observing how the cubes change.""",
            difficulty: Easy,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow]]
        },
        "0.4" :-> {
            name: "Spanish flag",
            help: Just """Try this on your own. You need to compose three
                          functions.""",
            difficulty: Medium,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red]]
        }
    ]
}
