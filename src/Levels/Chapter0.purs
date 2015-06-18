module Levels.Chapter0 where

import Prelude
import Data.List
import Data.Maybe

import Helper
import Transformer
import Types


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
            function: map (`snoc` Yellow)
        },
        "replaceYbyYR" :> {
            name: "map {Yellow}↦{Yellow}{Red}",
            function: replaceMultiple Yellow (Yellow : Red : Nil)
        },
        "rejectY" :> {
            name: "map (reject {Yellow})",
            function: mapReject Yellow
        }
    ],

    levels: fromArray [
        "0.1" :-> {
            name: "Introduction",
            help: Just """In this game, your goal is to create a sequence of functions which
                          transforms the stack of cubes into the desired pattern (shown above).
                          To solve this level, add the function `replaceYbyR` to your program by
                          dragging it to the list on the right side.
                          <code>map f</code> applies the function <code>f</code> to every
                          <i>stack</i> of cubes. Here, <code>f</code> replaces every {Yellow} cube
                          in a stack by a {Red} cube.""",
            difficulty: Easy,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Red, Red], [Red, Red], [Red], [Red], [Red, Red], [Red, Red, Red]]
        },
        "0.2" :-> {
            name: "Composition",
            help: Just """Most levels require a combination of two or more functions. Try to
                          add the functions `stackY` and `rejectY` to your program (by clicking
                          or dragging). You can change the order of the functions by dragging
                          them to a new position. Try to understand the effect of these two
                          functions by observing how the cubes change.""",
            difficulty: Easy,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow], [Red, Yellow]]
        },
        "0.3" :-> {
            name: "Spanish flag",
            help: Just """Try this on your own. You need to compose three
                          functions.""",
            difficulty: Medium,
            initial: [[Yellow, Yellow, Red], [Yellow, Red], [Red], [Red], [Yellow, Red], [Yellow, Yellow, Red]],
            target: [[Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red], [Red, Yellow, Red]]
        }
    ]
}
