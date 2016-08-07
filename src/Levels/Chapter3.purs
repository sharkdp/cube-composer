module Levels.Chapter3 where

import Prelude
import Data.List (List(..), (:), concatMap)
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import Transformer (replaceSingle)
import Types (Chapter, Stack, Cube(..), Difficulty(..))

cxToX :: Stack -> Stack
cxToX Nil                     = Nil
cxToX (Cons Cyan (Cons y xs)) = y : cxToX xs
cxToX (Cons x xs)             = x : cxToX xs

ooToC :: Stack -> Stack
ooToC Nil                            = Nil
ooToC (Cons Orange (Cons Orange xs)) = Cyan : ooToC xs
ooToC (Cons x cs)                    = x : ooToC cs

chapter3 :: Chapter
chapter3 = {
    name: "Chapter 3",

    transformers: fromArray [
        "mapXtoOX" :> {
            name: "map {X}↦[{X}{Orange}]",
            function: map (concatMap (\x -> (Orange : x : Nil)))
        },
        "mapCXtoX" :> {
            name: "map [{X}{Cyan}]↦{X}",
            function: map cxToX
        },
        "mapOOtoC" :> {
            name: "map [{Orange}{Orange}]↦{Cyan}",
            function: map ooToC
        },
        "mapCtoO" :> {
            name: "map {Cyan}↦{Orange}",
            function: replaceSingle Cyan Orange
        }
    ],

    levels: fromArray [
        "3.1" :-> {
            name: "Brick",
            help: Just """This chapter introduces wildcard cubes: {X}.""",
            difficulty: Easy,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Cyan], [Cyan, Orange], [Cyan], [Cyan, Orange], [Cyan]]
        },
        "3.2" :-> {
            name: "Fort",
            help: Nothing,
            difficulty: Hard,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Orange, Cyan], [Orange, Orange], [Orange, Cyan], [Orange, Orange], [Orange, Cyan]]
        },
        "3.3" :-> {
            name: "Castle",
            help: Nothing,
            difficulty: Medium,
            initial: [[Orange], [Orange, Orange], [Orange, Orange, Orange], [Orange, Orange, Orange, Orange], [Orange, Orange, Orange], [Orange, Orange], [Orange]],
            target: [[Orange, Orange], [Orange, Cyan], [Orange, Orange], [Orange, Cyan], [Orange, Orange], [Orange, Cyan], [Orange, Orange]]
        }
    ]
}
