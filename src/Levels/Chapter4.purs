module Levels.Chapter4 where

import Prelude
import Data.List
import Data.Maybe

import Helper
import Transformer
import Types

cxToX :: Stack -> Stack
cxToX Nil                     = Nil
cxToX (Cons Cyan (Cons y xs)) = y : cxToX xs
cxToX (Cons x xs)             = x : cxToX xs

ooToC :: Stack -> Stack
ooToC Nil                            = Nil
ooToC (Cons Orange (Cons Orange xs)) = Cyan : ooToC xs
ooToC (Cons x cs)                    = x : ooToC cs

chapter4 :: Chapter
chapter4 = {
    name: "Chapter 4",

    transformers: fromArray [
        "mapXtoOX" :> {
            name: "map {X}↦{Orange}{X}",
            function: map (concatMap (\x -> (Orange : x : Nil)))
        },
        "mapCXtoC" :> {
            name: "map {Cyan}{X}↦{X}",
            function: map cxToX
        },
        "mapOOtoC" :> {
            name: "map {Orange}{Orange}↦{Cyan}",
            function: map ooToC
        },
        "mapCtoO" :> {
            name: "map {Cyan}↦{Orange}",
            function: replaceSingle Cyan Orange
        },
        "rejectSizeG2" :> {
            name: "reject (size > 2)",
            function: reject (\x -> length x > 2)
        }
    ],

    levels: fromArray [
        "4.1" :-> {
            name: "Brick",
            help: Nothing,
            difficulty: Easy,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Cyan], [Cyan, Orange], [Cyan], [Cyan, Orange], [Cyan]]
        },
        "4.2" :-> {
            name: "Fort",
            help: Nothing,
            difficulty: Hard,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Orange, Cyan], [Orange, Orange], [Orange, Cyan], [Orange, Orange], [Orange, Cyan]]
        }
    ]
}
