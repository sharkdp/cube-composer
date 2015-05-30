module Levels.Chapter4 where

import Data.Array
import qualified Data.StrMap as SM

import Helper
import Transformer
import Types

cxToC :: Stack -> Stack
cxToC [] = []
cxToC [c] = [c]
cxToC (c1:c2:cs) = if c1 == Cyan
                   then c2 : cxToC cs
                   else c1 : cxToC (c2 : cs)

ooToC :: Stack -> Stack
ooToC [] = []
ooToC [c] = [c]
ooToC (c1:c2:cs) = if c1 == Orange && c2 == Orange
                   then Cyan : ooToC cs
                   else c1 : ooToC (c2 : cs)

chapter4 :: Chapter
chapter4 = {
    name: "Chapter 4",

    transformers: SM.fromList [
        "mapXtoOX" :> {
            name: "map {X}↦{Orange}{X}",
            function: map (concatMap (\c -> [Orange, c]))
        },
        "mapCXtoC" :> {
            name: "map {Cyan}{X}↦{Cyan}",
            function: map cxToC
        },
        "mapOOtoC" :> {
            name: "map {Orange}{Orange}↦{Cyan}",
            function: map ooToC
        },
        "mapCtoO" :> {
            name: "map {Cyan}↦{Orange}",
            function: tReplace Cyan Orange
        },
        "rejectSizeG2" :> {
            name: "reject(size > 2)",
            function: reject (\x -> length x > 2)
        }
    ],

    levels: SM.fromList [
        "4.1" :> {
            name: "Brick",
            difficulty: Easy,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Cyan], [Cyan, Orange], [Cyan], [Cyan, Orange], [Cyan]]
        },
        "4.2" :> {
            name: "Fort",
            difficulty: Hard,
            initial: [[Cyan, Orange], [Cyan, Cyan, Orange], [Orange, Orange], [Cyan, Cyan, Orange], [Cyan, Orange]],
            target: [[Orange, Cyan], [Orange, Orange], [Orange, Cyan], [Orange, Orange], [Orange, Cyan]]
        }

    ]
}
