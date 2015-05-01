module Levels.Chapter1 where

import Data.Array

import Types
import Transformer

chapter1 :: Chapter
chapter1 = {
    id:   "chapter1",
    name: "Chapter 1",
    transformers: [
        {
            id: "stackEqual",
            name: "stackEqual",
            function: tStackEqual
        }, {
            id: "mapClone",
            name: "map({X} ↦ {X}{X})",
            function: map $ concatMap (\x -> [x, x])
        }, {
            id: "flatten",
            name: "flatten",
            function: tFlatten
        }, {
            id: "replaceYbyB",
            name: "map({Yellow} ↦ {Brown})",
            function: tReplace Yellow Brown
        }, {
            id: "replaceYbyBY",
            name: "map({Yellow} ↦ {Brown}{Yellow})",
            function: tReplaceMultiple Yellow [Brown, Yellow]
        }, {
            id: "replaceBbyBBB",
            name: "map({Brown} ↦ {Brown}{Brown}{Brown})",
            function: tReplaceMultiple Brown [Brown, Brown, Brown]
        }, {
            id: "replaceBbyOO",
            name: "map({Brown} ↦ {Orange}{Orange})",
            function: tReplaceMultiple Brown [Orange, Orange]
        }, {
            id: "rejectO",
            name: "reject({Orange})",
            function: map (reject (== Orange)) >>> tClearEmpty
        }, {
            id: "pushY",
            name: "map(push({Yellow}))",
            function: map (`snoc` Yellow)
        }, {
            id: "tail",
            name: "map(tail)",
            function: tTail
        }
    ],
    levels: [
        {
            id: "1",
            name: "1",
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown], [Yellow], [Yellow], [Yellow], [Brown]]
        }
    ]
}
