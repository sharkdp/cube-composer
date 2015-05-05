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
        "rejectSizeGE2" :> {
            name: "reject(size > 2)",
            function: reject (\x -> length x > 2)
        },
        "mapYtoYR" :> {
            name: "map({Yellow} ↦ {Yellow}{Red})",
            function: tReplaceMultiple Yellow [Yellow, Red]
        },
        "mapBtoRB" :> {
            name: "map({Blue} ↦ {Red}{Blue})",
            function: tReplaceMultiple Blue [Red, Blue]
        },
        "rejectY" :> {
            name: "reject({Yellow})",
            function: map (reject (== Yellow)) >>> tClearEmpty
        },
        "rejectB" :> {
            name: "reject({Blue})",
            function: map (reject (== Blue)) >>> tClearEmpty
        },
        "filterContainsR" :> {
            name: "filter(contains({Red}))",
            function: filter (\stack -> Red `elemIndex` stack /= -1) >>> tClearEmpty
        },
        "mapPushR" :> {
            name: "map(push({Red}))",
            function: map (`snoc` Red)
        },
        "pushB" :> {
            name: "push({Blue})",
            function: flip snoc [Blue]
        },
        "mapReverse" :> {
            name: "map(reverse)",
            function: map reverse
        }
    ],

    levels: SM.fromList [
        "2_1" :> {
            name: "Mercury",
            initial: [[Red, Red], [Red, Yellow], [Blue, Yellow], [Blue, Blue]],
            target: [[Red, Red]]
        }
    ]
}
