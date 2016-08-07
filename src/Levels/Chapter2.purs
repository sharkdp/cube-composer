module Levels.Chapter2 where

import Prelude
import Data.List (List(..), (:), concat, span)
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import Transformer (mapStack, mapReject, replaceMultiple, replaceSingle)
import Types (Chapter, Transformer, Cube(..), Difficulty(..))

-- | concat adjacent lists if they are equal
stackEqualColumns :: Transformer
stackEqualColumns Nil = Nil
stackEqualColumns (Cons s ss) = concat (s:split.init) : stackEqualColumns split.rest
    where split = span (_ == s) ss

chapter2 :: Chapter
chapter2 = {
    name: "Chapter 2",

    transformers: fromArray [
        "replaceYbyB" :> {
            name: "map {Yellow}↦{Brown}",
            function: replaceSingle Yellow Brown
        },
        "replaceYbyBY" :> {
            name: "map {Yellow}↦[{Yellow}{Brown}]",
            function: replaceMultiple Yellow (Brown : Yellow : Nil)
        },
        "replaceBbyOO" :> {
            name: "map {Brown}↦[{Orange}{Orange}]",
            function: replaceMultiple Brown (Orange : Orange : Nil)
        },
        "rejectO" :> {
            name: "map (reject {Orange})",
            function: mapReject Orange
        },
        "stackY" :> {
            name: "map (stack {Yellow})",
            function: mapStack Yellow
        },
        "stackEqualColumns" :> {
            name: "stackEqualColumns",
            function: stackEqualColumns
        }
    ],

    levels: fromArray [
        "2.1" :-> {
            name: "Bricklayer",
            help: Just """This chapter introduces a new function `stackEqualColumns`. It
                          takes <i>adjacent equal columns</i> and stacks them on top of
                          each other. Try it!""",
            difficulty: Easy,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown], [Orange, Orange], [Brown, Brown, Brown], [Orange, Orange], [Brown]]
        },
        "2.2" :-> {
            name: "Gizeh",
            help: Just """You are on your own now...""",
            difficulty: Medium,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown, Brown, Brown, Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown]]
        },
        "2.3" :-> {
            name: "Poseidon",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Brown], [Brown, Brown, Brown, Brown], [Brown], [Brown, Brown]]
        },
        "2.4" :-> {
            name: "Bowl",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Brown]],
            target: [[Orange, Orange, Orange, Orange], [Orange, Orange], [Orange, Orange], [Orange, Orange, Orange, Orange]]
        },
        "2.5" :-> {
            name: "Stamp",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Yellow], [Yellow], [Yellow, Yellow, Yellow, Yellow], [Yellow], [Yellow]]
        }
    ]
}
