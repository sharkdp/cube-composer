module Levels.Chapter1 where

import Prelude
import Data.List
import Data.Maybe

import Helper
import Transformer
import Types

-- | concat adjacent lists if they are equal
stackEqual :: Transformer
stackEqual Nil = Nil
stackEqual (Cons s ss) = concat (s:split.init) : stackEqual split.rest
    where split = span (== s) ss

chapter1 :: Chapter
chapter1 = {
    name: "Chapter 1",

    transformers: fromArray [
        "replaceYbyB" :> {
            name: "map {Yellow}↦{Brown}",
            function: replaceSingle Yellow Brown
        },
        "replaceYbyBY" :> {
            name: "map {Yellow}↦{Brown}{Yellow}",
            function: replaceMultiple Yellow (Brown : Yellow : Nil)
        },
        "replaceBbyOO" :> {
            name: "map {Brown}↦{Orange}{Orange}",
            function: replaceMultiple Brown (Orange : Orange : Nil)
        },
        "rejectO" :> {
            name: "map (reject {Orange})",
            function: mapReject Orange
        },
        "pushY" :> {
            name: "map (stack {Yellow})",
            function: mapStack Yellow
        },
        "stackEqual" :> {
            name: "stackEqualColumns",
            function: stackEqual
        }
    ],

    levels: fromArray [
        "1.1" :-> {
            name: "Dismiss",
            help: Just """In this game, your goal is to create a sequence of functions which
                          transforms the stack of cubes into the desired pattern shown above.
                          To solve this level, add the `rejectO` function to your program by
                          dragging it to the list on the right side.""",
            difficulty: Easy,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown], [Yellow], [Yellow], [Yellow], [Brown]]
        },
        "1.2" :-> {
            name: "Gizeh",
            help: Nothing,
            difficulty: Medium,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown, Brown, Brown, Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown]]
        },
        "1.3" :-> {
            name: "Poseidon",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Brown], [Brown, Brown, Brown, Brown], [Brown], [Brown, Brown]]
        },
        "1.4" :-> {
            name: "Bowl",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Brown]],
            target: [[Orange, Orange, Orange, Orange], [Orange, Orange], [Orange, Orange], [Orange, Orange, Orange, Orange]]
        },
        "1.5" :-> {
            name: "Stamp",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Yellow], [Yellow], [Yellow, Yellow, Yellow, Yellow], [Yellow], [Yellow]]
        }
    ]
}
