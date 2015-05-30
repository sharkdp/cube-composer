module Levels.Chapter1 where

import Data.Array
import qualified Data.StrMap as SM

import Helper
import Transformer
import Types

chapter1 :: Chapter
chapter1 = {
    name: "Chapter 1",

    transformers: SM.fromList [
        "stackEqual" :> {
            name: "stackEqual",
            function: tStackEqual
        },
        "replaceYbyB" :> {
            name: "map({Yellow} ↦ {Brown})",
            function: tReplace Yellow Brown
        },
        "replaceYbyBY" :> {
            name: "map({Yellow} ↦ {Brown}{Yellow})",
            function: tReplaceMultiple Yellow [Brown, Yellow]
        },
        "replaceBbyOO" :> {
            name: "map({Brown} ↦ {Orange}{Orange})",
            function: tReplaceMultiple Brown [Orange, Orange]
        },
        "rejectO" :> {
            name: "reject({Orange})",
            function: map (reject (== Orange)) >>> tClearEmpty
        },
        "rejectB" :> {
            name: "reject({Brown})",
            function: map (reject (== Brown)) >>> tClearEmpty
        },
        "pushY" :> {
            name: "map(push({Yellow}))",
            function: map (`snoc` Yellow)
        }
    ],

    levels: SM.fromList [
        "1.1" :> {
            name: "Dismiss",
            difficulty: Easy,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown], [Yellow], [Yellow], [Yellow], [Brown]]
        },
        "1.2" :> {
            name: "Gizeh",
            difficulty: Medium,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown, Brown, Brown, Brown, Brown], [Orange, Brown, Orange, Brown], [Brown, Brown]]
        },
        "1.3" :> {
            name: "Poseidon",
            difficulty: Medium,
            initial: [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]],
            target: [[Brown, Brown], [Brown], [Brown, Brown, Brown, Brown], [Brown], [Brown, Brown]]
        },
        "1.4" :> {
            name: "Bowl",
            difficulty: Hard,
            initial: [[Brown], [Orange], [Orange], [Brown]],
            target: [[Orange, Orange, Orange, Orange], [Orange, Orange], [Orange, Orange], [Orange, Orange, Orange, Orange]]
        }
    ]
}
