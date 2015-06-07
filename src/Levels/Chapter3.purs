module Levels.Chapter3 where

import Data.Array
import Data.Maybe
import Data.Foldable
import Math
import qualified Data.StrMap as SM

import Helper
import Transformer
import Types

toDigit :: Cube -> Number
toDigit Orange = 0
toDigit Brown  = 1

toCube :: Number -> Cube
toCube 0 = Orange
toCube 1 = Brown

toNumber :: Stack -> Number
toNumber w = sum $ zipWith (\f c -> f * toDigit c) [1, 2, 4] w

foreign import digits
    """
    function digits(num) {
      var bin = ((num % 8 + 8) % 8).toString(2);
      bin = '000'.split('').slice( 0, 3 - bin.length ).join('') + bin;
      return bin.split('').map(Number).reverse();
    }
    """ :: Number -> [Number]

toStack :: Number -> Stack
toStack num = map toCube (digits num)

transformNumbers :: (Number -> Number) -> Transformer
transformNumbers f = map (toNumber >>> f >>> toStack)

foreign import even
    """
    function even(num) {
        return num % 2 == 0;
    }
    """:: Number -> Boolean

chapter3 :: Chapter
chapter3 = {
    name: "Chapter 3",

    transformers: SM.fromList [
        "mapAdd1" :> {
            name: "map (+1)",
            function: transformNumbers (+1)
        },
        "mapSub1" :> {
            name: "map (-1)",
            function: transformNumbers (\x -> x - 1)
        },
        "mapMul2" :> {
            name: "map (×2)",
            function: transformNumbers (*2)
        },
        "mapPow2" :> {
            name: "map (^2)",
            function: transformNumbers (`pow` 2)
        },
        "filterEven" :> {
            name: "filter even",
            function: filter (toNumber >>> even)
        }
    ],

    levels: SM.fromList [
        "3.1" :> {
            name: "0b0 .. 0b111",
            help: Nothing,
            difficulty: Easy,
            initial: map toStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toStack [1, 3, 5, 7, 1, 3, 5, 7]
        },
        "3.2" :> {
            name: "Odd..",
            help: Nothing,
            difficulty: Easy,
            initial: map toStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toStack [1, 3, 5, 7]
        },
        "3.3" :> {
            name: "Zero",
            help: Nothing,
            difficulty: Hard,
            initial: map toStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toStack [0, 0, 0, 0, 0, 0, 0, 0]
        },
        "3.4" :> {
            name: "Don't panic",
            help: Nothing,
            difficulty: Hard,
            initial: map toStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toStack [4, 2, 4, 2, 4, 2, 4, 2]
        }
    ]
}
