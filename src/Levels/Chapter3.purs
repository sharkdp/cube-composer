module Levels.Chapter3 where

import Prelude
import Data.Foldable
import Data.Int (even)
import Data.Int.Bits ((.&.))
import Data.List
import Data.Maybe
import Math
import qualified Data.StrMap as SM

import Helper
import Transformer
import Types

toDigit :: Cube -> Int
toDigit Orange = 0
toDigit Brown  = 1

toCube :: Int -> Cube
toCube 0 = Orange
toCube 1 = Brown

toInt :: Stack -> Int
toInt w = sum $ zipWith (\f c -> f * toDigit c) (1 : 2 : 4 : Nil) w

-- | Get the (first three bits of the) binary representation of a number
digits :: Int -> Array Int
digits n = map bit [1, 2, 4]
    where bit m = if n .&. m == m then 1 else 0

toAStack :: Int -> AStack
toAStack num = map toCube (digits num)

toStack :: Int -> Stack
toStack = toList <<< toAStack

mapNumbers :: (Int -> Int) -> Transformer
mapNumbers f = map (toInt >>> f >>> toStack)

chapter3 :: Chapter
chapter3 = {
    name: "Chapter 3",

    transformers: fromArray [
        "mapAdd1" :> {
            name: "map (+1)",
            function: mapNumbers (+1)
        },
        "mapSub1" :> {
            name: "map (-1)",
            function: mapNumbers (\x -> x - 1)
        },
        "mapMul2" :> {
            name: "map (×2)",
            function: mapNumbers (*2)
        },
        "mapPow2" :> {
            name: "map (^2)",
            function: mapNumbers (\x -> x * x)
        },
        "filterEven" :> {
            name: "filter even",
            function: filter (toInt >>> even)
        }
    ],

    levels: fromArray [
        "3.1" :-> {
            name: "0b0 .. 0b111",
            help: Just """What could be the meaning of the title <code>0b0 .. 0b111</code>? Calculate modulo eight.""",
            difficulty: Medium,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [1, 3, 5, 7, 1, 3, 5, 7]
        },
        "3.2" :-> {
            name: "Odd..",
            help: Nothing,
            difficulty: Easy,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [1, 3, 5, 7]
        },
        "3.3" :-> {
            name: "Zero",
            help: Nothing,
            difficulty: Hard,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [0, 0, 0, 0, 0, 0, 0, 0]
        },
        "3.4" :-> {
            name: "Don't panic",
            help: Just """Life, the Universe and Everything...""",
            difficulty: Hard,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [4, 2, 4, 2, 4, 2, 4, 2]
        }
    ]
}
