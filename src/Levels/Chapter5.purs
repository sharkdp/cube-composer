module Levels.Chapter5 where

import Prelude
import Data.Foldable (sum)
import Data.Int (even)
import Data.Int.Bits ((.&.))
import Data.List (List(..), filter, fromFoldable, (:), zipWith)
import Data.Maybe (Maybe(..))

import Helper (AStack, fromArray, (:->), (:>))
import Types (Chapter, Transformer, Stack, Cube(..), Difficulty(..))

toDigit :: Cube -> Int
toDigit Orange = 0
toDigit _      = 1

toCube :: Int -> Cube
toCube 0 = Orange
toCube _ = Brown

toInt :: Stack -> Int
toInt w = sum $ zipWith (\f c -> f * toDigit c) (1 : 2 : 4 : Nil) w

-- | Get the (first three bits of the) binary representation of a number
digits :: Int -> Array Int
digits n = map bit [1, 2, 4]
    where bit m = if n .&. m == m then 1 else 0

toAStack :: Int -> AStack
toAStack num = map toCube (digits num)

toStack :: Int -> Stack
toStack = fromFoldable <<< toAStack

mapNumbers :: (Int -> Int) -> Transformer
mapNumbers f = map (toInt >>> f >>> toStack)

chapter5 :: Chapter
chapter5 = {
    name: "Chapter 5",

    transformers: fromArray [
        "mapAdd1" :> {
            name: "map (+1)",
            function: mapNumbers (_ + 1)
        },
        "mapSub1" :> {
            name: "map (-1)",
            function: mapNumbers (_ - 1)
        },
        "mapMul2" :> {
            name: "map (×2)",
            function: mapNumbers (_ * 2)
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
        "5.1" :-> {
            name: "0b0 .. 0b111",
            help: Just """What could be the meaning of the title <code>0b0 .. 0b111</code>?
                          Read from top to bottom. Calculate modulo eight.""",
            difficulty: Medium,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [1, 3, 5, 7, 1, 3, 5, 7]
        },
        "5.2" :-> {
            name: "Odd..",
            help: Nothing,
            difficulty: Easy,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [1, 3, 5, 7]
        },
        "5.3" :-> {
            name: "Zero",
            help: Nothing,
            difficulty: Hard,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [0, 0, 0, 0, 0, 0, 0, 0]
        },
        "5.4" :-> {
            name: "Don't panic",
            help: Just """This is the last level ... for now. But you can design your own puzzles!
                          See the <a href="https://github.com/sharkdp/cube-composer">GitHub repository</a>
                          for more information. I hope you enjoyed the game.""",
            difficulty: Hard,
            initial: map toAStack [0, 1, 2, 3, 4, 5, 6, 7],
            target: map toAStack [4, 2, 4, 2, 4, 2, 4, 2]
        }
    ]
}
