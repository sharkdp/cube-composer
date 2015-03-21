module Main where

{-- import Data.DOM.Simple.Window --}
{-- import Data.DOM.Simple.Document --}
{-- import Data.DOM.Simple.Element --}
import Control.Monad.Eff
import Data.Array
import Data.Foldable
import Data.Maybe
import Debug.Trace

import Types
import Transformer
import Isomer

ts :: [Transformer]
ts = [
       tStackEqual
     , tReplace Red Blue
     , tTail
     ]

initial :: Wall
initial = [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]]

cubeColor :: Cube -> IsomerColor
cubeColor Blue = colorFromRGB 0 160 176
cubeColor Brown = colorFromRGB 106 74 60
cubeColor Red = colorFromRGB 204 51 63
cubeColor Orange = colorFromRGB 235 104 65
cubeColor Yellow = colorFromRGB 237 201 81

mapIndexed :: forall a b. (a -> Number -> b) -> [a] -> [b]
mapIndexed f xs = zipWith f xs (0 .. (length xs - 1))

renderStack :: forall eff. IsomerInstance -> Number -> Number -> Stack -> Eff (isomer :: Isomer | eff) Unit
renderStack isomer x y stack =
    sequence_ $ mapIndexed (\cube z -> renderCube isomer x (-6 * y) z (cubeColor cube)) stack

renderWall :: forall eff. IsomerInstance -> Number -> Wall -> Eff (isomer :: Isomer | eff) Unit
renderWall isomer y wall =
    sequence_ $ mapIndexed (\stack x -> renderStack isomer (length wall - x) y stack) (reverse wall)

renderWalls :: forall eff. IsomerInstance -> [Wall] -> Eff (isomer :: Isomer | eff) Unit
renderWalls isomer walls =
    sequence_ $ mapIndexed (\wall y -> renderWall isomer y wall) walls

main = do
    trace $ "Initial: " ++ (show initial)
    trace "Steps:"
    let steps = allSteps ts initial
    traverse_ (show >>> trace) steps
    trace "---"
    trace ""

    {-- doc <- document globalWindow --}
    {-- Just el <- querySelector "#targetshape" doc --}
    {-- setInnerHTML "lkasjd" el --}

    isomer <- getIsomerInstance "canvas"
    setIsomerConfig isomer 40 40 400

    {-- renderWall isomer initial --}
    renderWalls isomer steps
