module Main where

import Control.Monad.Eff
import Data.Array
import Data.DOM.Simple.Window
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.String.Regex (regex, parseFlags, replace)
import Debug.Trace

import Types
import Transformer
import Sortable
import Isomer

initial :: Wall
initial = [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]]

target :: Wall
target = [[Yellow], [Yellow, Yellow], [Yellow]]

cubeColor :: Cube -> IsomerColor
cubeColor Blue = colorFromRGB 0 160 176
cubeColor Brown = colorFromRGB 106 74 60
cubeColor Red = colorFromRGB 204 51 63
cubeColor Orange = colorFromRGB 235 104 65
cubeColor Yellow = colorFromRGB 237 201 81

mapIndexed :: forall a b. (Number -> a -> b) -> [a] -> [b]
mapIndexed f xs = zipWith f (0 .. (length xs - 1)) xs

renderStack :: forall eff. IsomerInstance -> Number -> Number -> Stack -> Eff (isomer :: Isomer | eff) Unit
renderStack isomer y x stack =
    sequence_ $ mapIndexed (renderCube isomer x (-6 * y)) $ map cubeColor stack

renderWall :: forall eff. IsomerInstance -> Number -> Wall -> Eff (isomer :: Isomer | eff) Unit
renderWall isomer y wall =
    sequence_ $ mapIndexed (\x stack -> renderStack isomer y (length wall - x) stack) (reverse wall)

renderWalls :: forall eff. IsomerInstance -> [Wall] -> Eff (isomer :: Isomer | eff) Unit
renderWalls isomer walls = do
    setIsomerConfig isomer 40 40 400
    sequence_ $ mapIndexed (renderWall isomer) walls

renderTarget isomer target = do
    setIsomerConfig isomer 22 1200 250
    renderWall isomer 0 target

sortHandler isomer = do
    doc <- document globalWindow
    Just ulAvailable <- getElementById "program" doc
    lis <- children ulAvailable
    ids <- sequence $ map (getAttribute "id") lis
    let fs = catMaybes $ map getTransformerById ids

    trace $ "IDs: " ++ show ids
    trace $ "Initial: " ++ show initial
    trace "Steps:"
    let steps = allSteps fs initial
    traverse_ (show >>> trace) steps
    trace "---"
    trace ""

    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer target

getTransformerById :: String -> Maybe Transformer
getTransformerById id = (\x -> x.function) <$> (head $ filter (\t -> t.id == id) transformers)

type TransformerRecord = {
    id :: String,
    name :: String,
    function :: Transformer
}

transformers :: [TransformerRecord]
transformers = [
    {
        id: "stackEqual",
        name: "stackEqual",
        function: tStackEqual
    }, {
        id: "flatten",
        name: "flatten",
        function: tFlatten
    }, {
        id: "replaceYB",
        name: "replace {Yellow} ↦ {Brown}",
        function: tReplace Yellow Brown
    }, {
        id: "replaceYBB",
        name: "replace {Yellow} ↦ {Yellow}{Brown}",
        function: tReplaceMultiple Yellow [Yellow, Brown]
    }, {
        id: "replaceBBBB",
        name: "replace {Brown} ↦ {Brown}{Brown}{Brown}",
        function: tReplaceMultiple Brown [Brown, Brown, Brown]
    }, {
        id: "replaceBOO",
        name: "replace {Brown} ↦ {Orange}{Orange}",
        function: tReplaceMultiple Brown [Orange, Orange]
    }, {
        id: "pushY",
        name: "push {Yellow}",
        function: map (flip snoc Yellow)
    }, {
        id: "tail",
        name: "tail",
        function: tTail
    }
]

replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show [Brown, Orange, Red, Yellow, Blue]) -- TODO
      where replaceColor s c = replace (regex (pattern c) rf) (replacement c) s
            rf = parseFlags "g"
            pattern c = "{" ++ c ++ "}"
            replacement c = "<div class=\"rect " ++ c ++ "\"> </div>"

main = do
    isomer <- getIsomerInstance "canvas"

    -- install sortable
    doc <- document globalWindow

    let dummyHandler = return unit
    getElementById "available" doc >>= (\(Just el) -> installSortable el dummyHandler)
    getElementById "program" doc >>= (\(Just el) -> installSortable el (sortHandler isomer))

    Just ulAvailable <- getElementById "available" doc
    let html = mconcat $ map (\t -> "<li id=\"" ++ t.id ++ "\">" ++ replaceColors t.name ++ "</li>") transformers
    setInnerHTML html ulAvailable

    -- initial rendering
    sortHandler isomer
