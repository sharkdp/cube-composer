module Main (main) where

import Control.Monad.Eff
import Data.Array
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.DOM.Simple.Types
import DOM
import Data.Enum
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.String.Regex (regex, parseFlags, replace)
import Debug.Trace

import Types
import Transformer
import Solver
import Sortable
import Isomer

cubeColor :: Cube -> IsomerColor
cubeColor Blue = colorFromRGB 0 160 176
cubeColor Brown = colorFromRGB 106 74 60
cubeColor Red = colorFromRGB 204 51 63
cubeColor Orange = colorFromRGB 235 104 65
cubeColor Yellow = colorFromRGB 237 201 81

traverseWithIndex_ :: forall a b m. (Applicative m) => (Number -> a -> m b) -> [a] -> m Unit
traverseWithIndex_ f xs = sequence_ (zipWith f (0 .. (length xs - 1)) xs)

renderStack :: forall eff. IsomerInstance -> Number -> Number -> Stack -> Eff (isomer :: Isomer | eff) Unit
renderStack isomer y x stack =
    traverseWithIndex_ (renderCube isomer x (-6 * y)) $ map cubeColor stack

renderWall :: forall eff. IsomerInstance -> Number -> Wall -> Eff (isomer :: Isomer | eff) Unit
renderWall isomer y wall =
    traverseWithIndex_ (\x -> renderStack isomer y (length wall - x)) (reverse wall)

renderWalls :: forall eff. IsomerInstance -> [Wall] -> Eff (isomer :: Isomer | eff) Unit
renderWalls isomer walls = do
    setIsomerConfig isomer 40 40 400
    traverseWithIndex_ (renderWall isomer) walls

renderTarget isomer target = do
    setIsomerConfig isomer 22 1200 250
    renderWall isomer 0 target

renderAll isomer = do
    doc <- document globalWindow
    Just ulAvailable <- getElementById "program" doc
    lis <- children ulAvailable
    -- The following line could be simplified to:
    --   ids <- traverse (getAttribute "id") lis
    -- see https://github.com/aktowns/purescript-simple-dom/issues/25
    ids <- sequence $ map (getAttribute "id") lis
    let fs = mapMaybe getTransformerById ids
    let steps = allSteps fs initial

    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer target

    -- DEBUG:
    trace $ "IDs: " ++ show ids
    trace $ "Initial: " ++ show initial
    trace "Steps:"
    traverse_ print steps
    trace "---"
    trace ""

replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show (Blue `enumFromTo` Yellow))
        where replaceColor s c = replace (regex (pattern c) rf) (replacement c) s
              rf = parseFlags "g"
              pattern c = "{" ++ c ++ "}"
              replacement c = "<div class=\"rect " ++ c ++ "\"> </div>"

keyPress :: forall eff. HTMLDocument -> DOMEvent -> Eff (dom :: DOM | eff) Unit
keyPress doc event = do
    code <- keyCode event
    case code of
         -- 'g': generate new puzzle
         71 -> return unit
         -- 'r': reset lists
         82 -> resetUI doc
         _ -> return unit
    return unit

resetUI :: forall eff. HTMLDocument -> Eff (dom :: DOM | eff) Unit
resetUI doc = do
    Just ulAvailable <- getElementById "available" doc
    let html = mconcat $ map (\t -> "<li id=\"" ++ t.id ++ "\">" ++ replaceColors t.name ++ "</li>") transformers
    setInnerHTML html ulAvailable
    Just ulProgram <- getElementById "program" doc
    setInnerHTML "" ulProgram

initial :: Wall
initial = [[Brown], [Orange], [Orange], [Yellow], [Yellow], [Yellow], [Orange], [Orange], [Brown]]

target :: Wall
target = [[Brown], [Yellow], [Yellow], [Yellow], [Brown]]

main = do
    isomer <- getIsomerInstance "canvas"

    -- install sortable
    doc <- document globalWindow

    let dummyHandler = return unit
    getElementById "available" doc >>= (\(Just el) -> installSortable el dummyHandler)
    getElementById "program" doc >>= (\(Just el) -> installSortable el (renderAll isomer))

    -- keyboard events
    addKeyboardEventListener KeydownEvent (keyPress doc) globalWindow

    resetUI doc
    renderAll isomer
