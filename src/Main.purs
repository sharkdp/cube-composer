module Main (main) where

import Control.Monad
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
import Level
import Sortable
import Isomer
import DOMHelper

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

withElementById :: forall eff. String
                            -> HTMLDocument
                            -> (HTMLElement -> Eff (dom :: DOM | eff) Unit)
                            -> Eff (dom :: DOM | eff) Unit
withElementById id doc f = getElementById id doc >>= maybe (return unit) f

renderAll isomer = do
    doc <- document globalWindow
    Just ulAvailable <- getElementById "program" doc
    lis <- children ulAvailable
    ids <- traverse (getAttribute "id") lis
    let fs = mapMaybe (getTransformerById chapter) ids
    let steps = allSteps fs level.initial

    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer level.target

    -- Level solved?
    let message = if (maybe false (== (level.target)) (last steps))
                  then "Solved!"
                  else "Target"
    withElementById "message" doc (setInnerHTML message)

    -- DEBUG:
    trace $ "IDs: " ++ show ids
    trace $ "Initial: " ++ show level.initial
    trace "Steps:"
    traverse_ print steps
    trace "---"
    trace $ "Target: " ++ show level.target
    trace ""

replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show (Blue `enumFromTo` Yellow))
        where replaceColor s c = replace (regex (pattern c) rf) (replacement c) s
              rf = parseFlags "g"
              pattern c = "{" ++ c ++ "}"
              replacement c = "<div class=\"rect " ++ c ++ "\"> </div>"

keyPress :: forall eff. IsomerInstance -> HTMLDocument -> DOMEvent -> Eff (dom :: DOM, isomer :: Isomer, trace :: Trace | eff) Unit
keyPress isomer doc event = do
    code <- keyCode event
    case code of
         -- 'g': generate new puzzle
         71 -> return unit
         -- 'r': reset lists
         82 -> do resetUI isomer doc
                  renderAll isomer
         _ -> return unit
    return unit

clickLi :: forall eff. IsomerInstance -> HTMLDocument -> HTMLElement -> DOMEvent -> Eff (dom :: DOM, trace :: Trace, isomer :: Isomer | eff) Unit
clickLi isomer doc li event = do
    parent <- parentElement li >>= getAttribute "id"
    let other = if parent == "available" then "program" else "available"
    withElementById other doc (flip appendChild li)

    renderAll isomer

resetUI :: forall eff. IsomerInstance -> HTMLDocument -> Eff (dom :: DOM | eff) Unit
resetUI isomer doc = do
    let html = mconcat $ map (\t -> "<li id=\"" ++ t.id ++ "\">" ++ replaceColors t.name ++ "</li>") chapter.transformers
    withElementById "available" doc $ \ulAvailable -> do
        setInnerHTML html ulAvailable

        -- set up mouse event handlers
        items <- children ulAvailable
        traverse_ (\li -> addMouseEventListener MouseClickEvent (clickLi isomer doc li) li) items

    withElementById "program" doc (setInnerHTML "")

-- TODO
chapter = case (head allChapters) of Just c -> c
level = case (getLevelById "1") of Just level -> level

main = do
    isomer <- getIsomerInstance "canvas"

    -- install sortable
    doc <- document globalWindow

    Just ulAvailable <- getElementById "available" doc
    Just ulProgram   <- getElementById "program" doc
    installSortable ulAvailable (return unit)
    installSortable ulProgram (renderAll isomer)

    -- set up keyboard event handlers
    addKeyboardEventListener KeydownEvent (keyPress isomer doc) globalWindow

    -- render initial state
    resetUI isomer doc
    renderAll isomer
