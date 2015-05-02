module Main (main) where

import Control.Monad
import Control.Monad.Eff
import DOM
import Data.Array
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.Enum
import Data.Foldable
import Data.Maybe
import Data.String.Regex (regex, parseFlags, replace)
import Data.Traversable
import Debug.Trace
import qualified Data.StrMap as SM

import DOMHelper
import Isomer
import Level
import Sortable
import Storage
import Transformer
import Types

-- | RGB codes for the abstract colors
cubeColor :: Cube -> IsomerColor
cubeColor Blue = colorFromRGB 0 160 176
cubeColor Brown = colorFromRGB 106 74 60
cubeColor Red = colorFromRGB 204 51 63
cubeColor Orange = colorFromRGB 235 104 65
cubeColor Yellow = colorFromRGB 237 201 81

-- | Like traverse_, but specialized for arrays with an additional parameter (index of element)
traverseWithIndex_ :: forall a b m. (Applicative m) => (Number -> a -> m b) -> [a] -> m Unit
traverseWithIndex_ f xs = sequence_ (zipWith f (0 .. (length xs - 1)) xs)

-- | Render a single stack of cubes
renderStack :: forall eff. IsomerInstance -> Number -> Number -> Stack -> Eff (isomer :: Isomer | eff) Unit
renderStack isomer y x stack =
    traverseWithIndex_ (renderCube isomer x (-6 * y)) $ map cubeColor stack

-- | Render a wall (multiple stacks)
renderWall :: forall eff. IsomerInstance -> Number -> Wall -> Eff (isomer :: Isomer | eff) Unit
renderWall isomer y wall =
    traverseWithIndex_ (\x -> renderStack isomer y (length wall - x)) (reverse wall)

-- | Render a series of walls
renderWalls :: forall eff. IsomerInstance -> [Wall] -> Eff (isomer :: Isomer | eff) Unit
renderWalls isomer walls = do
    setIsomerConfig isomer 40 40 400
    traverseWithIndex_ (renderWall isomer) walls

-- | Render the target shape
renderTarget isomer target = do
    setIsomerConfig isomer 22 1200 250
    renderWall isomer 0 target

-- | Render all UI components, DOM and canvas
render = do
    doc <- document globalWindow
    isomer <- getIsomerInstance "canvas"

    -- Retrieve current 'program'
    Just ulAvailable <- getElementById "program" doc
    lis <- children ulAvailable
    ids <- traverse (getAttribute "id") lis
    let fs = mapMaybe (getTransformerById chapter) ids
    let steps = allSteps fs level.initial

    -- On-canvas rendering
    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer level.target

    -- DOM 'rendering'
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

-- | Replace color placeholders in the transformer description by colored rectangular divs
replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show (Blue `enumFromTo` Yellow))
        where replaceColor s c = replace (regex (pattern c) rf) (replacement c) s
              rf = parseFlags "g"
              pattern c = "{" ++ c ++ "}"
              replacement c = "<div class=\"rect " ++ c ++ "\"> </div>"

-- | General key press handler
keyPress :: forall eff. DOMEvent -> Eff (dom :: DOM, isomer :: Isomer, trace :: Trace | eff) Unit
keyPress event = do
    doc <- document globalWindow
    code <- keyCode event
    case code of
         -- 'g': generate new puzzle
         71 -> return unit
         -- 'r': reset lists
         82 -> do resetUI
                  render
         _ -> return unit
    return unit

-- | Click handler for the <li> elements (transformers)
clickLi :: forall eff. HTMLElement -> DOMEvent -> Eff (dom :: DOM, trace :: Trace, isomer :: Isomer | eff) Unit
clickLi li event = do
    doc <- document globalWindow
    parent <- parentElement li >>= getAttribute "id"
    let other = if parent == "available" then "program" else "available"
    withElementById other doc (flip appendChild li)

    render

appendLiElement :: forall eff. HTMLElement -> String -> TransformerRecord -> Eff (dom :: DOM | eff) Unit
appendLiElement ul id t = do
    doc <- document globalWindow
    li <- createElement doc "li"
    setAttribute "id" id li
    setInnerHTML (replaceColors t.name) li
    appendChild ul li

-- | Set up or reset the whole UI
resetUI :: forall eff. Eff (dom :: DOM | eff) Unit
resetUI = do
    doc <- document globalWindow
    withElementById "available" doc $ \ulAvailable -> do
        -- create li elements for transformers
        _ <- SM.foldM (\z -> appendLiElement ulAvailable) unit chapter.transformers

        -- set up mouse event handlers
        items <- children ulAvailable
        traverse_ (\li -> addMouseEventListener MouseClickEvent (clickLi li) li) items

    withElementById "program" doc (setInnerHTML "")

-- TODO
chapter = case (head allChapters) of Just c -> c
level = case (getLevelById "1") of Just level -> level

-- | Initial game state for first-time visitors
initialGS :: GameState
initialGS = { currentLevel: "1", levelState: SM.empty }

main = do
    doc <- document globalWindow

    -- install sortable
    Just ulAvailable <- getElementById "available" doc
    Just ulProgram   <- getElementById "program" doc
    installSortable ulAvailable (return unit)
    installSortable ulProgram render

    -- set up keyboard event handlers
    addKeyboardEventListener KeydownEvent keyPress globalWindow

    -- render initial state
    resetUI
    render
