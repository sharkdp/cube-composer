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

-- | Get program (list of transformer ids) of the active level
getCurrentIds :: GameState -> [TransformerId]
getCurrentIds gs = case (SM.lookup gs.currentLevel gs.levelState) of
                       Just ids -> ids
                       Nothing -> []

-- | Traverse a StrMap while performing monadic side effects
traverseWithKey_ :: forall a m. (Monad m) => (String -> a -> m Unit) -> SM.StrMap a -> m Unit
traverseWithKey_ f sm = SM.foldM (const f) unit sm

-- | Render all UI components, DOM and canvas
render :: Boolean -> GameState -> _
render setupUI gs = do
    doc <- document globalWindow
    isomer <- getIsomerInstance "canvas"

    let level = getLevel gs.currentLevel
        chapter = getChapter gs.currentLevel
        tids = getCurrentIds gs

    -- Set up UI, only if new level is loaded
    when setupUI $ do
        Just ulAvailable <- getElementById "available" doc
        Just ulProgram <- getElementById "program" doc

        setInnerHTML "" ulAvailable
        setInnerHTML "" ulProgram

        let unused = foldl (flip SM.delete) chapter.transformers tids
        let active = foldl (\sm id -> SM.insert id (getTransformerRecord chapter id) sm) SM.empty tids

        -- create li elements for transformers
        traverseWithKey_ (appendTransformerElement ulAvailable) unused
        traverseWithKey_ (appendTransformerElement ulProgram) active

        -- set up mouse event handlers
        let installClickHandler li = addMouseEventListener MouseClickEvent (clickLi li) li
        children ulAvailable >>= traverse_ installClickHandler

        withElementById "levels" doc $ \selectLevel -> do
            setInnerHTML "levels" selectLevel
            traverseWithKey_ (appendLevelElement selectLevel gs.currentLevel) allLevels

    let transformers = map (getTransformer chapter) tids
    let steps = allSteps transformers level.initial

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
    trace $ "Program: " ++ show tids
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
keyPress :: forall eff. DOMEvent -> _
keyPress event = do
    doc <- document globalWindow
    code <- keyCode event
    case code of
         -- 'r': reset lists
         82 -> modifyGameStateAndRender true $ \gs ->
                   gs { levelState = SM.insert gs.currentLevel [] gs.levelState }
         _ -> return unit
    return unit

-- | Click handler for the <li> elements (transformers)
clickLi :: forall eff. HTMLElement
        -> DOMEvent
        -> Eff (dom :: DOM, trace :: Trace, isomer :: Isomer, storage :: Storage | eff) Unit
clickLi liEl event = do
    newId <- getAttribute "id" liEl
    modifyGameStateAndRender true (modify newId)

    where modify new gs = let program = getCurrentIds gs
                              program' = program `snoc` new
                          in  gs { levelState = SM.insert gs.currentLevel program' gs.levelState }

-- | Add a li-element corresponding to the given Transformer
appendTransformerElement :: forall eff. HTMLElement -> String -> TransformerRecord -> Eff (dom :: DOM | eff) Unit
appendTransformerElement ul id t = do
    doc <- document globalWindow
    li <- createElement doc "li"
    setAttribute "id" id li
    setInnerHTML (replaceColors t.name) li
    appendChild ul li

-- | Add an option-element corresponding to the given Level
appendLevelElement :: forall eff. HTMLElement -> String -> LevelId -> Level -> Eff (dom :: DOM | eff) Unit
appendLevelElement select currentId id l = do
    let chapter = getChapter id
    doc <- document globalWindow
    option <- createElement doc "option"
    setAttribute "value" id option
    when (currentId == id) $
        setAttribute "selected" "selected" option
    setTextContent (chapter.name ++ ": " ++ l.name ++ " (" ++ show l.difficulty ++ ")") option
    appendChild select option

-- | Initial game state for first-time visitors
initialGS :: GameState
initialGS = { currentLevel: "1_1", levelState: SM.empty }

-- | Load game, modify and store the game state. Render the new state
modifyGameStateAndRender :: forall eff.  Boolean
                         -> (GameState -> GameState)
                         -> Eff (storage :: Storage, dom :: DOM, isomer :: Isomer, trace :: Trace | eff) Unit
modifyGameStateAndRender setupUI modifyGS = do
    -- Load old game state from local storage
    mgs <- loadGameState
    let gs = fromMaybe initialGS mgs

    -- Modify by supplied function
    let gs' = modifyGS gs

    -- Render the new state and save back to local storage
    render setupUI gs'
    saveGameState gs'

-- | Event handler for a level change
levelChangeHandler :: forall eff. HTMLElement -> _
levelChangeHandler selectLevel = do
    levelId <- getSelectedValue selectLevel

    modifyGameStateAndRender true $ \gs ->
        gs { currentLevel = levelId }

-- | Event handler for a 'reprogram' (new instruction, re-ordering, ..)
reprogramHandler = do
    doc <- document globalWindow

    -- Retrieve current 'program'
    Just ulAvailable <- getElementById "program" doc
    lis <- children ulAvailable
    program <- traverse (getAttribute "id") lis

    modifyGameStateAndRender false $ \gs ->
        gs { levelState = SM.insert gs.currentLevel program gs.levelState }

main = do
    doc <- document globalWindow

    -- install sortable
    Just ulAvailable <- getElementById "available" doc
    Just ulProgram   <- getElementById "program" doc
    installSortable ulAvailable (return unit)
    installSortable ulProgram reprogramHandler

    -- set up keyboard event handlers
    addKeyboardEventListener KeydownEvent keyPress globalWindow

    -- set up 'change' handler for the level selector
    withElementById "levels" doc $ \selectLevel ->
        addChangeEventListener levelChangeHandler selectLevel

    -- load game state (or set initial one)
    gs <- fromMaybe initialGS <$> loadGameState
    saveGameState gs

    -- render initial state
    render true gs
