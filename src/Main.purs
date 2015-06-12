module Main (main) where

import Prelude
import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.Enum
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.String.Regex (regex, parseFlags, replace)
import Data.Traversable
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
cubeColor Cyan = colorFromRGB 0 160 176
cubeColor Brown = colorFromRGB 106 74 60
cubeColor Red = colorFromRGB 204 51 63
cubeColor Orange = colorFromRGB 235 104 65
cubeColor Yellow = colorFromRGB 237 201 81

-- | Like traverse_, but the function also takes an index parameter
traverseWithIndex_ :: forall a b m. (Applicative m) => (Int -> a -> m b) -> (List a) -> m Unit
traverseWithIndex_ f xs = go xs 0
    where go Nil _ = return unit
          go (Cons x xs) i = f i x *> go xs (i + 1)

-- | Spacing between two walls
spacing :: Number
spacing = 5.5

-- | Render a single stack of cubes
renderStack :: forall eff. IsomerInstance -> Number -> Number -> Stack -> Eff (isomer :: Isomer | eff) Unit
renderStack isomer y x stack =
    traverseWithIndex_ (\z -> renderCube isomer x (-spacing * y) (toNumber z)) $ map cubeColor stack

-- | Render a wall (multiple stacks)
renderWall :: forall eff. IsomerInstance -> Number -> Wall -> Eff (isomer :: Isomer | eff) Unit
renderWall isomer y Nil =
    -- Render a gray placeholder for the empty wall
    renderBlock isomer 1.0 (-spacing * y) 0.0 5.0 0.9 0.1 (colorFromRGB 100 100 100)
renderWall isomer y wall =
    traverseWithIndex_ (\x -> renderStack isomer y (toNumber (length wall - x))) (reverse wall)

-- | Render a series of walls
renderWalls :: forall eff. IsomerInstance -> (List Wall) -> Eff (isomer :: Isomer | eff) Unit
renderWalls isomer walls = do
    setIsomerConfig isomer 40.0 40.0 400.0
    traverseWithIndex_ (\y -> renderWall isomer (toNumber y)) walls

-- | Render the target shape
renderTarget isomer target = do
    setIsomerConfig isomer 30.0 1280.0 550.0
    renderWall isomer 0.0 target

-- | Get program (list of transformer ids) of the active level
getCurrentIds :: GameState -> (List TransformerId)
getCurrentIds gs = case (SM.lookup gs.currentLevel gs.levelState) of
                       Just ids -> ids
                       Nothing -> Nil

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
        children ulProgram >>=   traverse_ installClickHandler

        withElementById "levels" doc $ \selectLevel -> do
            setInnerHTML "levels" selectLevel
            traverse_ (appendLevelElement selectLevel gs.currentLevel) allLevelIds

    let transformers = map (getTransformer chapter) tids
    let steps = allSteps transformers level.initial

    -- On-canvas rendering
    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer level.target

    -- DOM 'rendering'
    let message = if (maybe false (== (level.target)) (last steps))
                  then "<span class=\"animated flash\">Solved!</span>"
                  else ""
    withElementById "message" doc (setInnerHTML message)
    let helpHTML = maybe "" (replaceColors <<< replaceTransformers chapter) level.help
    withElementById "help" doc (setInnerHTML helpHTML)

    -- DEBUG:
    log $ "Program: " ++ show tids
    log $ "Initial: " ++ show level.initial
    log "Steps:"
    traverse_ print steps
    log "---"
    log $ "Target: " ++ show level.target
    log ""

-- | Replace color placeholders in the transformer description by colored rectangular divs
replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show (toList $ Cyan `enumFromTo` Yellow))
        where replaceColor s c = replace (regex (pattern c) rf) (replacement c) s
              rf = parseFlags "g"
              pattern c = "{" ++ c ++ "}"
              replacement c = "<div class=\"cube " ++ c ++ "\"> </div>"

-- | Replace transformer names by boxes
replaceTransformers :: Chapter -> String -> String
replaceTransformers ch initial = SM.fold replaceT initial ch.transformers
    where replaceT text id tr = replace (regex (pattern id) rf) (replacement tr) text
          rf = parseFlags "g"
          pattern id = "`" ++ id ++ "`"
          replacement tr = "<span class=\"transformer\">" ++ tr.name ++ "</span>"

-- | Clear all functions for the current level
resetLevel = modifyGameStateAndRender true mod
    where mod gs = gs { levelState = SM.insert gs.currentLevel Nil gs.levelState }

-- | General key press handler
keyPress :: forall eff. DOMEvent -> _
keyPress event = do
    doc <- document globalWindow
    code <- keyCode event
    case code of
         -- 'r': reset lists
         82 -> do
                   ctrlPressed <- ctrlKey event
                   when (not ctrlPressed) resetLevel
         _ -> return unit
    return unit

-- | Click handler for the <li> elements (transformers)
clickLi :: forall eff. HTMLElement
        -> DOMEvent
        -> Eff (dom :: DOM, console :: CONSOLE, isomer :: Isomer, storage :: Storage | eff) Unit
clickLi liEl event = do
    newId <- getAttribute "id" liEl
    ulId <- parentElement liEl >>= getAttribute "id"
    modifyGameStateAndRender true (modify ulId newId)

    where modify ulId clicked gs = let program = getCurrentIds gs
                                       program' =
                                           if ulId == "available"
                                           then program `snoc` clicked
                                           else filter (/= clicked) program
                          in gs { levelState = SM.insert gs.currentLevel program' gs.levelState }

-- | Add a li-element corresponding to the given Transformer
appendTransformerElement :: forall eff. HTMLElement -> String -> TransformerRecord -> Eff (dom :: DOM | eff) Unit
appendTransformerElement ul id t = do
    doc <- document globalWindow
    li <- createElement "li" doc
    setAttribute "id" id li
    setInnerHTML (replaceColors t.name) li
    appendChild ul li

-- | Add an option-element corresponding to the given Level
appendLevelElement :: forall eff. HTMLElement -> LevelId -> LevelId -> Eff (dom :: DOM | eff) Unit
appendLevelElement select currentId lid = do
    let chapter = getChapter lid
        level = getLevel lid
    doc <- document globalWindow
    option <- createElement "option" doc
    setAttribute "value" lid option
    when (currentId == lid) $
        setAttribute "selected" "selected" option
    setTextContent (levelTitle lid level) option
    appendChild select option

-- | Initial game state for first-time visitors
initialGS :: GameState
initialGS = { currentLevel: "1.1", levelState: SM.empty }

-- | Load game, modify and store the game state. Render the new state
modifyGameStateAndRender :: forall eff.  Boolean
                         -> (GameState -> GameState)
                         -> Eff (storage :: Storage, dom :: DOM, isomer :: Isomer, console :: CONSOLE | eff) Unit
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
    program <- toList <$> traverse (getAttribute "id") lis

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

    -- Click handlers for buttons
    withElementById "reset" doc $ \button ->
        addMouseEventListener MouseClickEvent (const resetLevel :: DOMEvent -> _) button

    -- load game state (or set initial one)
    gs <- fromMaybe initialGS <$> loadGameState
    saveGameState gs

    -- render initial state
    render true gs
