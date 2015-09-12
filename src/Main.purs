module Main (App(..), main) where

import Prelude
import Control.Apply
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import DOM.Event.EventTypes (change, click, keydown)
import DOM.Event.Types (Event())
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget, htmlElementToEventTarget, htmlElementToElement)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, setTextContent, parentElement)
import DOM.Node.Types (Element(), Node(), elementToNode, elementToEventTarget)
import Data.Enum
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.Nullable (toMaybe)
import Data.String.Regex (regex, parseFlags, replace)
import Data.Traversable
import qualified Data.StrMap as SM

import Analytics
import DOMHelper
import Isomer
import Levels
import Sortable
import Storage
import Transformer
import Types

-- | Type synonyms for different combinations of effects
type EffIsomer = forall eff. Eff (isomer :: ISOMER | eff) Unit
type EffDOM    = forall eff. Eff (dom :: DOM | eff) Unit
type App       = forall eff. Eff (dom :: DOM, console :: CONSOLE, isomer :: ISOMER, storage :: STORAGE | eff) Unit

-- | RGB codes for the abstract colors
cubeColor :: Cube -> IsomerColor
cubeColor Cyan   = colorFromRGB   0 160 176
cubeColor Brown  = colorFromRGB 106  74  60
cubeColor Red    = colorFromRGB 204  51  63
cubeColor Orange = colorFromRGB 235 104  65
cubeColor Yellow = colorFromRGB 237 201  81

-- | Spacing between two walls
spacing :: Number
spacing = 5.5

-- | Like traverse_, but the function also takes an index parameter
traverseWithIndex_ :: forall a b m. (Applicative m) => (Int -> a -> m b) -> (List a) -> m Unit
traverseWithIndex_ f xs = go xs 0
    where go Nil _         = return unit
          go (Cons x xs) i = f i x *> go xs (i + 1)

-- | Traverse a StrMap while performing monadic side effects
traverseWithKey_ :: forall a m. (Monad m) => (String -> a -> m Unit) -> SM.StrMap a -> m Unit
traverseWithKey_ f sm = SM.foldM (const f) unit sm

-- | Render a single stack of cubes
renderStack :: IsomerInstance -> Number -> Number -> Stack -> EffIsomer
renderStack isomer y x stack =
    traverseWithIndex_ (\z -> renderCube isomer x (-spacing * y) (toNumber z)) $ map cubeColor stack

-- | Render a wall (multiple stacks)
renderWall :: IsomerInstance -> Number -> Wall -> EffIsomer
renderWall isomer y Nil =
    -- Render a gray placeholder for the empty wall
    renderBlock isomer 1.0 (-spacing * y) 0.0 5.0 0.9 0.1 (colorFromRGB 100 100 100)
renderWall isomer y wall =
    traverseWithIndex_ (\x -> renderStack isomer y (toNumber (length wall - x))) (reverse wall)

-- | Render a series of walls
renderWalls :: IsomerInstance -> (List Wall) -> EffIsomer
renderWalls isomer walls = do
    setIsomerConfig isomer 40.0 40.0 400.0
    traverseWithIndex_ (\y -> renderWall isomer (toNumber y)) walls

-- | Render the target shape
renderTarget :: IsomerInstance -> Wall -> EffIsomer
renderTarget isomer target = do
    setIsomerConfig isomer 30.0 1280.0 550.0
    renderWall isomer 0.0 target

-- | Get program (list of transformer ids) of the active level
getCurrentIds :: GameState -> (List TransformerId)
getCurrentIds gs = case (SM.lookup gs.currentLevel gs.levelState) of
                       Just ids -> ids
                       Nothing -> Nil

-- | Render all UI components, DOM and canvas
render :: Boolean -> GameState -> App
render setupUI gs = do
    doc <- getDocument
    isomer <- getIsomerInstance "canvas"

    let level   = getLevel gs.currentLevel
        chapter = getChapter gs.currentLevel
        tids    = getCurrentIds gs

    -- Set up UI, only if new level is loaded
    when setupUI $ do
        Just ulAvailable <- getElementById' "available" doc
        Just ulProgram   <- getElementById' "program" doc

        setInnerHTML "" ulAvailable
        setInnerHTML "" ulProgram

        let unused       = foldl (flip SM.delete) chapter.transformers tids
            insert sm id = case (getTransformerRecord chapter id) of
                               (Just tr) -> SM.insert id tr sm
                               Nothing ->   sm
            active =       foldl insert SM.empty tids

        -- create li elements for transformers
        traverseWithKey_ (appendTransformerElement ulAvailable) unused
        traverseWithKey_ (appendTransformerElement ulProgram) active

        -- set up mouse event handlers
        let installClickHandler li = addEventListener' click (clickLi (htmlElementToElement li)) (htmlElementToEventTarget li)
        children' ulAvailable >>= traverse_ installClickHandler
        children' ulProgram   >>= traverse_ installClickHandler

        withElementById "levels" doc $ \selectLevel -> do
            setInnerHTML "" selectLevel
            traverse_ (appendLevelElement selectLevel gs.currentLevel) allLevelIds

    let transformers = mapMaybe (getTransformer chapter) tids
    let steps = allSteps transformers level.initial

    -- On-canvas rendering
    clearCanvas isomer
    renderWalls isomer steps
    renderTarget isomer level.target

    -- DOM 'rendering'
    let solved = maybe false (== (level.target)) (last steps)
        visibility = if solved then "visible" else "hidden"
        cssAction = if solved then classAdd "flash" else classRemove "flash"

    withElementById "message" doc (setStyleAttribute "visibility" visibility <<< unsafeElementToHTMLElement)
    withElementById "solved" doc cssAction

    let helpHTML = maybe "" (nameToHTML <<< replaceTransformers chapter) level.help
    withElementById "help" doc (setInnerHTML helpHTML)

    -- DEBUG:
    log $ "Program: " ++ show tids
    log $ "Initial: " ++ show level.initial
    log "Steps:"
    traverse_ print steps
    log "---"
    log $ "Target: " ++ show level.target
    log ""

-- | Replace all occurences of a pattern in a string with a replacement
replaceAll :: String -> String -> String -> String
replaceAll pattern replacement = replace (regex pattern flags) replacement
    where flags = parseFlags "g"

-- | Replace color placeholders in the transformer description by colored rectangular divs
replaceColors :: String -> String
replaceColors s =
    foldl replaceColor s ("X" : map show (toList $ Cyan `enumFromTo` Yellow))
        where replaceColor s c = replaceAll (pattern c) (replacement c) s
              pattern c = "{" ++ c ++ "}"
              replacement c = "<div class=\"cube " ++ c ++ "\"> </div>"

-- | Replace stack markers
replaceStacks :: String -> String
replaceStacks = replaceAll "\\[" """<div class="stack">""" <<<
                replaceAll "\\]" "</div>"

-- | Render a transformer name as HTML
nameToHTML :: String -> String
nameToHTML = replaceColors <<< replaceStacks

-- | Replace transformer names by boxes
replaceTransformers :: Chapter -> String -> String
replaceTransformers ch initial = SM.fold replaceT initial ch.transformers
    where replaceT text id tr = replaceAll (pattern id) (replacement tr) text
          pattern id = "`" ++ id ++ "`"
          replacement tr = "<span class=\"transformer\">" ++ tr.name ++ "</span>"

-- | Clear all functions for the current level
resetLevel = modifyGameStateAndRender true mod
    where mod gs = gs { levelState = SM.insert gs.currentLevel Nil gs.levelState }

-- | Go to the previous level
prevLevel = modifyGameStateAndRender true mod
    where mod gs   = gs { currentLevel = prev gs.currentLevel }
          prev cur = fromMaybe cur $ before cur allLevelIds
          before _ Nil                   = Nothing
          before _ (Cons _ Nil)          = Nothing
          before x (Cons b (Cons x' xs)) = if x == x'
                                           then Just b
                                           else before x (x':xs)

-- | Go to the next level
nextLevel = modifyGameStateAndRender true mod
    where mod gs   = gs { currentLevel = next gs.currentLevel }
          next cur = fromMaybe cur $ head =<< (tail $ dropWhile (/= cur) allLevelIds)

-- | General key press handler
keyPress :: Event -> App
keyPress event = do
    doc <- getDocument
    let kev = unsafeEventToKeyboardEvent event
        code = keyCode kev
        ctrlPressed = ctrlKey kev

    when (not ctrlPressed) $
        case code of
             -- 'r': reset lists
             82 ->  resetLevel
             -- '<-', 'p': previous level
             37 -> prevLevel
             80 -> prevLevel
             -- '->', 'n': next level
             39 -> nextLevel
             78 -> nextLevel
             _ -> return unit

    return unit

-- | Click handler for the <li> elements (transformers)
clickLi :: Element -> Event -> App
clickLi liEl event = do
    newId <- unsafeGetAttribute "id" liEl
    Just ul <- toMaybe <$> parentElement (elementToNode liEl)
    ulId <- unsafeGetAttribute "id" ul
    modifyGameStateAndRender true (modify ulId newId)

    where modify ulId clicked gs = let program = getCurrentIds gs
                                       program' =
                                           if ulId == "available"
                                           then program `snoc` clicked
                                           else filter (/= clicked) program
                          in gs { levelState = SM.insert gs.currentLevel program' gs.levelState }

-- | Add a li-element corresponding to the given Transformer
appendTransformerElement :: Element -> String -> TransformerRecord -> EffDOM
appendTransformerElement ul id t = do
    doc <- getDocument
    li <- createElement "li" doc
    setAttribute "id" id li
    setInnerHTML (nameToHTML t.name) li
    appendChild (elementToNode li) (elementToNode ul)
    return unit

-- | Add an option-element corresponding to the given Level
appendLevelElement :: Element -> LevelId -> LevelId -> EffDOM
appendLevelElement select currentId lid = do
    let chapter = getChapter lid
        level = getLevel lid
    doc <- getDocument
    option <- createElement "option" doc
    setAttribute "value" lid option
    when (currentId == lid) $
        setAttribute "selected" "selected" option
    setTextContent (levelTitle lid level) (elementToNode option)
    appendChild (elementToNode option) (elementToNode select)
    return unit

-- | Initial game state for first-time visitors
initialGS :: GameState
initialGS = { currentLevel: firstLevel, levelState: SM.empty }

-- | Load, modify and store the game state. Render the new state
modifyGameStateAndRender :: Boolean
                         -> (GameState -> GameState)
                         -> forall eff. Eff (dom :: DOM, console :: CONSOLE, isomer :: ISOMER, storage :: STORAGE | eff) Unit
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
levelChangeHandler :: Element -> Event -> App
levelChangeHandler selectLevel _ = do
    levelId <- getSelectedValue selectLevel

    analyticsLevelChanged levelId

    modifyGameStateAndRender true $ \gs ->
        gs { currentLevel = levelId }

-- | Event handler for a 'reprogram' (new instruction, re-ordering, ..)
reprogramHandler :: App
reprogramHandler = do
    doc <- getDocument

    -- Retrieve current 'program'
    Just ulAvailable <- getElementById' "program" doc
    lis <- children' ulAvailable
    let getId = unsafeGetAttribute "id" <<< htmlElementToElement
    program <- toList <$> traverse getId lis

    modifyGameStateAndRender false $ \gs ->
        gs { levelState = SM.insert gs.currentLevel program gs.levelState }

main :: App
main = do
    doc <- getDocument

    -- install sortable
    Just ulAvailable <- getElementById' "available" doc
    Just ulProgram   <- getElementById' "program" doc
    installSortable ulAvailable (return unit)
    installSortable ulProgram reprogramHandler

    -- set up keyboard event handlers
    win <- windowToEventTarget <$> window
    addEventListener' keydown keyPress win

    -- set up 'change' handler for the level selector
    withElementById "levels" doc $ \selectLevel ->
        addEventListener' change (levelChangeHandler selectLevel) (elementToEventTarget selectLevel)

    -- Click handlers for buttons
    withElementById "reset" doc $ \button ->
        addEventListener' click (const resetLevel) (elementToEventTarget button)

    withElementById "nextlevel" doc $ \button ->
        addEventListener' click (const nextLevel) (elementToEventTarget button)

    -- load game state (or set initial one)
    gs <- fromMaybe initialGS <$> loadGameState
    saveGameState gs

    -- render initial state
    render true gs
