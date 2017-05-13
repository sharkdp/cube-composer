module Main (App(..), main) where

import Prelude
import Color (rgb, graytone)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import DOM (DOM)
import DOM.HTML.Event.EventTypes (change, click, keydown)
import DOM.Event.Types (Event())
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget, htmlElementToEventTarget, htmlElementToElement)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (appendChild, setTextContent, parentElement)
import DOM.Node.Types (Element(), elementToNode, elementToEventTarget)
import Data.Array as A
import Data.Enum (enumFromTo)
import Data.Either (fromRight)
import Data.Foldable (foldl, traverse_)
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable, filter, snoc, dropWhile, tail, head, (:), last, mapMaybe, reverse, length)
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.Monoid (class Monoid, mempty)
import Data.StrMap as SM
import Data.String.Regex (regex, parseFlags, replace)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Graphics.Drawing as D
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, clearRect, getCanvasWidth, getCanvasHeight)
import Graphics.Isometric (Scene, Color, scale, renderScene, prism, filled, cube)

import Analytics (analyticsLevelChanged)
import DOMHelper (addEventListener', withElementById, getElementById', getDocument, unsafeGetAttribute, children', getSelectedValue, setInnerHTML, ctrlKey, keyCode, unsafeEventToKeyboardEvent, unsafeElementToHTMLElement, setStyleAttribute, classRemove, classAdd)
import Levels (firstLevel, levelTitle, getLevel, getChapter, allLevelIds, getTransformer, getTransformerRecord)
import Sortable (installSortable)
import Storage (STORAGE, saveGameState, loadGameState)
import Transformer (allSteps)
import Types (GameState, LevelId, TransformerRecord, Chapter, TransformerId, Wall, Stack, Cube(..))

-- | Type synonyms for different combinations of effects
type EffDOM    = forall eff. Eff (dom :: DOM | eff) Unit
type App       = forall eff. Eff (dom :: DOM, console :: CONSOLE, canvas :: CANVAS, storage :: STORAGE | eff) Unit

-- | RGB codes for the abstract colors
cubeColor :: Cube -> Color
cubeColor Cyan   = rgb   0 160 176
cubeColor Brown  = rgb 106  74  60
cubeColor Red    = rgb 204  51  63
cubeColor Orange = rgb 235 104  65
cubeColor Yellow = rgb 237 201  81

-- | Spacing between two walls
spacing :: Number
spacing = 5.5

-- | Like `foldMap` on `List`, but the function also takes an index parameter
foldMapIndexed :: forall a m. (Monoid m) => (Int -> a -> m) -> List a -> m
foldMapIndexed f xs' = go 0 xs'
    where go _ Nil         = mempty
          go i (Cons x xs) = f i x <> go (i + 1) xs

-- | Traverse a StrMap while performing monadic side effects
traverseWithKey_ :: forall a m. (Monad m) => (String -> a -> m Unit) -> SM.StrMap a -> m Unit
traverseWithKey_ f sm = SM.foldM (const f) unit sm

renderCube :: Int -> Int -> Int -> Cube -> Scene
renderCube x y z c = filled (cubeColor c) $ cube point 0.9
  where point = { x: toNumber (-x)
                , y: spacing * toNumber y
                , z: toNumber z }

-- | Render a single stack of cubes
renderStack :: Int -> Int -> Int -> Stack -> Scene
renderStack len y x stack = foldMapIndexed (renderCube (len - x) y) stack

-- | Render a wall (multiple stacks)
renderWall :: Int -> Wall -> Scene
renderWall y Nil =
    -- Render a gray placeholder for the empty wall
    filled gray $ prism { x: -8.0, y: spacing * toNumber y, z: 0.0 } 8.0 0.9 0.1
      where gray = graytone 0.4
renderWall y wall = foldMapIndexed (renderStack len y) (reverse wall)
  where len = length wall

-- | Render a series of walls
renderWalls :: List Wall -> Scene
renderWalls walls = foldMapIndexed renderWall walls

-- | Render the target shape
renderTarget :: Wall -> Scene
renderTarget target = renderWall 0 target

-- | Get program (list of transformer ids) of the active level
getCurrentIds :: GameState -> (List TransformerId)
getCurrentIds gs = case (SM.lookup gs.currentLevel gs.levelState) of
                       Just ids -> ids
                       Nothing -> Nil

-- | Render all UI components, DOM and canvas
render :: Boolean -> GameState -> App
render setupUI gs = do
    doc <- getDocument
    canvas <- unsafeFromJust <$> getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    w <- getCanvasWidth canvas
    h <- getCanvasHeight canvas

    let level   = getLevel gs.currentLevel
        chapter = getChapter gs.currentLevel
        tids    = getCurrentIds gs

    -- Set up UI, only if new level is loaded
    when setupUI $ do
        ulAvailable <- unsafeFromJust <$> getElementById' "available" doc
        ulProgram   <- unsafeFromJust <$> getElementById' "program" doc

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
    let lightPos = { x: -2.0, y: 1.0, z: 3.0 }
    _ <- clearRect ctx { x: 0.0, y: 0.0, w, h }

    let renderCanvas x y s scene = D.render ctx $ D.translate x y $
            renderScene lightPos (scale s scene)
    renderCanvas    2.0 284.0 49.0 (renderWalls steps)
    renderCanvas 1280.0 380.0 35.0 (renderTarget level.target)

    -- DOM 'rendering'
    let solved = maybe false (_ == (level.target)) (last steps)
        visibility = if solved then "visible" else "hidden"
        cssAction = if solved then classAdd "flash" else classRemove "flash"

    withElementById "message" doc (setStyleAttribute "visibility" visibility <<< unsafeElementToHTMLElement)
    withElementById "solved" doc cssAction

    let helpHTML = maybe "" (nameToHTML <<< replaceTransformers chapter) level.help
    withElementById "help" doc (setInnerHTML helpHTML)

    -- Debug output:
    let toArray = A.fromFoldable :: forall a. List a -> Array a
        toArrays = toArray <<< map toArray
    log $ "Program: " <> show (toArray tids)
    log $ "Target: " <> show (toArrays level.target)
    log "Steps:"
    traverse_ (logShow <<< toArrays) steps
    log ""

-- | Replace all occurences of a pattern in a string with a replacement
replaceAll :: String -> String -> String -> String
replaceAll regexString replacement = replace pattern replacement
    where
      flags = parseFlags "g"
      pattern = unsafePartial $ fromRight (regex regexString flags)

-- | Replace color placeholders in the transformer description by colored rectangular divs
replaceColors :: String -> String
replaceColors s' =
    foldl replaceColor s' ("X" : map show (Cyan `enumFromTo` Yellow))
        where replaceColor s c = replaceAll (pattern c) (replacement c) s
              pattern c = "{" <> c <> "}"
              replacement c = "<div class=\"cube " <> c <> "\"> </div>"

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
          pattern id = "`" <> id <> "`"
          replacement tr = "<span class=\"transformer\">" <> tr.name <> "</span>"

-- | Clear all functions for the current level
resetLevel :: App
resetLevel = modifyGameStateAndRender true mod
    where mod gs = gs { levelState = SM.insert gs.currentLevel Nil gs.levelState }

-- | Go to the previous level
prevLevel :: App
prevLevel = modifyGameStateAndRender true mod
    where mod gs   = gs { currentLevel = prev gs.currentLevel }
          prev cur = fromMaybe cur $ before cur allLevelIds
          before _ Nil                   = Nothing
          before _ (Cons _ Nil)          = Nothing
          before x (Cons b (Cons x' xs)) = if x == x'
                                           then Just b
                                           else before x (x':xs)

-- | Go to the next level
nextLevel :: App
nextLevel = do
    mgs <- loadGameState
    let gs' = fromMaybe initialGS mgs
    analyticsLevelChanged (next gs'.currentLevel)

    modifyGameStateAndRender true mod
    where mod gs   = gs { currentLevel = next gs.currentLevel }
          next cur = fromMaybe cur $ head =<< (tail $ dropWhile (_ /= cur) allLevelIds)

-- | General key press handler
keyPress :: Event -> App
keyPress event = void do
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
             _ -> pure unit

-- | Click handler for the <li> elements (transformers)
clickLi :: Element -> Event -> App
clickLi liEl event = do
    newId <- unsafeGetAttribute "id" liEl
    ul <- unsafeFromJust <$> parentElement (elementToNode liEl)
    ulId <- unsafeGetAttribute "id" ul
    modifyGameStateAndRender true (modify ulId newId)

    where modify ulId clicked gs = let program = getCurrentIds gs
                                       program' =
                                           if ulId == "available"
                                           then program `snoc` clicked
                                           else filter (_ /= clicked) program
                          in gs { levelState = SM.insert gs.currentLevel program' gs.levelState }

-- | Add a li-element corresponding to the given Transformer
appendTransformerElement :: Element -> String -> TransformerRecord -> EffDOM
appendTransformerElement ul id t = void do
    doc <- getDocument
    li <- createElement "li" doc
    setAttribute "id" id li
    setInnerHTML (nameToHTML t.name) li
    appendChild (elementToNode li) (elementToNode ul)

-- | Add an option-element corresponding to the given Level
appendLevelElement :: Element -> LevelId -> LevelId -> EffDOM
appendLevelElement select currentId lid = void do
    let chapter = getChapter lid
        level = getLevel lid
    doc <- getDocument
    option <- createElement "option" doc
    setAttribute "value" lid option
    when (currentId == lid) $
        setAttribute "selected" "selected" option
    setTextContent (levelTitle lid level) (elementToNode option)
    appendChild (elementToNode option) (elementToNode select)

-- | Initial game state for first-time visitors
initialGS :: GameState
initialGS = { currentLevel: firstLevel, levelState: SM.empty }

-- | Load, modify and store the game state. Render the new state
modifyGameStateAndRender :: Boolean
                         -> (GameState -> GameState)
                         -> forall eff. Eff (dom :: DOM, console :: CONSOLE, canvas :: CANVAS, storage :: STORAGE | eff) Unit
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
    ulAvailable <- unsafeFromJust <$> getElementById' "program" doc
    lis <- children' ulAvailable
    let getId = unsafeGetAttribute "id" <<< htmlElementToElement
    program <- fromFoldable <$> traverse getId lis

    modifyGameStateAndRender false $ \gs ->
        gs { levelState = SM.insert gs.currentLevel program gs.levelState }

-- | Unsafe version of `fromJust`
unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust

main :: App
main = do
    doc <- getDocument

    -- install sortable
    ulAvailable <- unsafeFromJust <$> getElementById' "available" doc
    ulProgram   <- unsafeFromJust <$> getElementById' "program" doc
    installSortable ulAvailable (pure unit)
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
