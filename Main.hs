{-# language ScopedTypeVariables #-}
{-# language RecursiveDo #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox.Banana as TB

-- +-----------------------+
-- | Terminal              |
-- +-----------------------+
--    |          ^         ^
--  TB.Event     |         |
--    |        [TB.Scene]  Done
--    v          |         |
-- +------------------------+
-- | Interface              |
-- +------------------------+
--    |           ^        ^
-- GameInput      |        |
--    |      [GameView]  GameOutput
--    v           |        |
-- +-------------------------+
-- | Game logic              |
-- +-------------------------+
--
-- TODO: set up basic UI
-- TODO: model basic combat result
-- TODO: decide win and loss conditions
-- TODO: Consider fun mechanics

data GameInput = Tick | BuyWorker
  deriving (Eq)
type GameView = (Int, Int, Int)
type GameOutput = ()

showCells :: (Show a) => (Int, Int) -> a -> TB.Cells
showCells (col, row) s =
  foldMap
    (\(i, x) -> TB.set i row (TB.Cell x mempty mempty))
    (zip [col..] (show s))

mkGame
  :: Event GameInput
  -> MomentIO (Behavior GameView, Event GameOutput)
mkGame eGameInput = mdo
  let
    eTick = filterE (== Tick) eGameInput
    eAttemptBuyWorker = filterE (== BuyWorker) eGameInput
    eBuyWorker = whenE ((>= 50) <$> bMinerals) eAttemptBuyWorker

  bNumTicks :: Behavior Int <- accumB 0 ((+1) <$ eTick)

  bWorkers :: Behavior Int <-
    accumB 1 (unions
      [ (+1) <$ eBuyWorker
      ])

  bMinerals :: Behavior Int <-
    accumB 25 (unions
      [ (\w m -> min w 4 + m) <$> bWorkers <@ eTick
      , subtract 50 <$ eBuyWorker
      ])

  let
    bGameView = pure (,,) <*> bNumTicks <*> bWorkers <*> bMinerals
    eGameOutput = never
  pure (bGameView, eGameOutput)

leftmost :: [Event a] -> Event a
leftmost = foldr (unionWith const) never

-- leftmost [e1, e2, e3]

mkInterface
  :: Event TB.Event
  -> Behavior GameView
  -> Event GameOutput
  -> MomentIO (Event GameInput, Behavior TB.Scene, Event ())
mkInterface eEvent bGameView _eGameOutput = do
  let
    eGameInput = leftmost
      [ Tick <$ filterE (== TB.EventKey TB.KeySpace False) eEvent
      , BuyWorker <$ filterE (== TB.EventKey (TB.KeyChar 'w') False) eEvent
      ]

    bCells :: Behavior TB.Cells
    bCells = showCells (5, 5) <$> bGameView

    bGameScene = pure TB.Scene <*> bCells <*> pure TB.NoCursor
    eDone = () <$ filterE (== TB.EventKey TB.KeyEsc False) eEvent
  pure (eGameInput, bGameScene, eDone)

main'
  :: Event TB.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior TB.Scene, Event ())
main' eEvent _bSize = mdo
  (bGameView, eGameOutput) <- mkGame eGameInput
  (eGameInput, bScene, eDone) <- mkInterface eEvent bGameView eGameOutput
  pure (bScene, eDone)

--   bCursor :: Behavior TB.Cursor <- do
--     let
--       g :: TB.Event -> TB.Cursor -> TB.Cursor
--       g _ TB.NoCursor = TB.NoCursor
--       g event (TB.Cursor col row) =
--         case event of
--           TB.EventKey TB.KeyArrowDown _ -> TB.Cursor col (row + 1)
--           TB.EventKey TB.KeyArrowUp _ -> TB.Cursor col (row - 1)
--           TB.EventKey TB.KeyArrowLeft _ -> TB.Cursor (col - 1) row
--           TB.EventKey TB.KeyArrowRight _ -> TB.Cursor (col + 1) row
--           _ -> TB.Cursor col row
--     accumB (TB.Cursor 0 0) (g <$> eEvent)

--   bCells :: Behavior TB.Cells <- do
--     let
--       g :: TB.Cursor -> TB.Event -> TB.Cells -> TB.Cells
--       g (TB.Cursor col row) (TB.EventKey TB.KeyEnter _) cells =
--         cells <> TB.set col row (TB.Cell ' ' mempty TB.red)
--       g _ _ cells = cells
--     accumB mempty (g <$> bCursor <@> eEvent)

--   let
--     bScene :: Behavior TB.Scene
--     bScene = TB.Scene <$> bCells <*> bCursor

--     eDone :: Event ()
--     eDone = () <$ filterE (== TB.EventKey TB.KeyEsc False) eEvent
--   pure (bScene, eDone)

main :: IO ()
main = do
  TB.main (TB.InputModeEsc TB.MouseModeNo) TB.OutputModeNormal main'
