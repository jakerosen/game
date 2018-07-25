{-# language ScopedTypeVariables #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox.Banana as TB

-- main :: (width ~ Int, height ~ Int)
-- => InputMode
-- -> OutputMode
-- -> (Event TermboxEvent -> Behavior (width, height) -> MomentIO (Behavior Scene, Event a))
-- -> IO a

-- TODO: model acquiring resources over time
-- TODO: model number of workers and number of fighters
-- TODO: set up basic UI
-- TODO: allow purchasing of workers and fighters with resources
-- TODO: model basic combat result

f :: Event TB.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior TB.Scene, Event ())
f eEvent _bSize = do
  bCursor :: Behavior TB.Cursor <- do
    let
      g :: TB.Event -> TB.Cursor -> TB.Cursor
      g _ TB.NoCursor = TB.NoCursor
      g event (TB.Cursor col row) =
        case event of
          TB.EventKey TB.KeyArrowDown _ -> TB.Cursor col (row + 1)
          TB.EventKey TB.KeyArrowUp _ -> TB.Cursor col (row - 1)
          TB.EventKey TB.KeyArrowLeft _ -> TB.Cursor (col - 1) row
          TB.EventKey TB.KeyArrowRight _ -> TB.Cursor (col + 1) row
          _ -> TB.Cursor col row
    accumB (TB.Cursor 0 0) (g <$> eEvent)

  bCells :: Behavior TB.Cells <- do
    let
      g :: TB.Cursor -> TB.Event -> TB.Cells -> TB.Cells
      g (TB.Cursor col row) (TB.EventKey TB.KeyEnter _) cells =
        cells <> TB.set col row (TB.Cell ' ' mempty TB.red)
      g _ _ cells = cells
    accumB mempty (g <$> bCursor <@> eEvent)

  let
    bScene :: Behavior TB.Scene
    bScene = TB.Scene <$> bCells <*> bCursor

    eDone :: Event ()
    eDone = () <$ filterE (== TB.EventKey TB.KeyEsc False) eEvent
  pure (bScene, eDone)

main :: IO ()
main = do
  TB.main (TB.InputModeEsc TB.MouseModeNo) TB.OutputModeNormal f
