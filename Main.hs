{-# language ScopedTypeVariables #-}
{-# language InstanceSigs #-}
{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Termbox.Banana as TB
import Control.Monad.Trans.Writer
import Data.Text (Text)

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
-- TODO: render battle log
-- TODO: automatic tick


data GameInput = Tick | BuyWorker | BuyFighter
  deriving (Eq)
type GameView = (Int, Int, Int)
type GameOutput = ()

--------------------------------------------------------------------------------
-- Units
--------------------------------------------------------------------------------
data Unit
  = UnitFighter Fighter
  | UnitMonster Monster

data Fighter = Fighter Int
  deriving (Show)

data Monster = Monster Int
  deriving (Show)

inflictDamage :: Int -> Unit -> Maybe Unit
inflictDamage x (UnitFighter (Fighter hp)) =
  if hp > x
    then Just (UnitFighter (Fighter (hp - x)))
    else Nothing
inflictDamage x (UnitMonster (Monster hp)) =
  if hp > x
    then Just (UnitMonster (Monster (hp - x)))
    else Nothing

--------------------------------------------------------------------------------
-- Pure Functions
--------------------------------------------------------------------------------
showCells :: (Show a) => (Int, Int) -> a -> TB.Cells
showCells (col, row) s =
  foldMap
    (\(i, x) -> TB.set i row (TB.Cell x mempty mempty))
    (zip [col..] (show s))

resolveBattle
  :: [Unit] -- Acts first
  -> [Unit]
  -> Writer [Text] ([Unit], [Unit])
resolveBattle [] theirs = do
  tell ["They're beating on our base!"]
  pure ([], theirs)
resolveBattle ours [] = do
  tell ["We are victorious!"]
  pure (ours, [])
resolveBattle ours@(ourFirst:ourRest) theirs@(theirFirst:theirRest) =
  case inflictDamage (length ours) theirFirst of
    Nothing -> do
      -- log that their monster has died
      tell ["A Monster has died"]
      theyAttackUs theirRest
    Just x ->
      theyAttackUs (x:theirRest)
  where
    theyAttackUs :: [Unit] -> Writer [Text] ([Unit], [Unit])
    theyAttackUs theirs =
      case inflictDamage (length theirs) ourFirst of
        Nothing -> do
          tell ["Our Unit has died"]
          pure (ourRest, theirs)
        Just x ->
          pure (x:ourRest, theirs)

--------------------------------------------------------------------------------
-- Game Logic
--------------------------------------------------------------------------------

mkGame
  :: Event GameInput
  -> MomentIO (Behavior GameView, Event GameOutput)
mkGame eGameInput = mdo
  let
    eTick = filterE (== Tick) eGameInput

    eBattleResult :: Event ([Text], [Unit], [Unit])
    eBattleResult = f <$> bFighters <*> bHorde <@ eTick
      where
        f fs ms =
          let ((fs', ms'), log) = runWriter (resolveBattle fs ms)
          in (log, fs', ms')

    eAttemptBuyWorker = filterE (== BuyWorker) eGameInput
    eBuyWorker = whenE ((>= 50) <$> bMinerals) eAttemptBuyWorker

    eAttemptBuyFighter = filterE (== BuyFighter) eGameInput
    eBuyFighter = whenE ((>= 100) <$> bMinerals) eAttemptBuyFighter

  bNumTicks :: Behavior Int <- accumB 0 ((+1) <$ eTick)

  bWorkers :: Behavior Int <-
    accumB 1 (unions
      [ (+1) <$ eBuyWorker
      ])

  bFighters :: Behavior [Unit] <-
    stepper [] (leftmost
      [ (UnitFighter (Fighter 20) :) <$> bFighters <@ eBuyFighter
      , (\(_, _, fs) -> fs) <$> eBattleResult
      ])

  bHorde :: Behavior [Unit] <-
    let
      eSpawn = filterJust (f <$> bNumTicks <@ eTick)
        where
          f = \n ->
            if n `mod` 100 == 0 && n /= 0
              then Just [UnitMonster (Monster 100)]
              else Nothing
    in
      stepper []
        (unionWith (++)
          eSpawn
          ((\(_, ms, _) -> ms) <$> eBattleResult))

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

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

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
      , BuyFighter <$ filterE (== TB.EventKey (TB.KeyChar 'f') False) eEvent
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
