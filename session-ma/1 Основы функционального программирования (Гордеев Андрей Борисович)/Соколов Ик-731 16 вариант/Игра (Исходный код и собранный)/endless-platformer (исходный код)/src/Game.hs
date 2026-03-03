module Game
  ( runGame
  ) where

import Assets (loadAssets)
import Config (loadConfig)
import Database
  ( getTopScores
  , initDb
  , insertScore
  , listSaves
  , loadSave
  , upsertSave
  )
import Game.Constants
  ( backgroundColor
  , dbFileName
  , defaultSaveSeed
  , fps
  , leaderboardLimit
  , metersPerPixel
  , scoreSavedText
  , windowHeight
  , windowPos
  , windowTitle
  , windowWidth
  )
import Graphics.Gloss (Display(InWindow))
import Graphics.Gloss.Interface.IO.Game
  ( Event(..)
  , Key(..)
  , KeyState(Down)
  , SpecialKey(..)
  , playIO
  )
import Input (handleInputEvent)
import Render (drawAppIO)
import System.Exit (die, exitSuccess)
import World
  ( App(..)
  , Screen(..)
  , applySaveRow
  , goLeaderboard
  , goLoadGame
  , goTitle
  , initialApp
  , menuAdjustDifficulty
  , menuDifficultyIx
  , menuExitIx
  , menuIx
  , menuLeaderboardIx
  , menuLoadIx
  , menuMove
  , menuStartIx
  , nameAddChar
  , nameBackspace
  , nameConfirm
  , pauseActivate
  , pauseMove
  , requestLoadSlot
  , requestSaveSlot
  , slotMove
  , startPlaying
  , stepWorld
  , togglePause
  )

runGame :: IO ()
runGame = do
  eCfg <- loadConfig "config.json"
  case eCfg of
    Left err  -> die err
    Right cfg -> do
      eAssets <- loadAssets
      case eAssets of
        Left err   -> die err
        Right assets -> do
          eDb <- initDb dbFileName
          case eDb of
            Left err -> die err
            Right () ->
              playIO
                gameDisplay
                backgroundColor
                fps
                (initialApp cfg dbFileName)
                (drawAppIO assets)
                handleEvent
                stepApp

gameDisplay :: Display
gameDisplay = InWindow windowTitle (windowWidth, windowHeight) windowPos

handleEvent :: Event -> App -> IO App
handleEvent ev app =
  case ev of
    EventResize (w, h) ->
      pure app { viewW = w, viewH = h }

    EventKey (SpecialKey KeyEsc) Down _ _ ->
      exitSuccess

    EventKey (SpecialKey KeyEnter) Down _ _ ->
      case appScreen app of
        NameEntry -> pure (nameConfirm app)
        LoadGame  -> pure (requestLoadSlot app)
        SaveGame  -> pure (requestSaveSlot app)
        _         -> handleEnter app

    EventKey (SpecialKey KeyBackspace) Down _ _ ->
      pure (handleBackspace app)

    EventKey (Char '\b') Down _ _ ->
      pure (handleBackspace app)

    EventKey (Char 'q') Down _ _ ->
      pure (handleQuitKey app)

    EventKey (Char 'Q') Down _ _ ->
      pure (handleQuitKey app)

    EventKey (SpecialKey KeySpace) Down _ _
      | appScreen app == NameEntry ->
          pure (nameAddChar ' ' app)

    EventKey (Char c) Down _ _
      | appScreen app == NameEntry ->
          pure (nameAddChar c app)

    EventKey key Down _ _
      | isDebugKey key ->
          pure app { appShowDebug = not (appShowDebug app) }
      | isPauseKey key ->
          pure (togglePause app)
      | isMenuUpKey key && appScreen app == Title ->
          pure (menuMove (-1) app)
      | isMenuDownKey key && appScreen app == Title ->
          pure (menuMove 1 app)
      | isMenuLeftKey key && appScreen app == Title ->
          pure (menuAdjustDifficulty (-1) app)
      | isMenuRightKey key && appScreen app == Title ->
          pure (menuAdjustDifficulty 1 app)
      | isMenuUpKey key && isSlotScreen (appScreen app) ->
          pure (slotMove (-1) app)
      | isMenuDownKey key && isSlotScreen (appScreen app) ->
          pure (slotMove 1 app)
      | isMenuUpKey key && appScreen app == Paused ->
          pure (pauseMove (-1) app)
      | isMenuDownKey key && appScreen app == Paused ->
          pure (pauseMove 1 app)

    _ ->
      pure (applyInput ev app)

handleQuitKey :: App -> App
handleQuitKey app =
  case appScreen app of
    NameEntry   -> goTitle app
    Controls    -> goTitle app
    Leaderboard -> goTitle app
    LoadGame    -> goTitle app
    GameOver    -> goTitle app
    Playing     -> goTitle app
    Paused      -> goTitle app
    SaveGame    -> app { appScreen = Paused, pauseIx = 0 }
    Title       -> app

handleBackspace :: App -> App
handleBackspace app =
  case appScreen app of
    NameEntry   -> nameBackspace app
    LoadGame    -> goTitle app
    SaveGame    -> app { appScreen = Paused, pauseIx = 0 }
    Playing     -> goTitle app
    Paused      -> goTitle app
    GameOver    -> goTitle app
    Leaderboard -> goTitle app
    Controls    -> goTitle app
    _           -> app

isSlotScreen :: Screen -> Bool
isSlotScreen s = s == LoadGame || s == SaveGame

handleEnter :: App -> IO App
handleEnter app =
  case appScreen app of
    Title ->
      case menuIx app of
        i | i == menuStartIx       -> pure (startPlaying app)
        i | i == menuDifficultyIx  -> pure (menuAdjustDifficulty 1 app)
        i | i == menuLeaderboardIx -> pure (goLeaderboard app)
        i | i == menuLoadIx        -> pure (goLoadGame app)
        i | i == menuExitIx        -> exitSuccess
        _                          -> pure app
    Paused ->
      pure (pauseActivate app)
    _ ->
      pure (startPlaying app)

isDebugKey :: Key -> Bool
isDebugKey key = key == Char 't' || key == Char 'T'

isPauseKey :: Key -> Bool
isPauseKey key = key == Char 'p' || key == Char 'P'

isMenuUpKey :: Key -> Bool
isMenuUpKey key =
  key == SpecialKey KeyUp || key == Char 'w' || key == Char 'W'

isMenuDownKey :: Key -> Bool
isMenuDownKey key =
  key == SpecialKey KeyDown || key == Char 's' || key == Char 'S'

isMenuLeftKey :: Key -> Bool
isMenuLeftKey key =
  key == SpecialKey KeyLeft || key == Char 'a' || key == Char 'A'

isMenuRightKey :: Key -> Bool
isMenuRightKey key =
  key == SpecialKey KeyRight || key == Char 'd' || key == Char 'D'

applyInput :: Event -> App -> App
applyInput ev app =
  case appScreen app of
    Playing -> app { appInput = handleInputEvent ev (appInput app) }
    _       -> app

stepApp :: Float -> App -> IO App
stepApp dt app = do
  let app1 = stepWorld dt app
  performPendingDb app1

performPendingDb :: App -> IO App
performPendingDb app0 = do
  app1 <-
    if appPendingScoreSave app0
      then saveScore app0
      else pure app0

  app2 <-
    if appPendingLbLoad app1
      then loadLeaderboard app1
      else pure app1

  app3 <-
    if appPendingSavesLoad app2
      then loadSavesList app2
      else pure app2

  app4 <-
    case appPendingSaveSlot app3 of
      Nothing    -> pure app3
      Just slot  -> saveGameSlot slot app3

  case appPendingLoadSlot app4 of
    Nothing    -> pure app4
    Just slot  -> loadGameSlot slot app4

saveScore :: App -> IO App
saveScore app = do
  let meters = floor (worldScroll app * metersPerPixel) :: Int
  let diff   = show (appDifficulty app)
  e <- insertScore (appDbPath app) (appPlayerName app) meters diff
  case e of
    Left err ->
      pure app
        { appPendingScoreSave = False
        , appNotice = Just err
        }
    Right () ->
      pure app
        { appPendingScoreSave = False
        , appNotice = Just scoreSavedText
        }

loadLeaderboard :: App -> IO App
loadLeaderboard app = do
  e <- getTopScores (appDbPath app) leaderboardLimit
  case e of
    Left err ->
      pure app
        { appPendingLbLoad = False
        , appLeaderboard   = []
        , appNotice        = Just err
        }
    Right rows ->
      pure app
        { appPendingLbLoad = False
        , appLeaderboard   = rows
        , appNotice        = Nothing
        }

loadSavesList :: App -> IO App
loadSavesList app = do
  e <- listSaves (appDbPath app)
  case e of
    Left err ->
      pure app
        { appPendingSavesLoad = False
        , appSaves            = []
        , appNotice           = Just err
        }
    Right rows ->
      pure app
        { appPendingSavesLoad = False
        , appSaves            = rows
        , appNotice           = Nothing
        }

saveGameSlot :: Int -> App -> IO App
saveGameSlot slot app = do
  let diff = show (appDifficulty app)
  e <-
    upsertSave (appDbPath app) slot defaultSaveSeed
      (worldScroll app)
      (playerLives app)
      diff
  case e of
    Left err ->
      pure app
        { appPendingSaveSlot = Nothing
        , appNotice          = Just err
        }
    Right () ->
      pure
        app
          { appPendingSaveSlot   = Nothing
          , appPendingSavesLoad  = True
          , appNotice            = Just ("Saved to slot " ++ show slot ++ ".")
          , appScreen            = Paused
          , pauseIx              = 0
          }

loadGameSlot :: Int -> App -> IO App
loadGameSlot slot app = do
  e <- loadSave (appDbPath app) slot
  case e of
    Left err ->
      pure app
        { appPendingLoadSlot = Nothing
        , appNotice          = Just err
        }
    Right row ->
      case applySaveRow row app of
        Left err ->
          pure app
            { appPendingLoadSlot = Nothing
            , appNotice          = Just err
            }
        Right loaded ->
          pure loaded