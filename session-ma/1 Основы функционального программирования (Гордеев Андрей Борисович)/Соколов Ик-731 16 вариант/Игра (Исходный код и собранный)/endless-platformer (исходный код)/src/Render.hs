module Render (drawAppIO) where

import Assets (Assets(..))
import Config (GenConfig(..), SpeedRule(..), difficultyLevel, objectChance)
import Data.List (find, sortOn)
import Game.Constants
import Geometry
  ( Interval(..)
  , Rect(..)
  , intervalContains
  , intervalOverlaps
  , rectBottom
  , rectLeft
  , rectRight
  , rectTop
  )
import Graphics.Gloss
import World
  ( App(..)
  , Difficulty(..)
  , Screen(..)
  , isSupported
  )
import Database (ScoreRow(..), SaveRow(..))
import Text.Printf (printf)

drawAppIO :: Assets -> App -> IO Picture
drawAppIO assets app = pure (drawApp assets app)

drawApp :: Assets -> App -> Picture
drawApp assets app =
  case appScreen app of
    Title       -> drawMenu app
    Controls    -> drawControls app
    Playing     -> drawPlaying assets app
    Paused      -> drawPaused assets app
    Leaderboard -> drawLeaderboard app
    LoadGame    -> drawLoadGame app
    GameOver    -> drawGameOver assets app
    NameEntry   -> drawNameEntry app
    SaveGame    -> drawSaveGame app

drawMenu :: App -> Picture
drawMenu app =
  pictures
    [ translate titleX titleY
        $ scale titleScale titleScale
        $ color titleColor
        $ Text titleText
    , drawMenuItem app 0 "START" menuItemY0
    , drawMenuItem app 1 ("LEVEL: " ++ show (appDifficulty app)) menuItemY1
    , drawMenuItem app 2 "TOP" menuItemY2
    , drawMenuItem app 3 "Load Game" menuItemY3
    , drawMenuItem app 4 "Exit" menuItemY4
    , translate menuHintX menuHintY
        $ scale menuHintScale menuHintScale
        $ color menuHintColor
        $ Text menuHintText
    ]

drawMenuItem :: App -> Int -> String -> Float -> Picture
drawMenuItem app ix label y =
  translate menuItemX y
    $ scale menuItemScale menuItemScale
    $ color (menuItemColorBy app ix)
    $ Text label

menuItemColorBy :: App -> Int -> Color
menuItemColorBy app ix =
  if menuIx app == ix then menuSelectedColor else menuItemColor

drawControls :: App -> Picture
drawControls app =
  pictures
    [ translate (-160) 80
        $ scale 0.45 0.45
        $ color (makeColorI 200 255 0 255)
        $ Text "Controls"
    , translate (-420) 10
        $ scale 0.20 0.20
        $ color (makeColorI 230 230 230 255)
        $ Text "A/D or Left/Right: move"
    , translate (-420) (-20)
        $ scale 0.20 0.20
        $ color (makeColorI 230 230 230 255)
        $ Text "Space/W/Up: jump"
    , translate (-420) (-50)
        $ scale 0.20 0.20
        $ color (makeColorI 230 230 230 255)
        $ Text "P: pause"
    , translate (-420) (-80)
        $ scale 0.20 0.20
        $ color (makeColorI 230 230 230 255)
        $ Text "T: debug overlay"
    , translate (-420) (-130)
        $ scale 0.20 0.20
        $ color (makeColorI 200 200 200 255)
        $ Text "Enter: continue  |  Q: menu  |  Esc: exit"
    , drawExitTopRight app
    ]

drawPlaying :: Assets -> App -> Picture
drawPlaying assets app =
  pictures
    [ drawWorld assets app
    , drawHudPlaying app
    ]

drawPaused :: Assets -> App -> Picture
drawPaused assets app =
  pictures
    [ drawWorld assets app
    , drawHudPlaying app
    , drawOverlay app
    , drawPauseMenu app
    ]

drawGameOver :: Assets -> App -> Picture
drawGameOver assets app =
  pictures
    [ drawWorld assets app
    , drawOverlay app
    , drawGameOverText app
    , drawNotice app (-260) (gameOverTitleY - 105)
    ]

drawWorld :: Assets -> App -> Picture
drawWorld assets app =
  translate (-cameraX) 0
    $ pictures
      [ drawGroundWithHoles app cameraX holes
      , drawGroundMarks app cameraX holes
      , drawPlatforms plats
      , drawSpikes spikes
      , drawMedkits meds
      , drawPlayer assets app
      ]
  where
    cameraX = worldScroll app - playerBaseX
    holes   = worldHoles app
    plats   = worldPlatforms app
    spikes  = worldSpikes app
    meds    = worldMedkits app

drawHudPlaying :: App -> Picture
drawHudPlaying app =
  pictures
    [ drawDistanceTopLeft app
    , drawLivesBelowDistance app
    , drawDebugBelowLives app
    , drawExitTopRight app
    ]

drawOverlay :: App -> Picture
drawOverlay app =
  color (makeColorI 0 0 0 140)
    $ rectangleSolid (fromIntegral (viewW app)) (fromIntegral (viewH app))

drawPauseMenu :: App -> Picture
drawPauseMenu app =
  pictures
    [ translate 0 0
        $ pictures
          [ color pausePanelColor $ rectangleSolid pausePanelW pausePanelH
          , color pausePanelBorderColor $ rectangleWire pausePanelW pausePanelH
          ]
    , translate pauseTitleX pauseTitleY
        $ scale pauseTitleScale pauseTitleScale
        $ color pauseTitleColor
        $ Text pauseTitleText
    , drawPauseItem app 0 "Resume" pauseItem1Y
    , drawPauseItem app 1 "Save Game" pauseItem2Y
    , drawPauseItem app 2 "Quit to Title" pauseItem3Y
    , translate pauseHintX pauseHintY
        $ scale pauseHintScale pauseHintScale
        $ color pauseHintColor
        $ Text pauseHintText
    ]

drawPauseItem :: App -> Int -> String -> Float -> Picture
drawPauseItem app ix label y =
  translate pauseItemX y
    $ scale pauseItemScale pauseItemScale
    $ color (pauseItemClr app ix)
    $ Text label

pauseItemClr :: App -> Int -> Color
pauseItemClr app ix =
  if pauseIx app == ix then pauseItemSelectedColor else pauseItemColor

pauseTitleX, pauseTitleY, pauseItemX, pauseItem1Y, pauseItem2Y, pauseHintX, pauseHintY :: Float
pauseTitleX = -90
pauseTitleY = 55
pauseItemX  = -150
pauseItem1Y = 10
pauseItem2Y = -28
pauseHintX  = -250
pauseHintY  = -130

drawGameOverText :: App -> Picture
drawGameOverText app =
  pictures
    [ translate gameOverTitleX gameOverTitleY
        $ scale gameOverTitleScale gameOverTitleScale
        $ color gameOverTitleColor
        $ Text gameOverTitleText
    , translate (-260) (gameOverTitleY - 70)
        $ scale 0.22 0.22
        $ color distanceColor
        $ Text ("Distance: " ++ show meters ++ " m")
    , translate gameOverHintX gameOverHintY
        $ scale gameOverHintScale gameOverHintScale
        $ color gameOverHintColor
        $ Text gameOverHintText
    ]
  where
    meters = floor (worldScroll app * metersPerPixel) :: Int

drawDistanceTopLeft :: App -> Picture
drawDistanceTopLeft app =
  translate (screenLeft app + 10) (screenTop app - 30)
    $ scale 0.20 0.20
    $ color distanceColor
    $ Text ("Distance: " ++ show meters ++ " m")
  where
    meters = floor (worldScroll app * metersPerPixel) :: Int

drawLivesBelowDistance :: App -> Picture
drawLivesBelowDistance app =
  pictures [drawHeart i | i <- [1 .. maxLives]]
  where
    baseX = screenLeft app + 20
    baseY = screenTop app - 65

    drawHeart i =
      translate (baseX + dx i) baseY
        $ color (heartColor i)
        $ rectangleSolid heartW heartH

    dx i = fromIntegral (i - 1) * heartSpacing

    heartColor i =
      if i <= playerLives app then heartFullColor else heartEmptyColor

drawDebugBelowLives :: App -> Picture
drawDebugBelowLives app
  | not (appShowDebug app) = Blank
  | otherwise =
      pictures
        [ translate x (y0 - dy * fromIntegral i)
            $ scale s s
            $ color dbgColor
            $ Text dbgLine
        | (i, dbgLine) <- zip [0 :: Int ..] (debugLines app)
        ]
  where
    x        = screenLeft app + 10
    y0       = screenTop app - 95
    dy       = 18
    s        = 0.16
    dbgColor = makeColorI 210 230 255 255

debugLines :: App -> [String]
debugLines app =
  [ "DEBUG (T)"
  , "scroll=" ++ show1 scroll ++ " nextChunkIx=" ++ show (nextChunkIx app)
  , "px=" ++ show1 px ++ " y=" ++ show1 (playerY app) ++ " vy=" ++ show1 (playerVY app)
  , "supported=" ++ show supported
  , "lvl=" ++ show lvl ++ " speed=" ++ show1 speed
  , "p hole/spk/med " ++ pct holeP ++ "/" ++ pct spikeP ++ "/" ++ pct medP
  , "objs H/P/S/M " ++ show nH ++ "/" ++ show nP ++ "/" ++ show nS ++ "/" ++ show nM
  ]
  where
    cfg   = appConfig app
    scroll = worldScroll app
    px    = scroll + playerOffsetX app

    lvl   = difficultyLevel cfg scroll
    speed = speedAtLevel app lvl

    holeP  = objectChance (cfgHoleRule cfg) lvl
    spikeP = objectChance (cfgSpikeRule cfg) lvl
    medP   = objectChance (cfgMedkitRule cfg) lvl

    nH = length (worldHoles app)
    nP = length (worldPlatforms app)
    nS = length (worldSpikes app)
    nM = length (worldMedkits app)

    supported = isSupported px (playerY app) (playerVY app) (worldHoles app) (worldPlatforms app)

speedAtLevel :: App -> Int -> Float
speedAtLevel app lvl =
  speedBase rule + speedGrowth rule * fromIntegral lvl
  where
    cfg = appConfig app
    rule = case appDifficulty app of
      Easy   -> cfgEasySpeed cfg
      Normal -> cfgNormalSpeed cfg
      Hard   -> cfgHardSpeed cfg

show1 :: Float -> String
show1 x = show ((fromIntegral (round (x * 10) :: Int) / 10) :: Float)

pct :: Float -> String
pct p = show (round (p * 100) :: Int) ++ "%"

drawExitTopRight :: App -> Picture
drawExitTopRight app =
  translate (screenRight app - 80) (screenTop app - 30)
    $ scale 0.15 0.15
    $ color (makeColorI 240 240 240 255)
    $ Text "<-"

drawGroundWithHoles :: App -> Float -> [Interval] -> Picture
drawGroundWithHoles app cameraX holes =
  pictures (map drawSeg segs)
  where
    (leftX, rightX) = visibleRange app cameraX
    hs = sortOn intA $ filter (intervalOverlaps leftX rightX) holes
    segs = buildSegments leftX rightX hs

drawSeg :: (Float, Float) -> Picture
drawSeg (x1, x2) =
  translate cx groundY
    $ color groundColor
    $ rectangleSolid w groundHeight
  where
    w  = x2 - x1
    cx = (x1 + x2) / 2

buildSegments :: Float -> Float -> [Interval] -> [(Float, Float)]
buildSegments leftX rightX holes =
  go leftX holes
  where
    go cursor [] =
      if cursor < rightX then [(cursor, rightX)] else []
    go cursor (h : hs)
      | intA h >= rightX =
          if cursor < rightX then [(cursor, rightX)] else []
      | intB h <= cursor =
          go cursor hs
      | otherwise =
          let segEnd   = min (intA h) rightX
              cursor'  = max cursor (intB h)
              segsHere = if cursor < segEnd then [(cursor, segEnd)] else []
           in segsHere ++ go cursor' hs

drawGroundMarks :: App -> Float -> [Interval] -> Picture
drawGroundMarks app cameraX holes =
  pictures [drawMark x | x <- marks, not (isInHole x holes)]
  where
    (leftX, rightX) = visibleRange app cameraX
    k0 = floor (leftX / markSpacing) :: Int
    k1 = ceiling (rightX / markSpacing) :: Int
    marks = [fromIntegral k * markSpacing | k <- [k0 .. k1]]

isInHole :: Float -> [Interval] -> Bool
isInHole x holes = any (intervalContains x) holes

drawMark :: Float -> Picture
drawMark x =
  translate x (groundY + groundHeight / 2 - markH / 2)
    $ color markColor
    $ rectangleSolid markW markH
  where
    markW = 10
    markH = 8

drawPlatforms :: [Rect] -> Picture
drawPlatforms plats = pictures (map drawPlat plats)
  where
    drawPlat r =
      translate (rectX r) (rectY r)
        $ color platformColor
        $ rectangleSolid (rectW r) (rectH r)

drawSpikes :: [Rect] -> Picture
drawSpikes spikes = pictures (map drawSpike spikes)

drawSpike :: Rect -> Picture
drawSpike r =
  color spikeColor
    $ polygon
      [ (rectLeft r, rectBottom r)
      , (rectRight r, rectBottom r)
      , (rectX r, rectTop r)
      ]

drawMedkits :: [Rect] -> Picture
drawMedkits meds = pictures (map drawMedkit meds)

drawMedkit :: Rect -> Picture
drawMedkit r =
  translate (rectX r) (rectY r)
    $ pictures
      [ color medkitColor $ rectangleSolid w h
      , color medkitCrossColor $ rectangleSolid (w * 0.65) (h * 0.18)
      , color medkitCrossColor $ rectangleSolid (w * 0.18) (h * 0.65)
      ]
  where
    w = rectW r
    h = rectH r

drawPlayer :: Assets -> App -> Picture
drawPlayer assets app =
  translate px (playerY app)
    $ scale s s
    $ color (makeColorI 255 255 255 alpha) playerPic
  where
    px = worldScroll app + playerOffsetX app
    s  = spriteScale

    playerPic =
      if playerIsInAir app
        then assetsPlayerJump assets
        else pickRunFrame assets app

    alpha =
      if playerInvTimer app > 0 && blinkOff (playerInvTimer app)
        then 120
        else 255

spriteScale :: Float
spriteScale = min (playerWidth / playerSpritePxW) (playerHeight / playerSpritePxH)

pickRunFrame :: Assets -> App -> Picture
pickRunFrame assets app =
  assetsPlayerRun assets !! ix
  where
    n  = length (assetsPlayerRun assets)
    ix = (floor (worldScroll app / playerRunFrameStep) :: Int) `mod` n

blinkOff :: Float -> Bool
blinkOff inv = odd (floor (inv * invBlinkHz) :: Int)

playerIsInAir :: App -> Bool
playerIsInAir app = not (isSupported px (playerY app) (playerVY app) holes plats)
  where
    px    = worldScroll app + playerOffsetX app
    holes = worldHoles app
    plats = worldPlatforms app

visibleRange :: App -> Float -> (Float, Float)
visibleRange app cameraX =
  (cameraX - halfW - margin, cameraX + halfW + margin)
  where
    halfW  = fromIntegral (viewW app) / 2
    margin = 200

screenLeft, screenRight, screenTop :: App -> Float
screenLeft  app = -fromIntegral (viewW app) / 2
screenRight app =  fromIntegral (viewW app) / 2
screenTop   app =  fromIntegral (viewH app) / 2

drawLoadGame :: App -> Picture
drawLoadGame app =
  pictures
    [ translate (-220) 100
        $ scale 0.45 0.45
        $ color (makeColorI 200 255 0 255)
        $ Text "Load Game"
    , translate (-380) (-200)
        $ scale 0.20 0.20
        $ color (makeColorI 200 200 200 255)
        $ Text "Up/Down: slot  |  Enter: load  |  Backspace: menu"
    , drawSaveSlots app
    , drawNotice app (-420) (-180)
    , drawExitTopRight app
    ]

drawSaveGame :: App -> Picture
drawSaveGame app =
  pictures
    [ translate (-220) 120
        $ scale 0.45 0.45
        $ color (makeColorI 240 240 240 255)
        $ Text "Save Game"
    , translate (-420) 82
        $ scale 0.20 0.20
        $ color (makeColorI 200 200 200 255)
        $ Text "Up/Down: slot  |  Enter: save  |  Backspace: back"
    , drawSaveSlots app
    , drawNotice app (-420) (-180)
    , drawExitTopRight app
    ]

drawSaveSlots :: App -> Picture
drawSaveSlots app =
  pictures
    [ translate (-420) (y0 - dy * fromIntegral i)
        $ scale 0.18 0.18
        $ color (slotColor app i)
        $ Text (formatSlot (i + 1) (lookupSave (i + 1) (appSaves app)))
    | i <- [0 .. saveSlotsCount - 1]
    ]
  where
    y0 = 40
    dy = 26

slotColor :: App -> Int -> Color
slotColor app ix =
  if appSlotIx app == ix then menuSelectedColor else menuItemColor

lookupSave :: Int -> [SaveRow] -> Maybe SaveRow
lookupSave slot rows = find (\r -> saveSlot r == slot) rows

formatSlot :: Int -> Maybe SaveRow -> String
formatSlot slot mRow =
  case mRow of
    Nothing ->
      "Slot " ++ show slot ++ ": empty"
    Just r  ->
      "Slot " ++ show slot ++ ": "
        ++ show meters ++ " m  "
        ++ "Lives " ++ show (saveLives r) ++ "  "
        ++ saveDifficulty r ++ "  "
        ++ saveCreatedAt r
      where
        meters = floor (saveWorldScroll r * metersPerPixel) :: Int

drawLeaderboard :: App -> Picture
drawLeaderboard app =
  pictures
    [ translate (-180) 120
        $ scale 0.45 0.45
        $ color (makeColorI 200 255 0 255)
        $ Text "Leaderboard"
    , translate (-420) 60
        $ scale 0.20 0.20
        $ color (makeColorI 200 255 0 255)
        $ Text "Top 10 (Backspace: menu)"
    , drawLeaderboardRows app
    , drawNotice app (-420) (-180)
    , drawExitTopRight app
    ]

drawLeaderboardRows :: App -> Picture
drawLeaderboardRows app =
  pictures
    [ translate (-420) (y0 - dy * fromIntegral i - 30)
        $ scale 0.18 0.18
        $ color (makeColorI 230 230 230 255)
        $ Text (formatRow (i + 1) row)
    | (i, row) <- zip [0 :: Int ..] (appLeaderboard app)
    ]
  where
    y0 = 50
    dy = 35

formatRow :: Int -> ScoreRow -> String
formatRow pos r =
  printf "[%-2d] %6s m  %-10s  %-20s"
         pos
         (show (scoreDistance r))
         (scoreDifficulty r)
         (scorePlayerName r)

drawNotice :: App -> Float -> Float -> Picture
drawNotice app x y =
  case appNotice app of
    Nothing   -> Blank
    Just msg  ->
      translate x y
        $ scale 0.16 0.16
        $ color (makeColorI 255 200 200 255)
        $ Text msg

drawNameEntry :: App -> Picture
drawNameEntry app =
  pictures
    [ translate (-220) 110
        $ scale 0.42 0.42
        $ color (makeColorI 200 255 0 255)
        $ Text "Enter Name"
    , translate (-420) 50
        $ scale 0.20 0.20
        $ color (makeColorI 220 220 220 255)
        $ Text ("Max " ++ show playerNameMaxLen ++ " chars. Allowed: A-Z 0-9 space _ -")
    , translate (-420) 10
        $ scale 0.28 0.28
        $ color (makeColorI 255 255 255 255)
        $ Text (appNameInput app ++ "_")
    , translate (-320) (-200)
        $ scale 0.20 0.20
        $ color (makeColorI 200 200 200 255)
        $ Text "Backspace: delete  |  Enter: start  |  Q: menu"
    , drawExitTopRight app
    ]