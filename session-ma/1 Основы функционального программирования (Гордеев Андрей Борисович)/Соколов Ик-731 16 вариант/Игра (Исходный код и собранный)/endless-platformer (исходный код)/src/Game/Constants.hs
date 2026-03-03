module Game.Constants
  (
    windowTitle, windowWidth, windowHeight, windowPos,
    backgroundColor, fps,

    titleText, titleX, titleY, titleScale, titleColor,
    menuItemX, menuItemY0, menuItemY1, menuItemY2, menuItemY3, menuItemY4,
    menuItemScale, menuItemColor, menuSelectedColor,
    menuHintText, menuHintX, menuHintY, menuHintScale, menuHintColor,

    playerWidth, playerHeight, playerStartY, playerBaseX,
    steerRange, invBlinkHz, moveSpeed,
    runSpeedEasy, runSpeedNormal, runSpeedHard,
    metersPerPixel, gravity, jumpVelocity,
    playerSpritePxW, playerSpritePxH, playerRunFrameStep,

    groundY, groundHeight, groundTopY, groundColor,
    markSpacing, markColor, distanceColor,
    deathY, maxLives, invincibilityDuration,
    heartW, heartH, heartSpacing, heartFullColor, heartEmptyColor,

    chunkWidth, spawnAhead, despawnBehind,
    holeWidth,
    platformW, platformH, platformLift, platformEdgeInset, platformColor,
    spikeW, spikeH, spikeColor,
    medkitW, medkitH, medkitLift, medkitColor, medkitCrossColor,

    gameOverTitleText, gameOverTitleX, gameOverTitleY,
    gameOverTitleScale, gameOverTitleColor,
    gameOverHintText, gameOverHintX, gameOverHintY,
    gameOverHintScale, gameOverHintColor,

    pauseTitleText,
    pausePanelW, pausePanelH, pausePanelColor, pausePanelBorderColor,
    pauseTitleScale, pauseTitleColor,
    pauseItemScale, pauseItemSelectedColor, pauseItemColor,
    pauseHintText, pauseHintScale, pauseHintColor,
    pauseItem3Y,

    dbFileName, leaderboardLimit,
    defaultPlayerName, playerNameMaxLen,
    scoreSavedText, saveSlotsCount, defaultSaveSeed
  ) where

import Graphics.Gloss (Color, makeColorI)

windowTitle :: String
windowTitle = "Endless Runner"

windowWidth :: Int
windowWidth = 960

windowHeight :: Int
windowHeight = 540

windowPos :: (Int, Int)
windowPos = (100, 100)

backgroundColor :: Color
backgroundColor = makeColorI 15 20 25 255

fps :: Int
fps = 60

titleText :: String
titleText = "Endless Runner"

titleX :: Float
titleX = -330

titleY :: Float
titleY = 120

titleScale :: Float
titleScale = 0.6

titleColor :: Color
titleColor = makeColorI 220 235 250 255 

menuItemX :: Float
menuItemX = -325

menuItemY0, menuItemY1, menuItemY2, menuItemY3, menuItemY4 :: Float
menuItemY0 = 40
menuItemY1 = 5
menuItemY2 = -30
menuItemY3 = -65
menuItemY4 = -100

menuItemScale :: Float
menuItemScale = 0.26

menuItemColor :: Color
menuItemColor = makeColorI 200 215 230 255 

menuSelectedColor :: Color
menuSelectedColor = makeColorI 200 255 0 255

menuHintText :: String
menuHintText =
  "Up/Down: choose  |  Left/Right: LEVEL  |  Enter: select  |  Esc: exit"

menuHintX :: Float
menuHintX = -440

menuHintY :: Float
menuHintY = -170

menuHintScale :: Float
menuHintScale = 0.18

menuHintColor :: Color
menuHintColor =  makeColorI 200 215 230 255 

playerWidth, playerHeight :: Float
playerWidth = 40
playerHeight = 60

playerStartY :: Float
playerStartY = groundTopY + playerHeight / 2   

playerBaseX :: Float
playerBaseX = -300   

steerRange :: Float
steerRange = 140      

invBlinkHz :: Float
invBlinkHz = 12       

moveSpeed :: Float
moveSpeed = 260       

runSpeedEasy, runSpeedNormal, runSpeedHard :: Float
runSpeedEasy   = 280
runSpeedNormal = 320
runSpeedHard   = 380

metersPerPixel :: Float
metersPerPixel = 0.10   

gravity :: Float
gravity = 1200

jumpVelocity :: Float
jumpVelocity = 520

playerSpritePxW, playerSpritePxH :: Float
playerSpritePxW = 180
playerSpritePxH = 180

playerRunFrameStep :: Float
playerRunFrameStep = 28   

groundY :: Float
groundY = -200

groundHeight :: Float
groundHeight = 20

groundTopY :: Float
groundTopY = groundY + groundHeight / 2

groundColor :: Color
groundColor = makeColorI 80 170 80 255   

markSpacing :: Float
markSpacing = 120      

markColor :: Color
markColor = makeColorI 120 220 120 255   

distanceColor :: Color
distanceColor = makeColorI 240 240 240 255  

deathY :: Float
deathY = -300            

maxLives :: Int
maxLives = 3

invincibilityDuration :: Float
invincibilityDuration = 0.8   

heartW, heartH, heartSpacing :: Float
heartW = 18
heartH = 14
heartSpacing = 24

heartFullColor, heartEmptyColor :: Color
heartFullColor  = makeColorI 220 80 80 255    
heartEmptyColor = makeColorI 70 70 70 255     

chunkWidth :: Float
chunkWidth = 700

spawnAhead :: Float
spawnAhead = 2000      

despawnBehind :: Float
despawnBehind = 800    

holeWidth :: Float
holeWidth = 160

platformW, platformH, platformLift :: Float
platformW = 180
platformH = 18
platformLift = 50      

platformEdgeInset :: Float
platformEdgeInset = 0   

platformColor :: Color
platformColor = makeColorI 120 120 220 255   

spikeW, spikeH :: Float
spikeW = 40
spikeH = 35

spikeColor :: Color
spikeColor = makeColorI 220 80 80 255        

medkitW, medkitH, medkitLift :: Float
medkitW = 26
medkitH = 26
medkitLift = 70         

medkitColor, medkitCrossColor :: Color
medkitColor      = makeColorI 80 200 120 255   
medkitCrossColor = makeColorI 240 240 240 255  

gameOverTitleText :: String
gameOverTitleText = "Game Over"

gameOverTitleX, gameOverTitleY :: Float
gameOverTitleX = -170
gameOverTitleY = 80

gameOverTitleScale :: Float
gameOverTitleScale = 0.5

gameOverTitleColor :: Color
gameOverTitleColor = makeColorI 255 0 0 0   

gameOverHintText :: String
gameOverHintText = "Enter: restart  |  Backspace: menu  |  Esc: quit"

gameOverHintX, gameOverHintY :: Float
gameOverHintX = -360
gameOverHintY = -250

gameOverHintScale :: Float
gameOverHintScale = 0.2

gameOverHintColor :: Color
gameOverHintColor = makeColorI 240 240 240 255    

pauseTitleText :: String
pauseTitleText = "Paused"

pausePanelW, pausePanelH :: Float
pausePanelW = 600
pausePanelH = 300

pausePanelColor, pausePanelBorderColor :: Color
pausePanelColor       = makeColorI 30 30 30 235      
pausePanelBorderColor = makeColorI 220 220 220 255   

pauseTitleScale :: Float
pauseTitleScale = 0.35

pauseTitleColor :: Color
pauseTitleColor = makeColorI 200 255 0 255

pauseItemScale :: Float
pauseItemScale = 0.24

pauseItemSelectedColor, pauseItemColor :: Color
pauseItemSelectedColor = makeColorI 255 255 255 255   
pauseItemColor         = makeColorI 180 180 180 255   

pauseHintText :: String
pauseHintText = "Up/Down: choose  |  Enter: select  |  P: resume"

pauseHintScale :: Float
pauseHintScale = 0.14

pauseHintColor :: Color
pauseHintColor = makeColorI 160 160 160 255

pauseItem3Y :: Float
pauseItem3Y = -70    

dbFileName :: FilePath
dbFileName = "game.db"

leaderboardLimit :: Int
leaderboardLimit = 10

defaultPlayerName :: String
defaultPlayerName = "Player"

playerNameMaxLen :: Int
playerNameMaxLen = 16

scoreSavedText :: String
scoreSavedText = "Score saved to leaderboard."

saveSlotsCount :: Int
saveSlotsCount = 3

defaultSaveSeed :: Int
defaultSaveSeed = 0