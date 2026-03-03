module Generator
  ( ensureChunks
  , generateChunk
  , chunkStartX
  ) where

import Config
  ( GenConfig(..)
  , difficultyLevel
  , objectChance
  )
import Game.Constants
  ( chunkWidth
  , groundTopY
  , holeWidth
  , medkitH
  , medkitLift
  , medkitW
  , platformH
  , platformLift
  , platformW
  , spawnAhead
  , spikeH
  , spikeW
  )
import Geometry
  ( Interval(..)
  , Rect(..)
  , intervalContains
  , rectLeft
  , rectRight
  , rectTop
  )

data ChunkTemplate
  = TemplateA
  | TemplateB
  | TemplateC
  deriving (Eq, Show)

ensureChunks
  :: GenConfig
  -> Float
  -> Int
  -> [Interval]
  -> [Rect]
  -> [Rect]
  -> [Rect]
  -> (Int, [Interval], [Rect], [Rect], [Rect])
ensureChunks cfg scroll ix holes plats spikes meds
  | chunkStartX ix < scroll + spawnAhead =
      ensureChunks cfg scroll (ix + 1)
        (holes ++ hs)
        (plats ++ ps)
        (spikes ++ ss)
        (meds ++ ms)
  | otherwise =
      (ix, holes, plats, spikes, meds)
  where
    (hs, ps, ss, ms) = generateChunk cfg ix

chunkStartX :: Int -> Float
chunkStartX ix = fromIntegral ix * chunkWidth

generateChunk :: GenConfig -> Int -> ([Interval], [Rect], [Rect], [Rect])
generateChunk cfg ix
  | ix < cfgSafeChunks cfg = ([], [], [], [])
  | otherwise =
      case templateFor ix of
        TemplateA -> genFromTemplate cfg ix specA
        TemplateB -> genFromTemplate cfg ix specB
        TemplateC -> genFromTemplate cfg ix specC

templateFor :: Int -> ChunkTemplate
templateFor ix =
  case ix `mod` 3 of
    0 -> TemplateA
    1 -> TemplateB
    _ -> TemplateC

data TemplateSpec = TemplateSpec
  { tsHoleStartOff  :: Float
  , tsHoleExtraStep :: Float
  , tsSpikeOffs     :: [Float]
  , tsMedkitOffs    :: [Float]
  , tsExtraPlatXOff :: Float
  }

specA :: TemplateSpec
specA = TemplateSpec
  { tsHoleStartOff  = 240
  , tsHoleExtraStep = 40
  , tsSpikeOffs     = [320, 520]
  , tsMedkitOffs    = [430]
  , tsExtraPlatXOff = 560
  }

specB :: TemplateSpec
specB = TemplateSpec
  { tsHoleStartOff  = 220
  , tsHoleExtraStep = 40
  , tsSpikeOffs     = [320, 480, 630]
  , tsMedkitOffs    = [410, 590]
  , tsExtraPlatXOff = 560
  }

specC :: TemplateSpec
specC = TemplateSpec
  { tsHoleStartOff  = 260
  , tsHoleExtraStep = 60
  , tsSpikeOffs     = [260, 420, 580]
  , tsMedkitOffs    = [350, 520]
  , tsExtraPlatXOff = 500
  }

genFromTemplate
  :: GenConfig
  -> Int
  -> TemplateSpec
  -> ([Interval], [Rect], [Rect], [Rect])
genFromTemplate cfg ix spec =
  (holes, plats, spikes, medkits)
  where
    baseX = chunkStartX ix
    lvl   = difficultyLevel cfg baseX

    holeP   = objectChance (cfgHoleRule cfg) lvl
    spikeP  = objectChance (cfgSpikeRule cfg) lvl
    medP    = objectChance (cfgMedkitRule cfg) lvl

    hasHole    = rand01 ix 1 < holeP
    holeExtra  = fromIntegral (randBound ix 2 3) * tsHoleExtraStep spec
    holeW      = holeWidth + holeExtra
    holeStart  = baseX + tsHoleStartOff spec
    holeEnd    = holeStart + holeW

    holes =
      if hasHole
        then [Interval holeStart holeEnd]
        else []

    platY1 = groundTopY + platformLift + platformH / 2
    platY2 = platY1 + 70

    bridgeChance =
      clamp01
        ( cfgBridgeBaseChance cfg
          - fromIntegral lvl * cfgBridgeDecayPerLevel cfg
        )

    makeBridge = hasHole && rand01 ix 3 < bridgeChance
    bridgeX    = (holeStart + holeEnd) / 2

    extraChance = cfgExtraPlatformChance cfg
    makeExtra   = rand01 ix 4 < extraChance
    extraX      = baseX + tsExtraPlatXOff spec

    plats =
      filter (\r -> rectW r > 0)
        [ if makeBridge
            then Rect bridgeX platY1 (max platformW (holeW + 40)) platformH
            else Rect 0 0 0 0
        , if makeExtra
            then
              Rect extraX
                (if rand01 ix 5 < 0.35 then platY2 else platY1)
                platformW
                platformH
            else Rect 0 0 0 0
        ]

    spikes =
      concat
        [ spikeAt plats holes (baseX + off) (rand01 ix (10 + k) < spikeP)
        | (k, off) <- zip [0 ..] (tsSpikeOffs spec)
        ]

    medkits =
      concat
        [ medkitAt plats holes (baseX + off) (rand01 ix (20 + k) < medP)
        | (k, off) <- zip [0 ..] (tsMedkitOffs spec)
        ]

spikeAt :: [Rect] -> [Interval] -> Float -> Bool -> [Rect]
spikeAt plats holes x enabled
  | not enabled = []
  | any (intervalContains x) holes = []
  | otherwise = [Rect x spikeY spikeW spikeH]
  where
    spikeY =
      case platformUnder x plats of
        Nothing -> groundTopY + spikeH / 2
        Just r  -> rectTop r + spikeH / 2

medkitAt :: [Rect] -> [Interval] -> Float -> Bool -> [Rect]
medkitAt plats holes x enabled
  | not enabled = []
  | otherwise =
      case platformUnder x plats of
        Just r  -> [Rect x (rectTop r + medkitLift) medkitW medkitH]
        Nothing ->
          if any (intervalContains x) holes
            then []
            else [Rect x (groundTopY + medkitLift) medkitW medkitH]

platformUnder :: Float -> [Rect] -> Maybe Rect
platformUnder x plats =
  case [r | r <- plats, x >= rectLeft r, x <= rectRight r] of
    []     -> Nothing
    r : rs -> Just (foldl pick r rs)
  where
    pick a b = if rectTop b > rectTop a then b else a

randBound :: Int -> Int -> Int -> Int
randBound ix salt bound =
  fromInteger (mix `mod` toInteger bound)
  where
    x   = toInteger ix
    s   = toInteger salt
    mix = (x * 1103515245 + s * 12345 + 1013904223) `mod` 2147483647

rand01 :: Int -> Int -> Float
rand01 ix salt =
  fromIntegral (randBound ix salt denom) / fromIntegral denom
  where
    denom = 100000

clamp01 :: Float -> Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x