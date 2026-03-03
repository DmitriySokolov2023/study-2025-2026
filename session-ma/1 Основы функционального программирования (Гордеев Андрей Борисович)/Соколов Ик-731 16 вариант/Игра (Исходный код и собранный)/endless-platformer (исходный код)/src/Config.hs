{-# LANGUAGE OverloadedStrings #-}

module Config
  ( GenConfig(..)
  , SpeedRule(..)
  , ObjectRule(..)
  , loadConfig
  , difficultyLevel
  , objectChance
  ) where

import Control.Exception (IOException, catch, displayException)
import Data.Aeson (FromJSON(parseJSON), eitherDecode', withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)

data SpeedRule = SpeedRule
  { speedBase :: Float
  , speedGrowth :: Float
  }
  deriving (Eq, Show)

data ObjectRule = ObjectRule
  { objEnabled :: Bool
  , objMinDifficulty :: Int
  , objBaseRate :: Float
  , objGrowth :: Float
  }
  deriving (Eq, Show)

data GenConfig = GenConfig
  { cfgDifficultyStepPx :: Float
  , cfgSafeChunks :: Int
  , cfgEasySpeed :: SpeedRule
  , cfgNormalSpeed :: SpeedRule
  , cfgHardSpeed :: SpeedRule
  , cfgHoleRule :: ObjectRule
  , cfgSpikeRule :: ObjectRule
  , cfgMedkitRule :: ObjectRule
  , cfgBridgeBaseChance :: Float
  , cfgBridgeDecayPerLevel :: Float
  , cfgExtraPlatformChance :: Float
  }
  deriving (Eq, Show)

instance FromJSON SpeedRule where
  parseJSON = withObject "SpeedRule" $ \o ->
    SpeedRule <$> o .: "base" <*> o .: "growth"

instance FromJSON ObjectRule where
  parseJSON = withObject "ObjectRule" $ \o ->
    ObjectRule
      <$> o .: "enabled"
      <*> o .: "minDifficulty"
      <*> o .: "baseRate"
      <*> o .: "growth"

instance FromJSON GenConfig where
  parseJSON = withObject "GenConfig" $ \o -> do
    stepPx <- o .: "difficultyStepPx"
    safeChunks <- o .: "safeChunks"

    speedsObj <- o .: "speeds"
    easySp <- speedsObj .: "easy"
    normalSp <- speedsObj .: "normal"
    hardSp <- speedsObj .: "hard"

    objs <- o .: "objects"
    hole <- objs .: "hole"
    spike <- objs .: "spike"
    medkit <- objs .: "medkit"

    bridgeBase <- o .: "bridgeBaseChance"
    bridgeDecay <- o .: "bridgeDecayPerLevel"
    extraPlat <- o .: "extraPlatformChance"

    pure
      GenConfig
        { cfgDifficultyStepPx = stepPx
        , cfgSafeChunks = safeChunks
        , cfgEasySpeed = easySp
        , cfgNormalSpeed = normalSp
        , cfgHardSpeed = hardSp
        , cfgHoleRule = hole
        , cfgSpikeRule = spike
        , cfgMedkitRule = medkit
        , cfgBridgeBaseChance = bridgeBase
        , cfgBridgeDecayPerLevel = bridgeDecay
        , cfgExtraPlatformChance = extraPlat
        }

loadConfig :: FilePath -> IO (Either String GenConfig)
loadConfig rel = do
  ePath <- resolveConfigPath rel
  case ePath of
    Left err -> pure (Left err)
    Right path -> do
      eBs <- readFileEither path
      case eBs of
        Left err -> pure (Left err)
        Right bs ->
          case eitherDecode' bs of
            Left parseErr ->
              pure
                ( Left
                    ( unlines
                        [ "Config error: cannot parse JSON: " ++ path
                        , parseErr
                        ]
                    )
                )
            Right cfg ->
              pure (validateConfig cfg)

difficultyLevel :: GenConfig -> Float -> Int
difficultyLevel cfg distPx =
  floor (distPx / cfgDifficultyStepPx cfg)

objectChance :: ObjectRule -> Int -> Float
objectChance rule lvl
  | not (objEnabled rule) = 0
  | lvl < objMinDifficulty rule = 0
  | otherwise =
      clamp01
        ( objBaseRate rule
            + objGrowth rule
              * fromIntegral (lvl - objMinDifficulty rule)
        )

validateConfig :: GenConfig -> Either String GenConfig
validateConfig cfg = do
  checkPos "difficultyStepPx" (cfgDifficultyStepPx cfg)
  checkNonNegInt "safeChunks" (cfgSafeChunks cfg)

  checkSpeed "speeds.easy" (cfgEasySpeed cfg)
  checkSpeed "speeds.normal" (cfgNormalSpeed cfg)
  checkSpeed "speeds.hard" (cfgHardSpeed cfg)

  checkObj "objects.hole" (cfgHoleRule cfg)
  checkObj "objects.spike" (cfgSpikeRule cfg)
  checkObj "objects.medkit" (cfgMedkitRule cfg)

  checkRate "bridgeBaseChance" (cfgBridgeBaseChance cfg)
  checkNonNeg "bridgeDecayPerLevel" (cfgBridgeDecayPerLevel cfg)
  checkRate "extraPlatformChance" (cfgExtraPlatformChance cfg)

  pure cfg

checkSpeed :: String -> SpeedRule -> Either String ()
checkSpeed prefix s = do
  checkPos (prefix ++ ".base") (speedBase s)
  checkNonNeg (prefix ++ ".growth") (speedGrowth s)

checkObj :: String -> ObjectRule -> Either String ()
checkObj prefix o = do
  checkNonNegInt (prefix ++ ".minDifficulty") (objMinDifficulty o)
  checkRate (prefix ++ ".baseRate") (objBaseRate o)
  checkNonNeg (prefix ++ ".growth") (objGrowth o)

checkPos :: String -> Float -> Either String ()
checkPos name x =
  if x > 0
    then Right ()
    else Left ("Config error: " ++ name ++ " must be > 0.")

checkNonNeg :: String -> Float -> Either String ()
checkNonNeg name x =
  if x >= 0
    then Right ()
    else Left ("Config error: " ++ name ++ " must be >= 0.")

checkRate :: String -> Float -> Either String ()
checkRate name x
  | x < 0 = Left ("Config error: " ++ name ++ " must be >= 0.")
  | x > 1 = Left ("Config error: " ++ name ++ " must be <= 1.")
  | otherwise = Right ()

checkNonNegInt :: String -> Int -> Either String ()
checkNonNegInt name x =
  if x >= 0
    then Right ()
    else Left ("Config error: " ++ name ++ " must be >= 0.")

resolveConfigPath :: FilePath -> IO (Either String FilePath)
resolveConfigPath rel = do
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
  let candidates = [exeDir </> rel, rel]
  pickExisting rel candidates

pickExisting :: FilePath -> [FilePath] -> IO (Either String FilePath)
pickExisting rel candidates =
  go candidates
  where
    go [] = pure (Left (missingMsg rel candidates))
    go (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Right p) else go ps

missingMsg :: FilePath -> [FilePath] -> String
missingMsg rel candidates =
  unlines
    ( [ "Config error: cannot find file: " ++ rel
      , "Tried:"
      ]
        ++ map ("  - " ++) candidates
        ++ [ "Put the config next to the exe or run from project root."
           ]
    )

readFileEither :: FilePath -> IO (Either String BL.ByteString)
readFileEither path =
  (Right <$> BL.readFile path) `catch` handle
  where
    handle :: IOException -> IO (Either String BL.ByteString)
    handle e =
      pure
        ( Left
            ( unlines
                [ "Config error: cannot read file: " ++ path
                , displayException e
                ]
            )
        )

clamp01 :: Float -> Float
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x