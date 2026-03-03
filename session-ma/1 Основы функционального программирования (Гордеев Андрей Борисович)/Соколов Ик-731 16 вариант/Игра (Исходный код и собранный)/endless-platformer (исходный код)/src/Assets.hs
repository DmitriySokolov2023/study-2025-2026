module Assets
  ( Assets (..)
  , loadAssets
  ) where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)

data Assets = Assets
  { assetsPlayerRun :: [Picture]
  , assetsPlayerJump :: Picture
  }

loadAssets :: IO (Either String Assets)
loadAssets = do
  eRun <- loadMany runFiles
  eJump <- loadPng jumpFile
  pure $ do
    runPics <- eRun
    jumpPic <- eJump
    if null runPics
      then Left "Asset error: no player run frames loaded."
      else Right Assets
        { assetsPlayerRun = runPics
        , assetsPlayerJump = jumpPic
        }

runFiles :: [FilePath]
runFiles =
  [ "assets/player_run_1.png"
  , "assets/player_run_2.png"
  , "assets/player_run_3.png"
  , "assets/player_run_4.png"
  , "assets/player_run_5.png"
  , "assets/player_run_6.png"
  , "assets/player_run_7.png"
  , "assets/player_run_8.png"
  ]

jumpFile :: FilePath
jumpFile = "assets/player_jump.png"

loadMany :: [FilePath] -> IO (Either String [Picture])
loadMany rels = sequence <$> mapM loadPng rels

loadPng :: FilePath -> IO (Either String Picture)
loadPng rel = do
  ePath <- resolveAssetPath rel
  case ePath of
    Left err -> pure (Left err)
    Right path -> do
      mp <- loadJuicyPNG path
      pure $
        case mp of
          Nothing -> Left ("Asset error: cannot decode PNG: " ++ rel)
          Just p -> Right p

resolveAssetPath :: FilePath -> IO (Either String FilePath)
resolveAssetPath rel = do
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
    ( [ "Asset error: cannot find file: " ++ rel
      , "Tried:"
      ]
        ++ map ("  - " ++) candidates
        ++ [ "Put 'assets/' next to the exe or run from project root."
           ]
    )