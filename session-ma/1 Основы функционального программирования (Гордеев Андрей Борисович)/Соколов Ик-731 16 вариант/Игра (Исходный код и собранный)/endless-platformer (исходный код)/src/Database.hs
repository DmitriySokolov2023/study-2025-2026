{-# LANGUAGE OverloadedStrings #-}

module Database
  ( ScoreRow(..)
  , SaveRow(..)
  , initDb
  , insertScore
  , getTopScores
  , upsertSave
  , loadSave
  , listSaves
  ) where

import Control.Exception (SomeException, bracket, displayException, try)
import Database.SQLite.Simple
  ( Connection
  , Only(..)
  , Query
  , close
  , execute
  , execute_
  , open
  , query
  , query_
  )
import Database.SQLite.Simple.FromRow (FromRow(..), field)

data ScoreRow = ScoreRow
  { scoreId         :: Int
  , scorePlayerName :: String
  , scoreDistance   :: Int
  , scoreDifficulty :: String
  , scoreCreatedAt  :: String
  }
  deriving (Eq, Show)

instance FromRow ScoreRow where
  fromRow = ScoreRow <$> field <*> field <*> field <*> field <*> field

data SaveRow = SaveRow
  { saveSlot       :: Int
  , saveSeed       :: Int
  , saveWorldScroll :: Float
  , saveLives      :: Int
  , saveDifficulty :: String
  , saveCreatedAt  :: String
  }
  deriving (Eq, Show)

instance FromRow SaveRow where
  fromRow = SaveRow <$> field <*> field <*> field <*> field <*> field <*> field

initDb :: FilePath -> IO (Either String ())
initDb path =
  fmap (fmap (const ())) $
    withConn path $ \conn -> do
      execute_ conn "PRAGMA foreign_keys = ON;"
      execute_ conn createScoresSql
      execute_ conn createSavesSql

insertScore :: FilePath -> String -> Int -> String -> IO (Either String ())
insertScore path name dist diff =
  fmap (fmap (const ())) $
    withConn path $ \conn ->
      execute conn
        "INSERT INTO scores(player_name, distance, difficulty) VALUES (?,?,?)"
        (name, dist, diff)

getTopScores :: FilePath -> Int -> IO (Either String [ScoreRow])
getTopScores path limitN =
  withConn path $ \conn ->
    query conn
      "SELECT id, player_name, distance, difficulty, created_at \
      \FROM scores ORDER BY distance DESC, id DESC LIMIT ?"
      (Only limitN)

upsertSave :: FilePath -> Int -> Int -> Float -> Int -> String -> IO (Either String ())
upsertSave path slot seed scroll lives diff =
  fmap (fmap (const ())) $
    withConn path $ \conn ->
      execute conn
        "INSERT OR REPLACE INTO saves(slot, seed, world_scroll, lives, difficulty) \
        \VALUES (?,?,?,?,?)"
        (slot, seed, scroll, lives, diff)

loadSave :: FilePath -> Int -> IO (Either String SaveRow)
loadSave path slot = do
  eRows <-
    withConn path $ \conn ->
      query conn
        "SELECT slot, seed, world_scroll, lives, difficulty, created_at \
        \FROM saves WHERE slot = ?"
        (Only slot)
  pure $ do
    rows <- eRows
    expectOne ("No save in slot " ++ show slot ++ ".") rows

listSaves :: FilePath -> IO (Either String [SaveRow])
listSaves path =
  withConn path $ \conn ->
    query_ conn
      "SELECT slot, seed, world_scroll, lives, difficulty, created_at \
      \FROM saves ORDER BY slot"

expectOne :: String -> [a] -> Either String a
expectOne msg xs =
  case xs of
    [x] -> Right x
    []  -> Left msg
    _   -> Left "DB error: expected one row, got many."

withConn :: FilePath -> (Connection -> IO a) -> IO (Either String a)
withConn path action = do
  r <- try (bracket (open path) close action)
  pure $
    case r of
      Left e  -> Left (dbErr path e)
      Right x -> Right x

dbErr :: FilePath -> SomeException -> String
dbErr path e =
  unlines
    [ "DB error: " ++ path
    , displayException e
    ]

createScoresSql :: Query
createScoresSql =
  "CREATE TABLE IF NOT EXISTS scores (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ player_name TEXT NOT NULL,\
  \ distance INTEGER NOT NULL,\
  \ difficulty TEXT NOT NULL,\
  \ created_at TEXT NOT NULL DEFAULT (datetime('now'))\
  \);"

createSavesSql :: Query
createSavesSql =
  "CREATE TABLE IF NOT EXISTS saves (\
  \ slot INTEGER PRIMARY KEY,\
  \ seed INTEGER NOT NULL,\
  \ world_scroll REAL NOT NULL,\
  \ lives INTEGER NOT NULL,\
  \ difficulty TEXT NOT NULL,\
  \ created_at TEXT NOT NULL DEFAULT (datetime('now'))\
  \);"