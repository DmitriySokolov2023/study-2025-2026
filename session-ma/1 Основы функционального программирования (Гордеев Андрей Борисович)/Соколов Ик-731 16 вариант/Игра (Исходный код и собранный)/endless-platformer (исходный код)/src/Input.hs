module Input
  ( handleInputEvent
  ) where

import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey)
  , Key (Char, SpecialKey)
  , KeyState (Down)
  , SpecialKey (KeyLeft, KeyRight, KeySpace, KeyUp)
  )
import World (InputState (..))

handleInputEvent :: Event -> InputState -> InputState
handleInputEvent ev st =
  case ev of
    EventKey key keyState _ _
      | isLeftKey key -> st {inputLeft = keyState == Down}
      | isRightKey key -> st {inputRight = keyState == Down}
      | isJumpKey key && keyState == Down -> st {inputJump = True}
      | otherwise -> st
    _ -> st

isLeftKey :: Key -> Bool
isLeftKey key =
  key == SpecialKey KeyLeft || key == Char 'a' || key == Char 'A'

isRightKey :: Key -> Bool
isRightKey key =
  key == SpecialKey KeyRight || key == Char 'd' || key == Char 'D'

isJumpKey :: Key -> Bool
isJumpKey key =
  key == SpecialKey KeySpace
    || key == SpecialKey KeyUp
    || key == Char 'w'
    || key == Char 'W'