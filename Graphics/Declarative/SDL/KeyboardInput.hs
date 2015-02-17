module Graphics.Declarative.SDL.KeyboardInput where

import qualified Graphics.UI.SDL as SDL
import Data.Word
--import qualified Data.Set as Set
--import Data.Set (Set)
import Control.Applicative

import Graphics.Declarative.SDL.Keys

data KeyInput = KeyPress [Modifier] Key | KeyRelease [Modifier] Key

data Modifier = Shift | Ctrl

fromSDLKeyEvent :: Word32 -> SDL.Keysym -> Maybe KeyInput
fromSDLKeyEvent 768 sym = KeyPress [] <$> (fromKeycode $ SDL.keysymKeycode sym)
fromSDLKeyEvent 769 sym = KeyRelease [] <$> (fromKeycode $ SDL.keysymKeycode sym)
fromSDLKeyEvent _ _ = Nothing
