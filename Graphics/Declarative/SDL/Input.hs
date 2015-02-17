module Graphics.Declarative.SDL.Input where

import Control.Applicative
import qualified Graphics.UI.SDL as SDL

import Graphics.Declarative.SDL.KeyboardInput

data Input = KeyInput KeyInput | MouseInput MouseInput | Tick deriving (Show, Eq)

data MouseInput = MouseMove (Double, Double) deriving (Show, Eq)

fromSDLEvent :: SDL.Event -> Maybe Input
fromSDLEvent (SDL.KeyboardEvent evType _ _ _ _ keysym) = KeyInput <$> fromSDLKeyEvent evType keysym
fromSDLEvent (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) = Just $ MouseInput $ MouseMove (fromIntegral x, fromIntegral y)
fromSDLEvent _ = Nothing
