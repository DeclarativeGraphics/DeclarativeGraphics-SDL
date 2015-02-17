module Graphics.Declarative.SDL.Input where

import Control.Applicative
import qualified Graphics.UI.SDL as SDL

import Graphics.Declarative.SDL.KeyboardInput

data Input
  = KeyInput KeyInput
  | MouseInput MouseInput
  | Resize (Int, Int)
  | Tick
  deriving (Show, Eq)

data MouseInput = MouseMove (Double, Double) deriving (Show, Eq)

fromSDLEvent :: SDL.Event -> Maybe Input
fromSDLEvent (SDL.KeyboardEvent evType _ _ _ _ keysym) = KeyInput <$> fromSDLKeyEvent evType keysym
fromSDLEvent (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) = Just $ MouseInput $ MouseMove (fromIntegral x, fromIntegral y)
fromSDLEvent (SDL.WindowEvent _ _ _ 5 w h) = Just $ Resize (fromIntegral w, fromIntegral h)
fromSDLEvent _ = Nothing
