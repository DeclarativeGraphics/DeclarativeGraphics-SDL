{-# LANGUAGE PatternSynonyms #-}
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

data MouseInput = MouseMove (Double, Double)
                | MousePress Int
                | MouseRelease Int
                deriving (Show, Eq)

fromSDLEvent :: SDL.Event -> Maybe Input
fromSDLEvent (SDL.KeyboardEvent evType _ _ _ _ keysym) = KeyInput <$> fromSDLKeyEvent evType keysym
fromSDLEvent (SDL.MouseMotionEvent _ _ _ _ _ x y _ _) = Just $ MouseInput $ MouseMove (fromIntegral x, fromIntegral y)
fromSDLEvent (SDL.MouseButtonEvent SDL.SDL_MOUSEBUTTONDOWN _ _ _ button _ _ _ _) = Just $ MouseInput $ MousePress $ fromIntegral button
fromSDLEvent (SDL.MouseButtonEvent SDL.SDL_MOUSEBUTTONUP _ _ _ button _ _ _ _) = Just $ MouseInput $ MouseRelease $ fromIntegral button
fromSDLEvent (SDL.WindowEvent _ _ _ 5 w h) = Just $ Resize (fromIntegral w, fromIntegral h)
fromSDLEvent _ = Nothing
