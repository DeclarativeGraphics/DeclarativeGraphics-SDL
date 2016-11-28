{-# LANGUAGE PatternSynonyms #-}
module Graphics.Declarative.SDL.Input
  ( module Graphics.Declarative.SDL.Keys,
    Input (..),
    MouseInput (..),
    MB (..),
    KeyInput (..),
    fromSDLEvent,
    fromSDLEventPayload,
    fromSDLKeyEvent,
    mouseInputPos
  ) where

import Graphics.Declarative.SDL.Keys

import Data.Lens
import Data.Word
import Control.Applicative ((<$>))
import qualified SDL
import Linear
import Linear.Affine
import Debug.Trace
import Data.Text

data Input
  = KeyInput KeyInput
  | MouseInput MouseInput
  | TextInput String
  | Resize (Int, Int)
  deriving (Show, Eq)

data MouseInput = MouseMove (Double, Double)
                | MousePress (Double, Double) MB
                | MouseRelease (Double, Double) MB
                deriving (Show, Eq, Read)

data MB = MBLeft | MBMiddle | MBRight | MBX1 | MBX2 | MBExtra Int deriving (Show, Eq, Read)

data KeyInput = KeyPress Key
              | KeyRelease Key
              deriving (Show, Eq)

type TimeInMs = Word32

waitEventTimeout :: TimeInMs -> IO (Maybe Input)
waitEventTimeout timeout =
  event <- SDL.waitEventTimeout (fromIntegral timeout)
  return (fmap fromSDLEvent event)

ticks :: IO TimeInMs
ticks = SDL.ticks

fromSDLEvent :: SDL.Event -> Maybe Input
fromSDLEvent = fromSDLEventPayload . SDL.eventPayload

fromSDLEventPayload :: SDL.EventPayload -> Maybe Input
fromSDLEventPayload (SDL.KeyboardEvent ev)    = KeyInput <$> fromSDLKeyEvent (SDL.keyboardEventKeyMotion ev) (SDL.keyboardEventKeysym ev)
fromSDLEventPayload (SDL.MouseMotionEvent ev) = Just $ MouseInput $ MouseMove (fromIntegral x, fromIntegral y) where (P (V2 x y)) = SDL.mouseMotionEventPos ev
fromSDLEventPayload (SDL.MouseButtonEvent ev) = Just $ MouseInput $ fromSDLMouseButtonEvent ev
fromSDLEventPayload (SDL.WindowResizedEvent ev) = Just $ Resize (fromIntegral w, fromIntegral h) where (V2 w h) = SDL.windowResizedEventSize ev
fromSDLEventPayload (SDL.TextInputEvent ev) = Just $ TextInput $ unpack $ SDL.textInputEventText ev
fromSDLEventPayload ev = traceShow ev Nothing

fromSDLKeyEvent :: SDL.InputMotion -> SDL.Keysym -> Maybe KeyInput
fromSDLKeyEvent typ sym = case typ of
    SDL.Pressed  -> KeyPress   <$> key
    SDL.Released -> KeyRelease <$> key
  where key = fromKeycode $ SDL.unwrapKeycode $ SDL.keysymKeycode sym

fromSDLMouseButtonEvent :: SDL.MouseButtonEventData -> MouseInput
fromSDLMouseButtonEvent ev = case SDL.mouseButtonEventMotion ev of
    SDL.Pressed  -> MousePress (fromIntegral x, fromIntegral y) $ translateMB $ SDL.mouseButtonEventButton ev
    SDL.Released -> MouseRelease (fromIntegral x, fromIntegral y) $ translateMB $ SDL.mouseButtonEventButton ev
  where (P (V2 x y)) = SDL.mouseButtonEventPos ev

translateMB :: SDL.MouseButton -> MB
translateMB SDL.ButtonLeft = MBLeft
translateMB SDL.ButtonMiddle = MBMiddle
translateMB SDL.ButtonRight = MBRight
translateMB SDL.ButtonX1 = MBX1
translateMB SDL.ButtonX2 = MBX2
translateMB (SDL.ButtonExtra i) = MBExtra i

mouseInputPos :: Lens MouseInput MouseInput (Double, Double) (Double, Double)
mouseInputPos = Lens
  { get = getPos
  , modify = modifyPos }
  where
    getPos (MouseMove pos) = pos
    getPos (MousePress pos _) = pos
    getPos (MouseRelease pos _) = pos
    modifyPos f (MouseMove pos) = MouseMove (f pos)
    modifyPos f (MousePress pos b) = MousePress (f pos) b
    modifyPos f (MouseRelease pos b) = MouseRelease (f pos) b
