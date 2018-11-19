{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Graphics.Declarative.SDL.Input
  ( module Graphics.Declarative.SDL.Keys,
    Input (..),
    MouseInput (..),
    MB (..),
    KeyInput (..),
    fromSDLEvent,
    fromSDLEventPayload,
    fromSDLKeyEvent,
    mouseInputPos,
    waitEventTimeout,
    ticks,
    TimeInMs
  ) where

import Graphics.Declarative.SDL.Keys
import Graphics.Declarative.Transforms

import Control.Lens
import Data.Word
import Control.Applicative ((<$>))
import qualified SDL
import Linear
import Linear.Affine (Point(P))
import Debug.Trace
import Data.Text

data Input
  = KeyInput KeyInput
  | MouseInput MouseInput
  | TextInput String
  | Resize (Int, Int)
  | QuitEvent
  deriving (Show, Eq)

data MouseInput = MouseMove (V2 Double)
                | MousePress (V2 Double) MB
                | MouseRelease (V2 Double) MB
                deriving (Show, Eq, Read)

data MB = MBLeft | MBMiddle | MBRight | MBX1 | MBX2 | MBExtra Int deriving (Show, Eq, Read)

data KeyInput = KeyPress Key
              | KeyRelease Key
              deriving (Show, Eq)

type TimeInMs = Word32

instance Transformable Input where
    transformBy matrix (MouseInput mi) = MouseInput (transformBy matrix mi)
    transformBy _ anythingElse = anythingElse

instance Transformable MouseInput where
    transformBy matrix = over mouseInputPos changePos
        where
            changePos (V2 x y) =
                let (V3 x' y' w) = matrix !* V3 x y 1
                 in V2 (x' / w) (y' / w)

waitEventTimeout :: TimeInMs -> IO (Maybe Input)
waitEventTimeout timeout = do
  event <- SDL.waitEventTimeout (fromIntegral timeout)
  return (fromSDLEvent =<< event)

ticks :: IO TimeInMs
ticks = SDL.ticks

fromSDLEvent :: SDL.Event -> Maybe Input
fromSDLEvent = fromSDLEventPayload . SDL.eventPayload

fromSDLEventPayload :: SDL.EventPayload -> Maybe Input
fromSDLEventPayload SDL.QuitEvent             = Just QuitEvent
fromSDLEventPayload (SDL.KeyboardEvent ev)    = KeyInput <$> fromSDLKeyEvent (SDL.keyboardEventKeyMotion ev) (SDL.keyboardEventKeysym ev)
fromSDLEventPayload (SDL.MouseMotionEvent ev) = Just $ MouseInput $ MouseMove $ V2 (fromIntegral x) (fromIntegral y) where (P (V2 x y)) = SDL.mouseMotionEventPos ev
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
    SDL.Pressed  -> MousePress (V2 (fromIntegral x) (fromIntegral y)) $ translateMB $ SDL.mouseButtonEventButton ev
    SDL.Released -> MouseRelease (V2 (fromIntegral x) (fromIntegral y)) $ translateMB $ SDL.mouseButtonEventButton ev
  where (P (V2 x y)) = SDL.mouseButtonEventPos ev

translateMB :: SDL.MouseButton -> MB
translateMB SDL.ButtonLeft = MBLeft
translateMB SDL.ButtonMiddle = MBMiddle
translateMB SDL.ButtonRight = MBRight
translateMB SDL.ButtonX1 = MBX1
translateMB SDL.ButtonX2 = MBX2
translateMB (SDL.ButtonExtra i) = MBExtra i

mouseInputPos :: Lens' MouseInput (V2 Double)
mouseInputPos f =  \case
  MouseMove pos -> MouseMove <$> f pos
  MousePress pos button -> (`MousePress` button) <$> f pos
  MouseRelease pos button -> (`MouseRelease` button) <$> f pos
